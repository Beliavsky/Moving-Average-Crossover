! -----------------------------------------------------------------------------
! Program: price_analysis
!
! Description:
!   This program reads a CSV file (prices.csv) containing price data for several
!   symbols, filters the data within a specified date range, and then performs three 
!   analyses:
!
!     1. It prints, for each symbol, the first and last dates of valid (non-missing)
!        data along with the total number of valid days.
!
!     2. It computes the unconditional annualized return and volatility for each
!        symbol based on daily log returns.
!
!     3. For specified moving average (MA) lengths, it computes
!        the conditional annualized return and volatility for days when the price
!        (using the previous day's data) is above or below its SMA, along with the
!        fraction of days in each condition.
!
!   Missing prices are represented by "NA" in the CSV file and are converted to -9999.0.
!
! -----------------------------------------------------------------------------

program price_analysis
  implicit none

  ! Parameters and constants
  integer, parameter :: maxRows = 10000, maxSymbols = 10, numMA = 2
  character(len=20), parameter :: start_date = "1900-01-01", &
                                    end_date = "2100-01-01"
  integer, parameter :: trading_days = 252
  real, parameter :: ret_scale = 100.0, bad_num = -9999.0
  integer, parameter :: ma_lengths(numMA) = [100, 200]

  ! Declarations for file reading and data storage
  character(len=100) :: line
  character(len=20), allocatable :: dates(:), headerSymbols(:), fdates(:)
  real, allocatable :: prices(:,:)
  integer :: nRows, nSymbols, i, j, ios, ncount, pos, pos2, ma_len, nRet

  ! Declarations for analysis
  real :: ret, sumRet, sumSq, meanRet, stdRet, annual_ret, annual_vol
  real, allocatable :: sma(:), fprices(:,:)
  integer :: nAbove, nBelow, k, idx, validCount, pos_loop, &
     firstIdx, lastIdx, validDays
  real :: sumRetAbove, sumSqAbove, sumRetBelow, sumSqBelow
  real :: aboveAnnRet, aboveAnnVol, aboveFraction
  real :: belowAnnRet, belowAnnVol, belowFraction
  logical :: validWindow

  ! ---------------------------------------------------------------------------
  ! First pass: count the number of data rows (excluding header)
  nRows = 0
  open(unit=10, file="prices.csv", status="old", action="read")
  read(10, *)  ! Read header line
  do
     read(10, '(A)', iostat=ios) line
     if (ios /= 0) exit
     nRows = nRows + 1
  end do
  close(10)

  ! Allocate arrays for all data rows
  allocate(dates(nRows), prices(nRows, maxSymbols))

  ! ---------------------------------------------------------------------------
  ! Read header to get symbols (first field is Date; remaining are symbol names)
  open(unit=10, file="prices.csv", status="old", action="read")
  read(10, '(A)') line
  call parse_header(line, headerSymbols, nSymbols)
  if (nSymbols > maxSymbols) then
     print *, "Error: number of symbols exceeds maxSymbols"
     stop
  end if

  ! Reallocate prices array to (nRows, nSymbols)
  deallocate(prices)
  allocate(prices(nRows, nSymbols))

  ! Read each data line into dates and prices arrays
  ncount = 0
  do i = 1, nRows
     read(10, '(A)') line
     ncount = ncount + 1
     call parse_line(line, dates(ncount), prices(ncount,1:nSymbols))
  end do
  close(10)

  ! ---------------------------------------------------------------------------
  ! Filter rows based on the specified date range (string comparison works for YYYY-MM-DD)
  validCount = 0
  do i = 1, nRows
     if (trim(dates(i)) >= trim(start_date) .and. trim(dates(i)) <= trim(end_date)) then
        validCount = validCount + 1
     end if
  end do

  if (validCount == 0) then
     print *, "No data in the specified date range."
     stop
  end if

  allocate(fdates(validCount))
  allocate(fprices(validCount, nSymbols))
  idx = 0
  do i = 1, nRows
     if (trim(dates(i)) >= trim(start_date) .and. trim(dates(i)) <= trim(end_date)) then
        idx = idx + 1
        fdates(idx) = dates(i)
        fprices(idx, :) = prices(i, :)
     end if
  end do

  ! ---------------------------------------------------------------------------
  ! Print Date Ranges for Each Symbol
  print *
  print *, "prices_file: prices.csv"
  print *
  print '(A6,2X,A10,2X,A10,2X,A4)', "Symbol", "First_Date", "Last_Date", "Days"
  do j = 1, nSymbols
    firstIdx = 0; lastIdx = 0; validDays = 0
    do i = 1, validCount
       if (fprices(i, j) /= bad_num) then
          validDays = validDays + 1
          if (firstIdx == 0) firstIdx = i
          lastIdx = i
       end if
    end do
    if (firstIdx > 0) then
       write(*, '(A6,2X,A10,2X,A10,2X,I4)') trim(headerSymbols(j)), trim(fdates(firstIdx)), trim(fdates(lastIdx)), validDays
    else
       write(*, '(A6,2X,A)') trim(headerSymbols(j)), ": No valid data"
    end if
  end do

  print *
  ! ---------------------------------------------------------------------------
  ! Unconditional Return and Volatility Analysis
  print *, "Unconditional returns and volatility"
  print '(A6,2X,A7,2X,A7)', "Symbol", "Ann_Ret", "Ann_Vol"
  do j = 1, nSymbols
    sumRet = 0.0
    sumSq  = 0.0
    nRet   = 0
    do i = 2, validCount
       if (fprices(i-1, j) /= bad_num .and. fprices(i, j) /= bad_num) then
          ret = log(fprices(i, j) / fprices(i-1, j)) * ret_scale
          sumRet = sumRet + ret
          sumSq  = sumSq + ret*ret
          nRet = nRet + 1
       end if
    end do
    if (nRet > 1) then
       meanRet = sumRet / nRet
       stdRet  = sqrt((sumSq - nRet*meanRet*meanRet) / (nRet-1))
       annual_ret = meanRet * trading_days
       annual_vol = stdRet * sqrt(real(trading_days))
       write(*, '(A6,2X,F7.2,2X,F7.2)') trim(headerSymbols(j)), annual_ret, annual_vol
    else
       write(*, '(A6,2X,A)') trim(headerSymbols(j)), ": No sufficient data"
    end if
  end do

  print *
  ! ---------------------------------------------------------------------------
  ! Moving Average Conditional Returns Analysis
  print *, "Returns and volatility conditional on the price being above or below the moving average"
  print '(A6,2X,A9,2X,A14,2X,A14,2X,A14,2X,A14,2X,A14,2X,A14)', "Symbol", "MA_Length", "Ann_Ret_Above", &
       "Ann_Vol_Above", "Fraction_Above", "Ann_Ret_Below", "Ann_Vol_Below", "Fraction_Below"
  do j = 1, nSymbols
    do k = 1, numMA
      ma_len = ma_lengths(k)
      allocate(sma(validCount))
      ! Compute SMA for symbol j over window ma_len (set to bad_num if any missing value)
      do i = 1, validCount
         if (i >= ma_len) then
            sma(i) = 0.0
            validWindow = .true.
            do pos_loop = i-ma_len+1, i
              if (fprices(pos_loop, j) == bad_num) then
                 validWindow = .false.
                 exit
              else
                 sma(i) = sma(i) + fprices(pos_loop, j)
              end if
            end do
            if (validWindow) then
               sma(i) = sma(i) / ma_len
            else
               sma(i) = bad_num
            end if
         else
            sma(i) = bad_num
         end if
      end do

      ! Compute conditional returns (using previous day's data)
      sumRetAbove = 0.0; sumSqAbove = 0.0; nAbove = 0
      sumRetBelow = 0.0; sumSqBelow = 0.0; nBelow = 0
      nRet = 0
      do i = 3, validCount
         if ( fprices(i-1, j) /= bad_num .and. fprices(i, j) /= bad_num .and. &
              sma(i-1) /= bad_num ) then
            ret = log(fprices(i, j) / fprices(i-1, j)) * ret_scale
            nRet = nRet + 1
            if (fprices(i-1, j) > sma(i-1)) then
               sumRetAbove = sumRetAbove + ret
               sumSqAbove = sumSqAbove + ret*ret
               nAbove = nAbove + 1
            else
               sumRetBelow = sumRetBelow + ret
               sumSqBelow = sumSqBelow + ret*ret
               nBelow = nBelow + 1
            end if
         end if
      end do
      if (nAbove > 1) then
         meanRet = sumRetAbove / nAbove
         stdRet = sqrt((sumSqAbove - nAbove*meanRet*meanRet) / (nAbove - 1))
         annual_ret = meanRet * trading_days
         annual_vol = stdRet * sqrt(real(trading_days))
         aboveAnnRet = annual_ret
         aboveAnnVol = annual_vol
         aboveFraction = real(nAbove) / real(nRet)
      else
         aboveAnnRet = bad_num
         aboveAnnVol = bad_num
         aboveFraction = 0.0
      end if
      if (nBelow > 1) then
         meanRet = sumRetBelow / nBelow
         stdRet = sqrt((sumSqBelow - nBelow*meanRet*meanRet) / (nBelow - 1))
         annual_ret = meanRet * trading_days
         annual_vol = stdRet * sqrt(real(trading_days))
         belowAnnRet = annual_ret
         belowAnnVol = annual_vol
         belowFraction = real(nBelow) / real(nRet)
      else
         belowAnnRet = bad_num
         belowAnnVol = bad_num
         belowFraction = 0.0
      end if

      write(*, '(A6,2X,i9,2X,F14.2,2X,F14.2,2X,F14.2,2X,F14.2,2X,F14.2,2X,F14.2)') &
           trim(headerSymbols(j)), ma_len, aboveAnnRet, aboveAnnVol, aboveFraction, &
           belowAnnRet, belowAnnVol, belowFraction

      deallocate(sma)
    end do
  end do

contains

  subroutine parse_header(line, symbols, nSymbols)
    implicit none
    character(len=*), intent(in) :: line
    character(len=20), allocatable, intent(out) :: symbols(:)
    integer, intent(out) :: nSymbols
    character(len=200) :: tmp
    integer :: i, ncount, pos, pos2

    tmp = trim(line)
    ncount = 0
    do i = 1, len_trim(tmp)
      if (tmp(i:i) == ",") ncount = ncount + 1
    end do
    ! First field is Date, so nSymbols = ncount
    nSymbols = ncount
    allocate(symbols(nSymbols))
    pos = index(tmp, ",") + 1
    do i = 1, nSymbols
       pos2 = index(tmp(pos:), ",")
       if (pos2 == 0) then
          symbols(i) = adjustl(tmp(pos:))
       else
          symbols(i) = adjustl(tmp(pos:pos+pos2-2))
          pos = pos + pos2
       end if
    end do
  end subroutine parse_header

  subroutine parse_line(line, dateStr, priceArray)
    implicit none
    character(len=*), intent(in) :: line
    character(len=20), intent(out) :: dateStr
    real, intent(out), dimension(:) :: priceArray
    character(len=200) :: tmp
    character(len=20) :: token
    integer :: pos, pos2, fieldIndex

    tmp = trim(line)
    pos = 1
    pos2 = index(tmp, ",")
    if (pos2 == 0) then
       dateStr = trim(tmp)
    else
       dateStr = trim(tmp(1:pos2-1))
       pos = pos2 + 1
    end if
    do fieldIndex = 1, size(priceArray)
       pos2 = index(tmp(pos:), ",")
       if (pos2 == 0) then
          token = trim(tmp(pos:))
       else
          token = trim(tmp(pos:pos+pos2-2))
          pos = pos + pos2
       end if
       if (token == "NA") then
          priceArray(fieldIndex) = bad_num
       else
          read(token, *) priceArray(fieldIndex)
       end if
    end do
  end subroutine parse_line

end program price_analysis
