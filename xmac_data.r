# -----------------------------------------------------------------------------
# Script for Analyzing Price Data with Moving Averages
#
# This script reads a CSV file containing price data for multiple symbols, filters
# the data based on specified start and end dates, and converts it to an xts object.
# It then prints the date range (first and last valid dates and total days) for each symbol.
#
# Next, it computes unconditional annualized returns and volatilities for each symbol
# using daily log returns, and prints the results.
#
# Finally, the script performs a moving average conditional returns analysis for various
# moving average lengths, calculating annualized return, volatility, and the fraction of
# days with prices above and below the moving average (using the previous day's data).
#
# Dependencies: quantmod, TTR, xts
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(quantmod))
suppressPackageStartupMessages(library(TTR))
suppressPackageStartupMessages(library(xts))

trading_days <- 252
ret.scale <- 100.0
ma_lengths <- c(50, 100, 150, 200, 250)
start_date <- "1900-01-01" 
end_date <- "2100-01-01"
prices_file = "prices.csv"

prices_df <- read.csv(prices_file, stringsAsFactors = FALSE)
prices_df$Date <- as.Date(prices_df$Date)

# Filter rows based on the specified date range
prices_df <- subset(prices_df, Date >= as.Date(start_date) & Date <= as.Date(end_date))

# Convert the dataframe to an xts object (excluding the Date column)
prices_xts <- xts(prices_df[,-1], order.by = prices_df$Date)

# ---- Print Date Ranges for Each Symbol ----

# Initialize list to store coverage results
coverage_list <- list()

# Loop over each symbol to determine the first and last dates of valid (non-NA) data, and count the days
symbols <- colnames(prices_xts)
for(sym in symbols) {
  series <- prices_xts[, sym]
  valid <- series[!is.na(series)]
  if (length(valid) > 0) {
    first_date <- index(valid)[1]
    last_date <- index(valid)[length(valid)]
    n_days <- length(valid)
  } else {
    first_date <- NA
    last_date <- NA
    n_days <- 0
  }
  coverage_list[[length(coverage_list) + 1]] <- data.frame(
    Symbol = sym,
    First_Date = first_date,
    Last_Date = last_date,
    Days = n_days,
    stringsAsFactors = FALSE
  )
}

# Combine coverage results into a dataframe and print
coverage_df <- do.call(rbind, coverage_list)
cat("prices_file:", prices_file, "\n\n")
print(coverage_df, row.names = FALSE)
cat("\n")

# ---- Unconditional Return and Volatility Analysis ----

# Initialize list to store unconditional analysis results
uncond_list <- list()

for(sym in symbols) {
  price_series <- prices_xts[, sym]
  
  # Compute daily log returns (suppress warnings for missing values)
  ret <- suppressWarnings(ret.scale * dailyReturn(price_series, type = "log"))
  
  # Remove NA values
  valid_ret <- ret[!is.na(ret)]
  if(nrow(valid_ret) == 0) next
  
  # Compute annualized return and volatility
  ann_ret <- mean(valid_ret, na.rm = TRUE) * trading_days
  ann_vol <- sd(valid_ret, na.rm = TRUE) * sqrt(trading_days)
  
  uncond_list[[length(uncond_list) + 1]] <- data.frame(
    Symbol = sym,
    Ann_Ret = ann_ret,
    Ann_Vol = ann_vol,
    stringsAsFactors = FALSE
  )
}

# Combine unconditional results into a dataframe and print
uncond_df <- do.call(rbind, uncond_list)
cat("Unconditional returns and volatility\n")
print(format(uncond_df, nsmall = 2, digits = 2), row.names = FALSE)
cat("\n")

# ---- Moving Average Conditional Returns Analysis ----

results_list <- list()

# Loop over each symbol (each column in prices_xts)
for(sym in symbols) {
  # Extract the price series for the symbol
  price_series <- prices_xts[, sym]
  
  # Compute daily log returns (suppress warnings from missing values)
  ret <- suppressWarnings(ret.scale * dailyReturn(price_series, type = "log"))
  
  # If there are no returns (e.g. all data is NA), skip this symbol
  if(nrow(ret) == 0) next
  
  # Loop over each moving average length
  for(ma in ma_lengths) {
    # Compute the Simple Moving Average (SMA), suppressing warnings for missing values removed
    sma <- suppressWarnings(SMA(price_series, n = ma))
    
    # Determine the condition using the previous day's price relative to its SMA.
    # For day t, use lagged price and lagged SMA (day t-1)
    condition <- ifelse(lag(price_series, k = 1) > lag(sma, k = 1), "Above", "Below")
    
    # Align the condition to the same dates as the returns
    condition <- condition[index(ret)]
    
    # Remove observations with NA values in either condition or returns
    valid <- !is.na(condition) & !is.na(ret)
    if(sum(valid) == 0) next  # Skip if no valid data is available
    
    condition_valid <- condition[valid]
    ret_valid <- ret[valid]
    
    # Separate returns based on the condition
    above_ret <- ret_valid[condition_valid == "Above"]
    below_ret <- ret_valid[condition_valid == "Below"]
    
    # Calculate the fraction of days in each condition
    frac_above <- length(above_ret) / length(ret_valid)
    frac_below <- length(below_ret) / length(ret_valid)
    
    # Calculate annualized return and volatility for days above the SMA
    annual_ret_above <- mean(above_ret, na.rm = TRUE) * trading_days
    annual_vol_above <- sd(above_ret, na.rm = TRUE) * sqrt(trading_days)
    
    # Calculate annualized return and volatility for days below the SMA
    annual_ret_below <- mean(below_ret, na.rm = TRUE) * trading_days
    annual_vol_below <- sd(below_ret, na.rm = TRUE) * sqrt(trading_days)
    
    # Save the results in a dataframe row
    results_list[[length(results_list) + 1]] <- data.frame(
      Symbol = sym,
      MA_Length = ma,
      Ann_Ret_Above = annual_ret_above,
      Ann_Vol_Above = annual_vol_above,
      Fraction_Above = frac_above,
      Ann_Ret_Below = annual_ret_below,
      Ann_Vol_Below = annual_vol_below,
      Fraction_Below = frac_below,
      stringsAsFactors = FALSE
    )
  }
}

# Combine all results into one dataframe
results_df <- do.call(rbind, results_list)

# Set a wide print option so that all columns appear on one line
options(width = 10000)
cat("Returns and volatility conditional on the price being above or below the moving average\n")
print(format(results_df, nsmall = 2, digits = 2), row.names = FALSE)
