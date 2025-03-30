# Moving Average Crossover
Test moving average crossover trading systems for various moving average lengths for several assets in R and Fortran. The 
R output is below. One can see that for several U.S. exchange-traded funds, returns have been higher and volatility lower 
when yesterday's price closed above the moving average, compared to when it closed below.

```
prices_file: prices.csv 

 Symbol First_Date  Last_Date Days
    SPY 1993-01-29 2025-03-28 8097
    EFA 2001-08-27 2025-03-28 5932
    EEM 2003-04-14 2025-03-28 5526
    EMB 2007-12-19 2025-03-28 4346
    HYG 2007-04-11 2025-03-28 4522
    LQD 2002-07-30 2025-03-28 5704

Unconditional returns and volatility
 Symbol Ann_Ret Ann_Vol
    SPY    9.72   18.61
    EFA    5.43   20.97
    EEM    8.10   27.26
    EMB    4.17   11.05
    HYG    4.67   11.15
    LQD    4.52    8.44

Returns and volatility conditional on the price being above or below the moving average
 Symbol MA_Length Ann_Ret_Above Ann_Vol_Above Fraction_Above Ann_Ret_Below Ann_Vol_Below Fraction_Below
    SPY     50.00          6.30         13.33           0.69         17.13         26.80           0.31
    SPY    100.00          8.43         13.52           0.73         13.38         28.35           0.27
    SPY    150.00          9.10         13.72           0.76         11.50         29.23           0.24
    SPY    200.00         10.33         13.92           0.77          7.78         29.65           0.23
    SPY    250.00         10.27         14.20           0.78          7.79         29.95           0.22
    EFA     50.00          6.85         15.44           0.63          3.95         28.00           0.37
    EFA    100.00          8.13         14.78           0.65          2.05         29.08           0.35
    EFA    150.00          8.41         14.80           0.67          0.91         29.70           0.33
    EFA    200.00          7.93         14.79           0.69          1.66         30.87           0.31
    EFA    250.00          6.53         14.73           0.69          6.28         30.53           0.31
    EEM     50.00          5.97         20.63           0.59          9.60         34.86           0.41
    EEM    100.00         10.34         20.46           0.61          1.14         35.53           0.39
    EEM    150.00          9.43         20.62           0.62          1.44         36.08           0.38
    EEM    200.00          4.37         20.56           0.64          8.65         36.97           0.36
    EEM    250.00          4.93         20.67           0.66          7.16         37.63           0.34
    EMB     50.00          8.23          8.47           0.64         -2.91         14.60           0.36
    EMB    100.00          6.05          8.48           0.66          0.38         15.13           0.34
    EMB    150.00          4.86          8.06           0.69          3.14         16.13           0.31
    EMB    200.00          5.23          7.98           0.71          3.60         16.39           0.29
    EMB    250.00          5.12          7.86           0.74          6.36         14.53           0.26
    HYG     50.00          5.92          7.64           0.69          2.19         16.52           0.31
    HYG    100.00          5.45          7.47           0.74          3.20         17.95           0.26
    HYG    150.00          6.01          6.86           0.75          0.95         19.07           0.25
    HYG    200.00          5.57          6.72           0.76          2.82         19.70           0.24
    HYG    250.00          5.54          6.69           0.78          2.70         20.60           0.22
    LQD     50.00          6.30          6.38           0.66          0.62         11.46           0.34
    LQD    100.00          5.31          6.28           0.68          1.95         11.89           0.32
    LQD    150.00          4.74          6.39           0.70          2.61         12.03           0.30
    LQD    200.00          4.66          6.40           0.72          2.07         12.42           0.28
    LQD    250.00          4.15          6.49           0.74          3.87         12.52           0.26
```
