---
title: Natural Gas Consumption Forecasting Model
layout: post
post-image: /assets/images/nat_gas_files/nat_gas_poster.png
description: This is my midterm project for my predicative analytics course. The goal of this project was to forecast the consumption of natural gas in the United States, using machine-learning techniques.
tags:
- Machine-Learning
- Forecasting
- R
---

# Data Importation

<p>
For my predictive analytics course, I chose to model natural gas consumption in the US, from 2017 to 2021. I obtained the data from the US Energy Information Administration. I will also
incorporate national average temperature, a natural gas price index
(Henry-Hub), US natural gas exports, a technological progress index (%
of GDP spent on R&D), and an index of industrial production output. This
data is monthly and will be used as predictor variables for our
regression model.
</p>

##### Importing data 

    df.gas <- read.csv("Data/natural_gas.csv")
    df.henryhub <- read.csv("Data/natgas_price.csv")
    df.exports <- read.csv("Data/gas_exports.csv")
    df.temp <- read.csv("Data/temp_monthly.csv")
    df.RD <- read.csv("Data/GERD%ofGDP.csv")
    df.IND <- read.csv("Data/IndustrialProduction.csv")

##### Generating: Consumption History Time-Series

    myts.gas <- df.gas |>
      filter(Description == "Natural Gas Consumption",
             substr(YYYYMM, 5, 6) != 13,) |>
      mutate(Year = substr(YYYYMM, 1, 4),
             Month = substr(YYYYMM, 5, 6),
             YearMonth = yearmonth(paste(Year, Month)),
             Consumption = as.numeric(Value)) |>
      as_tsibble(index = YearMonth,
                 key = Consumption) |>
      select(YearMonth, Consumption)
    myts.gas$Consumption <- myts.gas$Consumption^1
    myts.gas <- as_tsibble(myts.gas, index = YearMonth)


##### Generating: Contemporary Consumption Data Frame

    `df.gas <- df.gas |> 
      filter(Description == "Natural Gas Consumption",
             substr(YYYYMM, 5, 6) != 13,
             between(substr(YYYYMM, 1, 4), "2017", "2021")) |>
      mutate(Year = substr(YYYYMM, 1, 4),
             Month = substr(YYYYMM, 5, 6),
             YearMonth = yearmonth(paste(Year, Month)),
             Consumption = as.numeric(Value)) |>
      select(YearMonth, Consumption)` 

##### Generating: Price Data Frame

    `df.price <- df.henryhub |>
      mutate(YearMonth = yearmonth(Date)) |>
      filter(between(substr(Date, 5, 8), "2017", "2021")) |>
      select(YearMonth, Price)`

##### Generating: US Exports Data Frame

    `df.exports.22 <- df.exports |>
      mutate(YearMonth = yearmonth(Date),
             Exports = U.S..Natural.Gas.Exports..MMcf.) |>
      filter(between(substr(Date, 5, 8), "1997", "2022"))
    df.exports <- df.exports.22 |>
      filter(between(substr(Date, 5, 8), "2017", "2021"))  |>
      select(YearMonth, Exports)`

##### Generating: US Temperature Data Frame

    `df.temp <- df.temp |>
      filter(between(substr(DATE, 1, 4), "2017", "2021")) |>
      mutate(YearMonth = yearmonth(DATE)) |>
      group_by(YearMonth) |> 
      aggregate(TAVG ~ YearMonth, mean) |>
      mutate(Temp = TAVG)`

##### Generating: R&D Spending (% of GDP) Data Frame

    `df.RD <- df.RD |> 
      filter(between(substr(TIME, 1, 4), "2017", "2021")) |>
      mutate(Year = substr(TIME, 1, 4),
             Month = substr(TIME, 5, 6),
             YearMonth = yearmonth(paste(Year, Month)),
             Tech = Value) |>
      select(c(YearMonth, Tech)) |>
      arrange(YearMonth)`

##### Generating: Industry Data Frame

    `df.IND <- df.IND |>
      filter(between(substr(TIME, 1, 4), "2017", "2021")) |>
      mutate(YearMonth = yearmonth(TIME),
             Industry = Value) |>
      select(c(YearMonth, Industry)) |>
      arrange(YearMonth)`

##### Generating: Combined Data Frame

    `df <- cbind(df.gas, df.price, df.exports, df.temp, df.RD, df.IND) |>
      select(-c(3,5,7,8,10,12))`

##### Generating: Final Time-Series

    `myts <- df |>
      as_tsibble(index = YearMonth, 
                 key = c(Consumption, Price, Exports, Temp, Tech, Industry)) |>
      arrange(YearMonth)`

# Time-Series Plots

<p>
Now that we have our data and time-series organized, let’s visualize it.
</p>

### Consumption History

<p>
In an attempt to understanding the greater trend of domestic US
consumption of natural gas, we will begin by plotting a complete
time-series of the data first This spans from 1973 to 2022. We will
seasonally-adjust the time-series as well, to better identify the trend
of the data.
</p>

##### Adding seasonally adjusted values

    `myts.gas.decomp <- myts.gas |>
      model(STL(Consumption)) |>
      components()
    myts.gas["Adjusted"] <- myts.gas.decomp$season_adjust`
      
##### Plotting the seasonally adjusted consumption time-series (1973 - Present)

    `ggplot(myts.gas, aes(x = YearMonth, y = Adjusted)) +  
      geom_line(aes(y = Adjusted)) +
      labs(title = "Natural Gas Consumption - Seasonaly Adjusted",
           subtitle = "in USA (1973 - July, 2023)",
           x = "Month",
           y = "Billion Cubic Feet")` 

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/TS_history.pdf", width = 12)

    ## Saving 12 x 5 in image

### Contemporary Consumption

<p>
Now we will plot the time-series that we will analyze: 2017 - 2022.
</p>

    # Plotting the consumption time-series (2017 - 2021)
    ggplot(myts, aes(x = YearMonth, y = Consumption)) + 
      geom_point(aes(y = Consumption)) + 
      geom_line(aes(y = Consumption)) +
      labs(title = "Time-Series of Natural Gas Consumption",
           subtitle = "in USA (2017-2021)",
           x = "Month",
           y = "Billion Cubic Feet") 

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/TS_contemp.pdf", width = 12)

    ## Saving 12 x 5 in image

### US Natural Gas Exports

<p>
Now we will visualize the complete time series of the US natural gas
exports market. This plot will contain quantity of exports and price
history, based on the widely used Henry Hub index.
</p>

    # Generating a data frame, combining price and export data
    df.market <- df.henryhub |>
      mutate(YearMonth = yearmonth(Date)) |>
      filter(between(substr(Date, 5, 8), "1997", "2022")) |>
      select(YearMonth, Price)
    df.market <- cbind(df.market, df.exports.22)
    df.market <- df.market[, !duplicated(colnames(df.market))]

    # Converting that data frame into a time-series
    myts.market <- df.market |>
      as_tsibble(index = YearMonth, key = c(Price, Exports)) |>
      mutate(Exports = Exports/100000)

    # Plotting the price and exports time-series
    ggplot(myts.market, aes(x = YearMonth)) + 
      geom_line(aes(y = Price), color = "Dark Green") +  
      scale_y_continuous(sec.axis = sec_axis(~., 
                                             name = "Exports (10 Billion Cubic Feet)")) +
      geom_line(aes(y = Exports), color = "Purple") +
      labs(title = "US Natural Gas - Export Market",
           subtitle = "Time Series of Exports (Qty) and Henry Hub Price Index (1997-2022)",
           x = "Month",
           y = "Price ($ per Million Btu") + 
      annotate(
        "text", label = "Price", 
        x = (yearmonth("2014-03")), y = 6.4, size = 5.5, colour = "Dark Green") +
      annotate(
        "text", label = "Exports", 
        x = (yearmonth("2005-02")), y = 1.5, size = 5.5, colour = "Purple")

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/Market.pdf", width = 12, height = 7)

# Model Preperation

### Box-Cox Transformation

<p>
Before we can build our models, we need to ensure the time-series is
ready. After testing multiple transformations, I found the Box-Cox to be
optimal.
</p>

    lambda <- BoxCox.lambda(myts$Consumption, method = c("guerrero"))
    myts$Consumption <- myts$Consumption^lambda
    myts <- as_tsibble(myts, index = YearMonth)

    # Plotting the time-series
    ggplot(myts, aes(x = YearMonth, y = Consumption)) + 
      geom_point(aes(y = Consumption)) + 
      geom_line(aes(y = Consumption)) +
      labs(title = "Time-Series of Natural Gas Consumption",
           subtitle = "in USA (2017-2021)",
           x = "Month",
           y = "Transformed Cubic Feet") 

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/TS_transformed.pdf", width = 12, height = 7)

    # Unit root test
    myts |> features(Consumption, unitroot_kpss)

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.163         0.1

<p>
As we can see above, our transformed time-series is stationary, as it
passes the KPSS test.
</p>

### Train & Test Sets

<p>
In order to assess the accuracy of our models, we will need to split our
data in to ‘train’ and ‘test’ data sets. The first four years of the
data (80%), 2017-2020, will be put in the train set, and the final year
(20%), 2021, will be put in the test data set.
</p>

    # Train
    train <- as_tsibble(myts[1:48,], 
                        index = YearMonth, 
                        key = c(Consumption, Price, Temp, Tech, Industry))
    train$Consumption <- train$Consumption*1
    train <- as_tsibble(train, index = YearMonth)

    # Test
    test <- as_tsibble(myts[49:60,], 
                       index = YearMonth, 
                       key = c(Consumption, Price, Temp, Tech, Industry))
    test$Consumption <- test$Consumption*1
    test <- as_tsibble(test, index = YearMonth)

# Model Generation

<p>
Now we will create our forecasting models using the training data set.
The first model will be a linear regression model, using the predictor
variables previously discussed, and trend and seasonal components. The
second model will be an optimized ETS. The third model will be an
optimized ARIMA. The final model will be an aggregation of all three
previous models.
</p>

    fit <- train |> model(
      LM = TSLM(Consumption ~ 
                  Price + Temp + Tech + Industry + Exports + trend() + season()),
      ETS = ETS(Consumption),
      ARIMA = ARIMA(Consumption)) |>
      mutate(ENSEMBLE = (LM + ETS + ARIMA)/3)
    fit

    ## # A mable: 1 x 4
    ##        LM          ETS                     ARIMA      ENSEMBLE
    ##   <model>      <model>                   <model>       <model>
    ## 1  <TSLM> <ETS(M,A,M)> <ARIMA(0,1,1)(0,1,0)[12]> <COMBINATION>

<p>
The ETS and ARIMA models were automatically optimized. The ETS model has
multiplicative error and season components, and an additive trend
component. The ARIMA is seasonally and non-seasonally differenced, and
has a first order moving average component \[MA(q=1)\].
</p>

# Residual Analysis

<p>
Before we can forecast with these models, we need to analyze their
residuals.
</p>

    # Residuals Plot
    augment(fit) |>
      autoplot(.resid) +
      labs(title = "Residuals for All Models",
           y = "Residuals")

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/Residuals.pdf", width = 12, height = 7)

    # ACF Plots
    augment(fit) |>
      ACF(.resid) |>
      autoplot() +
      labs(title = "ACF for All Models",
           y = "ACF")

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-17-2.png)

    # Exporting the plot
    ggsave(filename = "Graphics/ACF.pdf", width = 12, height = 7)

    # Portmanteau Test (Ljung-Box Test)
    augment(fit) |>
      features(.resid, ljung_box, lag = 24) 

    ## # A tibble: 4 × 3
    ##   .model   lb_stat lb_pvalue
    ##   <chr>      <dbl>     <dbl>
    ## 1 ARIMA       15.6  0.901   
    ## 2 ENSEMBLE    25.0  0.407   
    ## 3 ETS         25.0  0.407   
    ## 4 LM          57.6  0.000138

<p>
As we can see, the residuals are relatively normally distributed around
the mean of zero, meaning they are homoscedastic. The ACF plots for the
ETS and ARIMA models look good, as does the ENSEMBLE model. The ACF plot
for the regression model is slightly concerning. Furthermore, while the
ETS and ARIMA models pass the Ljung-Box test, the regression model does
not. While normally, these two facts would disqualify this model, we
will proceed with this model for the sake of this exercise. As we will
see when we forecast with these models, it performs well despite these
concerns.
</p>

# Forecast

### ETS Forecast

    # Generating forecast plot
    fit |> select(c(ETS)) |> forecast(h = 12) |> 
      autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "ETS Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption")  

    ## Plot variable not specified, automatically selected `.vars = Consumption`

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-18-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/ETS_fit.pdf", width = 12, height = 7)

### ARIMA Forecast

    # Generating forecast plot
    fit |> select(c(ARIMA)) |> forecast(h = 12) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "ARIMA Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

    ## Plot variable not specified, automatically selected `.vars = Consumption`

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/ARIMA_fit.pdf", width = 12, height = 7)

### Regression Forecast

    # Generating forecast plot
    fit |> select(c(LM)) |> forecast(new_data = test) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "Regression Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

    ## Plot variable not specified, automatically selected `.vars = Consumption`

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-20-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/LM_fit.pdf", width = 12, height = 7)

### Aggregate Forecast

    # Generating forecast plot
    fit |> select(c(ENSEMBLE)) |> forecast(new_data = test) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "Aggregate Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

    ## Plot variable not specified, automatically selected `.vars = Consumption`

![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-21-1.png)

    # Exporting the plot
    ggsave(filename = "Graphics/ENSEMBLE_fit.pdf", width = 12, height = 7)

# Accuracy

<p>
To assess the accuracy of our forecast models more specifically, we need
to compare the generated forecasts against the test data with the
following metrics:
</p>

### Point Forecast Accuracy

    # Generating the forecast table (fable)
    myf <- fit |> select(-c(LM, ENSEMBLE)) |> forecast(h = 12)
    myf.lm <- fit |> select(c(LM)) |> forecast(new_data = test)
    myf.agg <- fit |> select(c(ENSEMBLE)) |> forecast(new_data = test)
    myf <- rbind(myf, myf.lm, myf.agg)

    ## Warning: `rbind.fbl_ts()` was deprecated in fabletools 0.2.0.
    ## ℹ Please use `bind_rows()` instead.
    ## ℹ The deprecated feature was likely used in the base package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    # Generating a point forecast accuracy table
    accuracy(myf, test) |>
      mutate(Model = .model) |>
      select(c(Model, MAE, RMSE, MAPE)) |>
      arrange(MAPE)

    ## # A tibble: 4 × 4
    ##   Model          MAE      RMSE  MAPE
    ##   <chr>        <dbl>     <dbl> <dbl>
    ## 1 ENSEMBLE 0.0000118 0.0000145  3.08
    ## 2 ETS      0.0000141 0.0000169  3.63
    ## 3 ARIMA    0.0000144 0.0000195  3.68
    ## 4 LM       0.0000387 0.0000412 10.2

<p>
All of our models perform well, with the regression model performing
slightly worse. The ENSEMBLE model is slightly better than the ETS and
ARIMA models.
</p>

### Prediction Interval Accuracy

    myf |> accuracy(test, list(crps =CRPS)) |>
      mutate(Model = .model,
             CRPS = crps) |>
      select(c(Model, CRPS)) |>
      arrange(CRPS)

    ## # A tibble: 4 × 2
    ##   Model          CRPS
    ##   <chr>         <dbl>
    ## 1 ENSEMBLE 0.00000853
    ## 2 ETS      0.0000101 
    ## 3 ARIMA    0.0000111 
    ## 4 LM       0.0000290

<p>
Again ENSEMBLE model outperforms the others, but all models perform
relatively well.
</p>
