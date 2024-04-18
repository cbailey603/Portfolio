---
title: "Natural Gas Consumption Forecasting Model"
layout: post
post-image: /assets/images/nat_gas_files/nat_gas_poster.png
output: pdf_document
tags:
- "Machine-Learning"
- Forecasting
- R
description: "This is my midterm project for my predicative analytics course. The
  goal of this project was to forecast the consumption of natural gas in the United
  States, using machine-learning techniques."
---

# Abstract

<p>
Natural gas is an increasingly prominent energy source in the United States. The war in Ukraine and subsequent sanctions on Russia have aﬀected the international natural gas market, increasing prices. Technical progress indexes and industrial production capabilities have been demonstrated to negatively aﬀect levels of natural gas consumption (Du, B., Guo, X., Wang, A., & Duan, H., 2023). To investigate the eﬀect that these elements, international and domestic, have on natural gas consumption in the US, several forecasting models were generated in this report. A regression model, using the previously mentioned factors as predictor variables, an ETS, ARIMA, and an aggregate model were generated and assessed for their forecasting accuracy.  The results of this report are as follows: (a) natural gas consumption in the US has been steadily increasing throughout the 21st century and looks to continue in the short term. (b) US natural gas consumption is largely tied to electricity use and is highly seasonal, with spikes in the winter and peak of summer. (c) Technical progress negatively aﬀects the consumption of natural gas as more advanced and sustainable energy sources become implementable. (d) International market forces have little to no eﬀect on US domestic consumption within the timeframe analyzed in this report. Therefore, this report suggests that natural gas will likely act as a stepping stone energy source between coal and oil and renewable sources like solar and wind. As technical progress continues to improve renewable energies and make them cost-eﬀective and scalable, natural gas consumption should decrease.   
</p>

# Introduction & Significance

<p>
Combating climate change and ensuring the long-term health of the planet is a monumental undertaking. Opinions are strongly held and tend to diﬀer on all aspects of this topic, from its existence to minute policy details. US policy, administered through the EPA, has been to invest in and incentivize the natural gas industry as a stepping stone toward more sustainable energy solutions. Natural gas CO2 emission levels are estimated to be half that of coal, among other advantages. (EIA, 2022) However, it is also scrutinized by those preferring to see investments go towards cleaner, renewable sources, like wind and solar. While these criticisms are valid, existing research indicates that as technical progress advances, making renewable energies implementable, natural gas consumption should decrease (Du, B., Guo, X., Wang, A., & Duan, H., 2023). Regardless, it remains a prominent energy source for wealthy nations like the US because, while the necessary technology is advanced, the industry is robust and capable of producing large amounts of energy.  In the US, in particular, there is an abundance of natural gas reservoirs, making it a proﬁtable export. The war in Ukraine and subsequent sanctions on Russia have dramatically increased the demand and willingness to pay for US natural gas in the EU, resulting in a price increase. Furthermore, regardless of the greater context of climate and international politics, energy forecasting is logistically important, as they are needed to allocate resources accurately. (Ravnik, J., & Hriberšek, M., 2019) All this considered, understanding US natural gas consumption past and future trends is important to the climate, the energy sector, and international politics.
</p>

## Data: Sources

<p>
This list highlights the monthly data used and where it was obtained:
</p>

* Consumption - EIA
* Average US Temperature - NOAA
* Price (Henry-Hub Index) - EIA
* Exports (Qty) - OECD
* Percent of GDP spent on R&D - OECD
* Industrial Production - OECD


## Data: Importation 

    df.gas <- read.csv("Data/natural_gas.csv")
    df.henryhub <- read.csv("Data/natgas_price.csv")
    df.exports <- read.csv("Data/gas_exports.csv")
    df.temp <- read.csv("Data/temp_monthly.csv")
    df.RD <- read.csv("Data/GERD%ofGDP.csv")
    df.IND <- read.csv("Data/IndustrialProduction.csv")

## Data: Cleaning and Formating 

    # Generating: Consumption History Time-Series
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


    # Generating: Contemporary Consumption Data Frame
    df.gas <- df.gas |> 
      filter(Description == "Natural Gas Consumption",
             substr(YYYYMM, 5, 6) != 13,
             between(substr(YYYYMM, 1, 4), "2017", "2021")) |>
      mutate(Year = substr(YYYYMM, 1, 4),
             Month = substr(YYYYMM, 5, 6),
             YearMonth = yearmonth(paste(Year, Month)),
             Consumption = as.numeric(Value)) |>
      select(YearMonth, Consumption) 


    # Generating: Price Data Frame
    df.price <- df.henryhub |>
      mutate(YearMonth = yearmonth(Date)) |>
      filter(between(substr(Date, 5, 8), "2017", "2021")) |>
      select(YearMonth, Price)


    # Generating: US Exports Data Frame
    df.exports.22 <- df.exports |>
      mutate(YearMonth = yearmonth(Date),
             Exports = U.S..Natural.Gas.Exports..MMcf.) |>
      filter(between(substr(Date, 5, 8), "1997", "2022"))
    df.exports <- df.exports.22 |>
      filter(between(substr(Date, 5, 8), "2017", "2021"))  |>
      select(YearMonth, Exports)


    # Generating: US Temperature Data Frame
    df.temp <- df.temp |>
      filter(between(substr(DATE, 1, 4), "2017", "2021")) |>
      mutate(YearMonth = yearmonth(DATE)) |>
      group_by(YearMonth) |> 
      aggregate(TAVG ~ YearMonth, mean) |>
      mutate(Temp = TAVG)


    # Generating: R&D Spending (% of GDP) Data Frame
    df.RD <- df.RD |> 
      filter(between(substr(TIME, 1, 4), "2017", "2021")) |>
      mutate(Year = substr(TIME, 1, 4),
             Month = substr(TIME, 5, 6),
             YearMonth = yearmonth(paste(Year, Month)),
             Tech = Value) |>
      select(c(YearMonth, Tech)) |>
      arrange(YearMonth)


    # Generating: Industry Data Frame
    df.IND <- df.IND |>
      filter(between(substr(TIME, 1, 4), "2017", "2021")) |>
      mutate(YearMonth = yearmonth(TIME),
             Industry = Value) |>
      select(c(YearMonth, Industry)) |>
      arrange(YearMonth)


    # Generating: Combined Data Frame
    df <- cbind(df.gas, df.price, df.exports, df.temp, df.RD, df.IND) |>
      select(-c(3,5,7,8,10,12))


    # Generating: Final Time-Series
    myts <- df |>
      as_tsibble(index = YearMonth, 
                 key = c(Consumption, Price, Exports, Temp, Tech, Industry)) |>
      arrange(YearMonth)

# Visualization

<p>
Now that we have our data and time-series organized, let’s visualize it. This step is integral to understanding the trend and form of the data. 
</p>

### Consumption History

<p>
In an attempt to understanding the greater trend of domestic US consumption of natural gas, we will begin by plotting a complete time-series of the data first This spans from 1973 to 2022. We will seasonally-adjust the time-series as well, to better identify the trend
of the data.
</p>

    # Adding seasonally adjusted values
    myts.gas.decomp <- myts.gas |>
      model(STL(Consumption)) |>
      components()
    myts.gas["Adjusted"] <- myts.gas.decomp$season_adjust
      
    # Plotting the seasonally adjusted consumption time-series (1973 - Present)
    ggplot(myts.gas, aes(x = YearMonth, y = Adjusted)) +  
      geom_line(aes(y = Adjusted)) +
      labs(title = "Natural Gas Consumption - Seasonaly Adjusted",
           subtitle = "in USA (1973 - July, 2023)",
           x = "Month",
           y = "Billion Cubic Feet")

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-11-1.png)
<p>
</p>

<p>
As we can see in the plot above, U.S. natural gas consumption has been on the rise since the late 1980s. There was a plateau in the 2000s, however, the increase continued again in the 2010s up to the present. This recent increase is partially attributable to combating climate change. As was previously mentioned, natural gas is a great energy source to reduce carbon emissions, as it emits far less than other fossil fuels while using similar technology and logistics. For this reason, it is an excellent option for regulators to subsidies, as it modernizes the energy market without majorly disrupting the existing industry. 
</p>

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

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-12-1.png)
<p>
</p>

<p>
When we zoom in and remove the seasonal-adjustment, the seasonality of the time series becomes evident. As was mentioned, natural gas consumption is largely a product of electricity usage, which is increased during winter months from heating. There is also a slight increase in electricity usage in the summer months for air conditioning. 
</p>


### US Natural Gas Exports

<p>
Now we will visualize the complete time series of the US natural gas exports market. This plot will contain quantity of exports and price history, based on the widely used Henry Hub index.
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

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-13-1.png)
<p>
</p>

<p>
This graph plots US exports of natural gas and the internationally accept price index for natural gas, the Henry Hub Index. As we can see from the plot, as the price for natural gas dropped following the 2008 recession, exports have steadily increased, until recently, when prices rose once again. While exports will prove to have little significance in our models, it is interesting to visualize the interactions between price and exports. 
</p>

# Model Preperation

<p>
In order to generate the best possible forecasting models, the time-series needed to be transform. Multiple transformations were tested, including seasonal adjustment and diﬀerencing. However, a Box-Cox transformation proved to be the optimal choice, as this yielded a stationary (KPSS test p-value >= 0.1) and positive time-series. Then, the time-series was split into train (2017-2020) and test (2021) sets.
</p>

### Box-Cox Transformation

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
           
<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-14-1.png)
<p>
</p>

    # Unit root test
    myts |> features(Consumption, unitroot_kpss)

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.163         0.1
<p>
</p>

<p>
As we can see above, our transformed time-series is stationary, as it passes the KPSS test.
</p>

### Train & Test Sets

<p>
In order to assess the accuracy of our models, we will need to split our data in to ‘train’ and ‘test’ data sets. The first four years of the data (80%), 2017-2020, will be put in the train set, and the final year (20%), 2021, will be put in the test data set.
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

<p>
</p>

# Model Generation

<p>
Now we will create our forecasting models using the training data set. The first model will be a linear regression model, using the predictor variables previously discussed, and trend and seasonal components. The second model will be an optimized ETS. The third model will be an optimized ARIMA. The final model will be an aggregation of all three previous models.
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
</p>

<p>
The ETS and ARIMA models were automatically optimized. The ETS model has multiplicative error and season components, and an additive trend component. The ARIMA is seasonally and non-seasonally differenced, and has a first order moving average component \[MA(q=1)\].
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

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-17-1.png)
<p>
</p>

    # ACF Plots
    augment(fit) |>
      ACF(.resid) |>
      autoplot() +
      labs(title = "ACF for All Models",
           y = "ACF")

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-17-2.png)
<p>
</p>

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
</p>

<p>
As we can see, the residuals are relatively normally distributed around the mean of zero, meaning they are homoscedastic. The ACF plots for the ETS and ARIMA models look good, as does the ENSEMBLE model. The ACF plot for the regression model is slightly concerning. Furthermore, while the ETS and ARIMA models pass the Ljung-Box test, the regression model does not. While normally, these two facts would disqualify this model, we will proceed with this model for the sake of this exercise. As we will see when we forecast with these models, it performs well despite these concerns.
</p>

# Forecast

<p>
Now we will visually plot the generated forecast models against the actual values. While we will quantify the accuracy of the models in the accuracy section later in this report, these visualizations will allow us to discern which models perform best.
</p>

### ETS Forecast

    # Generating forecast plot
    fit |> select(c(ETS)) |> forecast(h = 12) |> 
      autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "ETS Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption")  

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-18-1.png)
<p>
</p>

### ARIMA Forecast

    # Generating forecast plot
    fit |> select(c(ARIMA)) |> forecast(h = 12) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "ARIMA Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-19-1.png)
<p>
</p>


### Regression Forecast

    # Generating forecast plot
    fit |> select(c(LM)) |> forecast(new_data = test) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "Regression Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-20-1.png)
<p>
</p>
    

### Aggregate Forecast

    # Generating forecast plot
    fit |> select(c(ENSEMBLE)) |> forecast(new_data = test) |> autoplot(train) +
      ylim(c(0.0002, 0.0006)) +
      autolayer(test) +
      labs(title = "Aggregate Model Forecast",
           x = "Month",
           y = "Transformed Natural Gas Consumption") 

<p>
</p>
![](/assets/images/nat_gas_files/figure-markdown_strict/unnamed-chunk-21-1.png)
<p>
</p>
    

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
</p>

<p>
As we can see from the metrics in the table above, all models except for the regression performed well. If we consider MAPE, the ETS and ARIMA forecasts were only ~3.6% diﬀerent than the actual values. The aggregate model performed best overall, with its forecasts only diﬀering from the actuals by ~3%. 
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
</p>

<p>
According to the continuous ranked probability score (CRPS), a metric of prediction interval accuracy, our models rank the same as point-forecast accuracy, with the aggregate model being the best and regression the worst.
</p>

## Discussion: Optimal Model

<p>
The aggregate model performs the best. The ETS and ARIMA models are univariate but forecast well within the analyzed time frame. The regression model is multivariate and incorporates external factors that have been proven to aﬀect natural gas consumption levels in other research. While the regression model does not perform as well as the others for the time-series analyzed in this report, it may perform better over a longer time frame. Therefore, the optimal model is the aggregation of all three: ETS, ARIMA, and regression.
</p>

## Discussion: Utility

<p>
Forecasting energy demand and consumption is important in ensuring adequate power distribution and accessibility. For natural gas logistics planners, an ETS, ARIMA, or ensemble of both models, would assist in understanding how to allocate natural gas eﬀectively. For analysts trying to understanding macroeconomic patterns of natural gas consumption, the model generated in this report would be beneﬁcial. 
</p>

# Conclusion

<p>
Natural gas is preferable fossil fuel to coal and oil, given its lower emissions and abundance, making it a facet of climate change action. As the US progresses technologically, and enhances the implementability of renewable energies, natural gas consumption should decrease. However, as the forecasts in this model indicate, that decrease has not begun.
</p>

# Data and Files

[All data and files can be found on my Github](https://github.com/cbailey603/Natural_gas_Forecasting)

# References

<p>
Du, B., Guo, X., Wang, A., & Duan, H. (2023). Driving factors and decoupling analysis of natural gas consumption in major Organization for Economic Cooperation and Development countries. Science progress, 106(3), 368504231180783. https://doi.org/10.1177/00368504231180783
</p>

<p>
U.S. Energy Information Administration - EIA - independent statistics and analysis. Natural gas and the environment - U.S. Energy Information Administration (EIA). (n.d.).  https://www.eia.gov/energyexplained/natural-gas/natural-gas-and-the-environment.php#:~ :text=Natural%20gas%20is%20a%20relatively,an%20equal%20amount%20of%20energy
</p>

<p>
Ravnik, J., & Hriberšek, M. (2019). A method for natural gas forecasting and preliminary allocation based on unique standard natural gas consumption proﬁles. Energy, 180, 
149–162. https://doi.org/10.1016/j.energy.2019.05.084
</p>

