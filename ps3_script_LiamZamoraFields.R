#Part2
library(tidyverse)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

#Reshape to long
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

#To tsibble
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)


#Candadian Gas

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

#We sum the monthly values because that is what quarterly volume is asking. If we wanted average monthly
#gasoline production by quarter then we would average the months. It does still appear to be seasonal.

can_gas_q |> autoplot(Volume)

#AutoPlot

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

#The two series follow the same tend. The NSA series has peaks and valleys at the same time every year.
#The seasonaly adjusted series does not have this pattern. Instead, it appears to stay near the mean 
#of the NSA series for a given period.

can_gas |>
  autoplot(Volume) +
  geom_smooth()+
  labs(
    title = "Canadian Gas Production Volume",
    x = NULL, y = "Billions of Cubic Meters", color = NULL
  ) +
  theme_minimal()

#I added a trend line to the autopilot in order to get a clearer view of that trend through the seasonality
#of the data.

##PART3

#It seems like Canadian Gas production had wider swings in the period of the 80s-90s. 
#In general, the swings do appear to increase over time though, ignoring the 80s-90s period.
#I am not inclined to deny logging it.

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))
  

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Canadian Gas Production Log(Volume)",
    x = NULL, y = "Log Billions of Cubic Meters", color = NULL
  ) +
  theme_minimal()

#I think the log transformation is not appropriate here. The increases in the seasonal swings
#are not consistent enough with the increase in level to warrant the transformation.


retail_ts <- retail_ts |>
  mutate(log_sales = log(sales))
  
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = " Millions of dollars", color = NULL
  ) +
  theme_minimal()

retail_ts |> 
  filter(series_id == "RSAFSNA") |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Log Sales",
    x = NULL, y = "Log Millions of dollars", color = NULL
  ) +
  theme_minimal()

#Yes, the retail sales data is better suited to a log transformation,  
#the seasonal swings are increasing in size over time.

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )
  
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Production Growth Rate",
    x = NULL, y = "Percent", color = NULL
  ) +
  theme_minimal()

can_gas |>
  autoplot(growth_logdiff) +
  labs(
    title = "Canadian Gas Production Growth Rate (Log Difference)",
    x = NULL, y = "Percent", color = NULL
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Canadian Gas Production Growth Rate Difference",
    x = NULL, y = "Percent", color = NULL
  ) +
  theme_minimal()

#The log difference is not good enough of an approximation. Frequent swings as high as 3%.

##SA vs. NSA Growth Rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom, alpha = 0.6) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

#The NSA series is significantly more volatile than the SA series. The SA series is 
#much smoother. This is because of the seasonal peaks and valleys in the NSA series.

##Part 4

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "No Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

##In the NSA series, excluding 2020, we see a consistent repeated pattern that occurs every year. 
#The SA series does not have this pattern, it is much more stable and smooth, almost flat, with small growth.


can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "Log Billions of Cubic Meters") +  
  theme_minimal()

#The winter months seem to regularly be the peak of gas production, while the summer months are the lowest.
#This is likely do to natural gas creating energy to power Canada's heaters all winter.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  ) 

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

#December has the strongest seasonal effect at 1.2. Sales are about 20% higher than trend in that month.
#The size of seasonal swings appears to be shrinking over time.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#In general the match between our version of X13ARIMA and the Census Bureau's version is quite close.
#The difference is usually less than 1%. The largest difference occurs in 2020 thanks to the pandemic.

#LTSD
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#The approximate average monthly growth rate is 0.4%. June has the most significant seasonal effect.

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#The model does a good job at capturing the trend and seasonality between 1990 and around 2020. 
#Pre 1990 the model's trend level is highly inaccurate. 

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

#These residuals are not white noise. The Ljung-Box test shows a p-value of 0.000, meaning 
#that the residuals are not independent and that theres likely structure to model further.

