# Time series graphics exercise
library(fpp3)

setwd('C:/Users/hoybr/Documents/coding tutorial/time series/2- time series graphics')

# Question 1 - explore the time series

# Bricks
help(aus_production)

# Time interval - quarterly
beer <- aus_production %>% 
  select(1:2)

beer %>% 
  autoplot(Beer)

# Lynx
help(pelt)

# Time interval - yearly
lynx <- pelt %>% 
  select(1, 3)

lynx %>% 
  autoplot(Lynx)


# Close
help(gafa_stock)

# Time interval - daily but not consistent
Close <- gafa_stock %>% 
  select(Symbol, Date, Close)

Close %>% 
  autoplot(Close)

# Demand
help(vic_elec)

# Time interval - half hourly
vic_elec %>% 
  autoplot(Demand) +
  labs(title = "Electricity Demand", 
       y = "GWh")


# Question 2 - what days corresponded to the peak closing price for each of the four stocks
unique(Close$Symbol)

Close %>% filter(Symbol == "AAPL", 
                 year(Date) == 2016) %>% 
  gg_season(Close, period = "week")



Close %>% filter(Symbol == "AMZN") %>% 
  gg_season(Close, period = "week")


Close %>% filter(Symbol == "GOOG") %>% 
  gg_season(Close, period = "week")


Close %>% filter(Symbol == "FB") %>% 
  gg_season(Close, period = "week")


# Question 3
tute1 <- readr::read_csv("tute1.csv")
View(tute1)
# converting to a tsibble
tute <- tute1 %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter)


tute |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")


# Question 4
install.packages("USgas")
library(USgas)

us_total

# converting to a tsibble
USgas <- us_total %>% 
  as_tsibble(index = year,
             key = state)

# Plotting time plots for New England states
USgas %>% 
  filter(state %in% c("Maine", "Vermont", "New Hampshire", 
                      "Massachusetts", "Connecticut", "Rhode Island")) %>% 
  autoplot(y) +
  labs(title = "Annual Gas consumption within the New England Area",
       y = "Gas consumption")


# Question 5
tourist <- readxl::read_xlsx("tourism.xlsx")

# Converting to a tsibble
tourist$Quarter <- as.Date(tourist$Quarter)

tourist <- tourist %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter, 
             key = c(Region, State, Purpose))

#Find what combination of Region and 
# Purpose had the maximum number of overnight trips on average
tourist %>%
  group_by(Region, Purpose) %>% 
  summarise(avg_trips = mean(Trips))


# Question 6
arrival <- aus_arrivals

# Autoplot to see trend
arrival %>% 
  autoplot(Arrivals)

arrival %>% 
  autoplot(Arrivals) +
  facet_grid(vars(Origin), scales = "free_y")

# Seeing seasonal trend
arrival %>% 
  gg_season(Arrivals) +
  facet_grid(vars(Origin), scales = "free_y")

# Subseries
arrival %>% filter(Origin == "NZ") %>% 
  gg_subseries(Arrivals)

arrival %>% filter(Origin == "UK") %>% 
  gg_subseries(Arrivals)

arrival %>% filter(Origin == "US") %>% 
  gg_subseries(Arrivals)

arrival %>% filter(Origin == "Japan") %>% 
  gg_subseries(Arrivals)
