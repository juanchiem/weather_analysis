library(tidyverse)

last_month <- Sys.Date() - 0:29
daf <- data.frame(
  date = last_month,
  price = runif(30)
)
base <- ggplot(df, aes(date, price)) +
  geom_line()

base + scale_x_date(date_labels = "%b %d")
base + scale_x_date(date_breaks = "1 week", date_labels = "%W")
base + scale_x_date(date_minor_breaks = "1 day")

# Set limits
library(lubridate)
base + scale_x_date(limits = c(ymd("2019-04-01"), ymd("2019-04-25")))

tbl <- tibble(
  day = seq.Date(ymd("2016-01-01"), ymd("2017-03-01"), by = 1)
) %>%
  mutate(temp = rnorm(nrow(.), mean = 0, sd = 15))

ggplot(tbl) +
  geom_line(aes(x = day, y = temp), color="#D55E00") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b")

d <- data.frame(date = as.Date("2016-01-01") + 0:365, 
                y = cumsum(rnorm(366)))
ggplot(d, aes(date, y)) + 
  geom_line() + 
  scale_x_date(date_breaks = "month")


# plotly 
library(plotly)
library(scales)
require(lubridate)
MySample <- data.frame(x = c("04-01-10","05-01-10","06-01-10","07-01-10","08-01-10","09-01-10",
                              "10-01-10","11-01-10","12-01-10","01-01-11","02-01-11","03-01-11",
                              "04-01-11","05-01-11","06-01-11","07-01-11","08-01-11","09-01-11",
                              "10-01-11","11-01-11","12-01-11","01-01-12","02-01-12","03-01-12",
                              "04-01-12","05-01-12","06-01-12"),
                       y = c(120,210,130,160,190,210,80,70,110,120,140,160,130,200,110,180,210,
                              200,90,60,100,100,120,170,100,180,120)) ## convert to dataframe

MySample$date <- as.Date(MySample$x, "%m-%d-%y")
MySample$year <- year(MySample$date)

p <- ggplot(MySample, aes(date, y, fill = year)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ year, scales = "free") +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_fill_gradient(breaks=unique(MySample$year))

p <- ggplotly(p)
p

# dygraphs
library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector()
