df <- data.frame(dvec <- as.Date("2001-04-01")+0:90)

df$dvec <- as.numeric(dvec-dvec[1]) %/% 10
df

(expenses <- data_frame(
  date=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by=1),
  amount=rgamma(length(date), shape = 2, scale = 20)))

expenses %>%
  group_by(month = floor_date(date, "10 days")) %>%
  summarize(amount=sum(amount),
            n=n()) %>% View

dates <- seq(as.POSIXct("2011-01-01 00:00:00", tz = "GMT"),
             as.POSIXct("2011-01-10 23:59:00", tz = "GMT"),
             by = 900)
windspeed <- sample(0:20, length(dates), replace = F)
Data  <- data.frame(dates = dates, windspeed = windspeed)

breaks <- seq(floor_date(min(Data$dates), unit = "days") + hms("17:00:00"), ceiling_date(max(Data$dates), unit = "days"), by = "12 hour")
Data$lev <- cut.POSIXt(Data$dates, breaks = breaks, labels = 1:(length(breaks)-1))
filter(Data, lev %in% seq(1 , max( as.numeric(as.character(unique(Data$lev))), na.rm = T ), by =2) )%>%
  group_by(lev) %>% summarise(means  = mean(windspeed))
