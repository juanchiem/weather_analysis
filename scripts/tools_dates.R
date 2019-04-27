dat <- data.frame(year = rep(1980:1989, each = 366), day= rep(1:366, times = 10))
head(dat)
tail(dat)

dat$month <- with(dat, format(strptime(paste(year, day), format = "%Y %j"), '%m'))
head(dat)
tail(dat)

require(lubridate)
x = as.Date('2010-06-10')
yday(x)