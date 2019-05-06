library(reprex)

reprex(venue = "so",
       {
         suppressWarnings(suppressMessages(library("tidyverse", quietly = T)))

         dat <- tibble(
           date = seq(as.Date("2010-01-01"), as.Date("2018-12-31"), by=1),
           tmean = rgamma(length(date), shape=2, scale=2)) %>%
           mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  julian = lubridate::yday(date))

         # calculate daily average temperature for the serie 2010-2016
         dat_serie <- dat %>%
           filter(year<2017) %>%
           group_by(julian) %>%
           summarise(
             month = first(month),
             avg = mean(tmean, .2, na.rm = T)) %>%
           ungroup()


         p0 <- dat_serie %>%
           filter(month %in% c("1", "2", "3", "9","10", "11", "12")) %>%
           mutate(julian_AWS = ifelse(julian>=244, julian-243, julian+123)) %>%
           ggplot() +
           geom_line(aes(julian_AWS, avg, color = factor(month)))+

           scale_x_continuous(breaks = c(1,#S
                                         31,#O
                                         61,#N
                                         91,#D
                                         121,#E
                                         151,#F
                                         181),#M
                              labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))+
           theme(axis.text.x=element_text(hjust=-1))

         dat_17_18 <-  dat %>%
           filter(date >= '2017-09-01', date < '2018-03-30') %>%
           mutate(julian_AWS = ifelse(julian>=244, julian-243, julian+123))

         p0 + geom_line(data = dat_17_18,
                        aes(julian_AWS, tmean, linetype = factor(year)))
       }
)

dat_17_18 <-
  dat %>%
  filter(date >= '2017-09-01', date < '2018-03-30') %>%
  left_join(dat_serie, by = c("julian", "month"))
str(dat_17_18)

ggplot(dat_17_18, aes(x = date)) +
  geom_line(aes(y = avg, color = factor(month))) +
  geom_line(aes(y = tmean, linetype = factor(year))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(axis.text.x = element_text(hjust = -1))

# https://stackoverflow.com/questions/45505388/add-condition-geom-point-ggplot2-r
# https://learnr.wordpress.com/2009/04/29/ggplot2-labelling-data-series-and-adding-a-data-table/
