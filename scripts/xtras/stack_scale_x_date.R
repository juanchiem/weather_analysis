library(reprex)

reprex(venue = "so",
       {
  suppressWarnings(suppressMessages(library("tidyverse", quietly = T)))

# I have a time serie data from two consecutive years:

  dat <- tibble(
    date = seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by=1),
    var = rgamma(length(date), shape=2, scale=2)) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           julian = lubridate::yday(date))
  dat

  dat %>%
    ggplot() +
    geom_line(aes(julian, var, color = factor(month), linetype=factor(year)))

  dat %>%
    filter((year == 2017 & month %in% c("9","10", "11", "12"))|
             (year == 2018 & month %in% c("1", "2", "3"))) %>%
    mutate(julian_AWS = ifelse(julian>=244, julian-243, julian+123)) %>%
    ggplot() +
    geom_line(aes(julian_AWS, var, color = factor(month), linetype=factor(year)))+
    scale_y_continuous(labels = numform::f_celcius, limits = c(0, 30), expand = c(0, 0))+
    scale_x_continuous(breaks = c(1,#S
                                  31,#O
                                  61,#N
                                  91,#D
                                  121,#E
                                  151,#F
                                  181),#M
                       labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))+
    theme(axis.text.x=element_text(hjust=-1))
}
)


