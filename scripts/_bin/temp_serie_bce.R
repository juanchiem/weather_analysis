# browseURL("https://rpubs.com/bradleyboehmke/weather_graphic")
suppressWarnings(suppressMessages(library("tidyverse", quietly = T)))

theme_set(theme_grey() +
            theme(plot.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.ticks = element_blank(),
                  # axis.text = element_blank(),
                  axis.title = element_blank()))

# bce <- readxl::read_excel("./data/bce_serie.xlsx") %>%
#   mutate(day = as.numeric(format(date, format = "%d"))) %>%
#   mutate(julian = lubridate::yday(date)) %>%
#   select(year, month, day, julian, tmax, tmin, tmean, rain)
#
# # construir la banda central de Tmin y Tmax de cada dia
# bce_serie <- bce %>%
#   filter(year!=2018) %>%
#   group_by(julian) %>%
#   summarise(
#     month = first(month),
#     day = first(day),
#     avg = mean(tmean, .2, na.rm = T),
#     # Rango 80% de los años (rango interno)
#     lower_80=quantile(tmean, .2, na.rm = T),
#     upper_80=quantile(tmean, .8, na.rm = T),
#     # Min y max de tmean (rango externo)
#     lower_tmean=min(tmean, na.rm = T),
#     upper_tmean=max(tmean, na.rm = T)
#     ) %>%
#   mutate(month_lab = format(ISOdate(2004,1:12,1),"%B")[month]) %>%
#   ungroup()
#
# bce_serie
#
# PastLows <- bce %>%
#   filter(year!=2018) %>%
#   group_by(julian) %>%
#   summarise(month = first(month),
#             day = first(day),
#             Pastlow = min(tmean)) # identify lowest temp for each day from 1995-2013
#
# PastHighs <- bce %>%
#   filter(year!=2018) %>%
#   group_by(julian) %>%
#   summarise(month = first(month),
#             day = first(day),
#             Pasthigh = max(tmean)) # identify lowest temp for each day from 1995-2013
#
# save(bce, bce_serie, PastLows, PastHighs, file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")

load("~/Dropbox/Proyectos/clima bce/bce_serie.RData")

month_lab = substr(unique(bce_serie$month_lab), start=1, stop=3)

p0 <- bce_serie %>%
  ggplot(aes(julian, avg)) +
  geom_linerange(aes(x=julian, ymin=avg, ymax=upper_tmean),
                 colour = "red", alpha=.2)+
  geom_linerange(aes(x=julian, ymin=lower_tmean, ymax=avg),
                 colour = "blue", alpha=.2) +
  geom_linerange(aes(x=julian, ymin=lower_80, ymax=upper_80),
                 colour = "wheat4")+
  geom_vline(xintercept = c(31,59, 90, 120,151,181,212,243,273,304,334,365),
             colour = "wheat4", linetype=3, size=.5)+
  geom_hline(yintercept = c(10,20,30), colour = "white", linetype=3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = month_lab)
p0

# range(bce$year)
# legend_data <- data.frame(x=seq(175,182),y=rnorm(8,25,1))
p1 <- p0 +
  #barras
  annotate("segment", x=181, xend=181, y=23, yend=27, colour = "wheat4", size=3) +
  annotate("segment", x=181, xend=181, y=27, yend=30, colour="red", alpha=.2, size=3) +
  annotate("segment", x=181, xend=181, y=20, yend=23, colour="blue", alpha=.2, size=3) +

  # geom_line(data=legend_data, aes(x=x,y=y)) +

  # annotate("segment", x = 183, xend = 185, y = 27, yend = 27, colour = "wheat4", size=.5) +
  # annotate("segment", x = 183, xend = 185, y = 23, yend = 23, colour = "wheat4", size=.5) +
  # annotate("segment", x = 185, xend = 185, y = 23, yend = 27, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 25, label = "80%\nde los años", size=2, colour="gray30") +
  # annotate("text", x = 162, y = 25, label = "Temperatura\n2019 ", size=2, colour="gray30") +
  annotate("text", x = 193, y = 30, label = "Record alta", size=2, colour="gray30") +
  annotate("text", x = 193, y = 20, label = "Record baja", size=2, colour="gray30")
p1
