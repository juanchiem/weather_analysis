library(tidyverse)
library(lubridate)

theme_set(theme_grey() +
            theme(plot.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.ticks = element_blank(),
                  # axis.text = element_blank(),
                  axis.title = element_blank())
          )
bce <- readxl::read_excel("./data/bce_serie.xlsx")

# Calcular lluvias acumuladas por decada
bce_rain <- bce %>%
  filter(year!=2018) %>%
  group_by(date=if_else(day(date) >= 30,
                        floor_date(date, "20 days"),
                        floor_date(date, "10 days"))) %>%
  summarize(rain_acum = sum(rain),
            days = n()) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  ungroup %>%
  group_by(year) %>%
  mutate(decada = row_number()) %>%
  ungroup

# Chequear
ht(bce_rain, 6)
bce_rain %>% count(year,days) %>% table

# bce_rain %>%
#   ggplot(aes(factor(decada), rain_acum, group=factor(decada))) +
#   # geom_tufteboxplot()+
#   geom_boxplot(coef = 0, outlier.shape = NA, fill = NA)+
#   scale_y_continuous(limits = quantile(bce_rain$rain_acum, c(0.1, 0.97)))

library(Hmisc)
f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.2, 0.5, 0.8, 0.9))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

bce_rain %>%
  ggplot(aes(x=decada, y=rain_acum, group=decada)) +
  stat_summary(fun.data = f, geom="boxplot",
               fill="grey", col="grey90", width =0.3, outlier.size = NA)+
  stat_summary(fun.y = function(x) median(x), geom='point', col="white", size = 3)

bce_rain %>%
  filter(decada %in% c(25:36,1:9)) %>%
  mutate(decada_PVO = ifelse(decada>=25, decada-24, decada+12)) %>% #View
  ggplot(aes(x=decada_PVO, y=rain_acum, group=decada_PVO)) +
  stat_summary(fun.data = f, geom="boxplot",
               fill="deepskyblue", col="grey90", width =0.3, outlier.size = NA)+
  stat_summary(fun.y = function(x) median(x), geom='point', col="white", size = 3)+
  geom_vline(xintercept = c(0.5,#S
                            3.5,#O
                            6.5,#N
                            9.5,#D
                            12.5,#E
                            15.5,#F
                            18.5,#M
                            21.5),
             colour = "wheat4", linetype=3, size=.3)+
  geom_hline(yintercept = c(0,25,50,75), colour = "wheat4", linetype=3, size=.3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(2,#S
                                5,#O
                                8,#N
                                11,#D
                                14,#E
                                17,#F
                                20),#M
                     labels = c("Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar"))->p0
p0
