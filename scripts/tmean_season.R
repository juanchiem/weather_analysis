library(tidyverse)
browseURL("https://cals.arizona.edu/azmet/julian.html")
load(file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")

bce_PVO <- bce_serie %>%
  filter(month %in% c("9","10", "11", "12", "1", "2", "3")) %>%
  mutate(month = fct_relevel(as.factor(month), "9","10", "11", "12", "1", "2", "3"),
         julian_PVO = ifelse(julian>=244, julian-243, julian+123))

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}
a <- dgr_fmt(seq(0,30, by=10))# create y-axis variable

# bce_PVO %>% count(month)


# Grafico base: serie + season -----
bce_PVO %>%
  ggplot(aes(julian_PVO, avg)) +
  geom_linerange(aes(x=julian_PVO, ymin=avg, ymax=upper_tmean),
                 colour = "red", alpha=.2)+
  geom_linerange(aes(x=julian_PVO, ymin=lower_tmean, ymax=avg),
                 colour = "blue", alpha=.2) +
  geom_linerange(aes(x=julian_PVO, ymin=lower_80, ymax=upper_80),
                 colour = "wheat4") +
  # Agregar tmean de la campaña
  geom_line(bce_season, mapping=aes(x=julian_PVO, y=tmean, group=1)) +
  geom_vline(xintercept = c(1,#S
                            30,#O
                            60,#N
                            90,#D
                            120,#E
                            150,#F
                            180,#M
                            213),
             colour = "wheat4", linetype=3, size=.5)+
  scale_y_continuous(breaks = seq(0,30, by=10), labels = a) +
  geom_hline(yintercept = c(10,20,30), colour = "white", linetype=2) +
  # scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  # scale_x_continuous(breaks = round(seq(min(bce_PVO$julian_PVO), max(bce_PVO$julian_PVO), by = 10),1)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(1,#S
                                30,#O
                                60,#N
                                90,#D
                                120,#E
                                150,#F
                                180),#M
                     labels = c("Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar"))+
  theme(axis.text.x = element_text(hjust = 0))-> p_PVO
p_PVO

# scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1)) +

bce_season <- readxl::read_excel("data/bce_2019.xlsx") %>%
  filter((year == 2018 & month %in% c("9","10", "11", "12")) |
           (year == 2019 & month %in% c("1","2", "3"))) %>%
  mutate(month = fct_relevel(as.factor(month), "9","10", "11", "12", "1", "2", "3"),
         date = paste0(as.character(year), "/", as.character(month), "/", as.character(day))) %>%
  mutate(julian = lubridate::yday(date),
         julian_PVO = ifelse(julian>=244, julian-243, julian+123))
# bce_18_19 %>% count(year,month)

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
PresentLows <- bce_PVO %>%
  left_join(select(bce_season,julian_PVO, tmean), by = "julian_PVO") %>%  # merge historical lows to current year low data
  mutate(record = ifelse(tmean < lower_tmean, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y") %>%   # filter for days that represent current year record lows
  select(julian_PVO, day, month, avg=tmean)

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- bce_PVO %>%
  left_join(select(bce_season,julian_PVO, tmean), by = "julian_PVO") %>%  # merge historical lows to current year low data
  mutate(record = ifelse(tmean > upper_tmean, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  %>% # filter for days that represent current year record lows
  select(julian_PVO, day, month, avg=tmean)

library(ggrepel)
options(repr.plot.width = 2, repr.plot.height = 2)

# Agregar dias extremos -----
p1_PVO <- p_PVO +
  # Agregar dias extremos de 2019
  geom_point(data=PresentLows, aes(x=julian_PVO, y=avg), colour="blue3") +
  geom_text_repel(data=PresentLows, aes(label=paste0(day,"/",month)),
                  size=3, nudge_y=-5, segment.size=0.2, segment.color="grey50", direction="x")+

  geom_point(data=PresentHighs, aes(x=julian_PVO, y=avg), colour="firebrick3")+
  geom_text_repel(data=PresentHighs, aes(label=paste0(day,"/",month)),
                  size=3,nudge_y=5, segment.size=0.2,segment.color="grey50", direction="x")+

  annotate("text", x =5, y = 30, hjust = 0,
           label = "Días más frios que la serie 1971-2017", size=3, colour="blue3") +
  annotate("text", x =5, y = 28, hjust = 0,
           label = "Días más cálidos que la serie 1971-2017", size=3, colour="firebrick3") +
  geom_point(data = data.frame(julian_PVO = c(3,3), avg = c(30,28)),
             col=c('blue3', "firebrick3"), aes(x=julian_PVO))+
  ggtitle("Temperatura media de Balcarce: 2019 y serie 1971-2017") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=15))
p1_PVO

# Agregar fenologia -----
# R1 15 de dic. Floracion 15 de enero. Fin de llanado 10 marzo

p_final <- p1_PVO +
  #barras de floracion
  annotate("rect", xmin=105, xmax=135, ymin=0, ymax=30,
           alpha=0.2, fill="grey90")+
  annotate("text", x = 120, y = 1, label = "Floración", size=3) +

  #barras de llenado
  annotate("rect", xmin=135, xmax=190, ymin=0, ymax=30,
           alpha=0.2, fill="grey60")+
  annotate("text", x = 160, y = 1, label = "Llenado", size=3)
p_final

