library(tidyverse)
# browseURL("https://cals.arizona.edu/azmet/julian.html")
load(file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")

bce_PVO <- bce_serie %>% 
  filter(year!=2018) %>% 
  filter(month %in% c("9","10", "11", "12", "1", "2", "3")) %>% 
  select(year, month, day, tmean) %>% 
  mutate(month = fct_relevel(as.factor(month), "9","10", "11", "12", "1", "2", "3")) %>% 
  # fct_relevel(month = month, ) %>% 
  group_by(year) %>%
  arrange(month) %>%
  mutate(newDay = seq(1, length(day))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>% 
  select(newDay, everything())
bce_PVO
dim(bce_PVO)

Past <- bce_PVO %>%
  group_by(newDay) %>%
  summarise(upper = max(tmean, na.rm = T), # identify max value for each day
            lower = min(tmean, na.rm = T), # identify min value for each day
            avg = mean(tmean, na.rm = T),  # calculate mean value for each day
            se = sd(tmean, na.rm = T)/sqrt(length(!is.na(tmean)))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()

dim(Past)

PastLows <- bce_PVO %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(tmean)) # identify lowest temp for each day from 1995-2013

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- bce_PVO %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(tmean))  # identify highest temp for each day from 1995-2013

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(0,30, by=10))
Past %>% 
  ggplot(aes(newDay, avg)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        # axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")+
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_upper, ymax=upper), colour = "red", alpha=.2)+
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=avg_lower), colour = "blue", alpha=.2)->p0
p0

p1 <- p0 +  
  geom_vline(xintercept = c(30+1, #O 
                            31+30, #N
                            61+31, #D
                            92+30, #E 
                            122+29, #F
                            151+30), #M 
             colour = "wheat4", linetype=3, size=.5)+
  scale_y_continuous(breaks = seq(0,30, by=10), labels = a) +
  geom_hline(yintercept = c(10,20,30), colour = "white", linetype=2) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,#S
                                45,#O
                                75,#N
                                105,#D
                                135,#E
                                165,#F 
                                195#M
                     ),
                     labels = c("Sep", "Oct", "Nov", "Dic", "Ene", "Feb", "Mar"))
p1

bce_18_19 <- readxl::read_excel("bce_2019.xlsx")

# bce_18_19 %>% count(year,month)

Present <- bce_18_19 %>%
  filter((year == 2018 & month %in% c("9","10", "11", "12")) |
           (year == 2019 & month %in% c("1","2", "3"))) %>% 
  mutate(month = fct_relevel(as.factor(month), "9","10", "11", "12", "1", "2", "3")) %>% 
  mutate(newDay = row_number()) %>%  # create matching x-axis as historical data
  select(newDay, day, month, year, avg=tmean) %>% 
  group_by(year, month) %>%
  arrange(newDay) %>%
  ungroup() 

# ftable(xtabs(~year + month, data = Present))

# View(Present)
# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(avg<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(avg>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter

p2 <- p1 + 
  geom_line(Present, mapping=aes(x=newDay, y=avg, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)
p2

# Agregar dias extremos de 2019
p3 <- p2 + 
  geom_point(data=PresentLows, aes(x=newDay, y=avg), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=avg), colour="firebrick3")+ 
  annotate("text", x =5, y = 30, hjust = 0, 
           label = "Días más frios que la serie 1971-2017", size=3, colour="blue3") +
  annotate("text", x =5, y = 28, hjust = 0,
           label = "Días más cálidos que la serie 1971-2017", size=3, colour="firebrick3") +
  geom_point(data = data.frame(newDay = c(3,3), avg = c(30,28)), 
             col=c('blue3', "firebrick3"), aes(x=newDay))
p3

# range(bce$year)

p4 <- p3 +
  #barras
  annotate("segment", x=181-100, xend=181-100, y=2, yend=5, 
           colour = "wheat4", size=2) +
  annotate("segment", x = 181-100, xend = 181-100, y=5, yend=7, 
           colour = "red", size=2,alpha = 0.2) +
  annotate("segment", x = 181-100, xend = 181-100, y=0, yend=2, 
           colour = "blue", size=2, alpha = 0.2) +
  
  geom_line(data=data.frame(x=seq(75,75+7),y=rnorm(8,3,1)), 
                            aes(x=x,y=y))

p4b <- p4+
  annotate("segment", x = 183-100, xend = 185-100, y = 5, yend = 5, colour = "wheat4", size=.5) +
  annotate("segment", x = 183-100, xend = 185-100, y = 2, yend = 2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185-100, xend = 185-100, y = 2, yend = 5, colour = "wheat4", size=.5) +
  annotate("text", x = 190-100, y = 3, label = "Rango\nnormal", size=2, colour="gray30") +
  annotate("text", x = 167-100, y = 3, label = "Temp\n2018/19 ", size=2, colour="gray30") +
  annotate("text", x = 193-100, y = 7, label = "Record alta", size=2, colour="gray30") +
  annotate("text", x = 193-100, y = 0, label = "Record baja", size=2, colour="gray30")
p4b

p5 <- p4b + 
  ggtitle("Temperatura media de Balcarce: campaña 2018/19 y serie 1971-2018") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=15)) 
p5

p6 <- p5 +
  #barras de floracion
  annotate("rect", xmin=105, xmax=135, ymin=0, ymax=30, 
           alpha=0.2, fill="grey90")+
  annotate("text", x = 120, y = 1, label = "Floración", size=3) +
  
  #barras de llenado
  annotate("rect", xmin=135, xmax=190, ymin=0, ymax=30, 
           alpha=0.2, fill="grey60")+
  annotate("text", x = 160, y = 1, label = "Llenado", size=3)
p6

#R1 15 de dic. Floracion 15 de enero. Fin de llanado 10 marzo
