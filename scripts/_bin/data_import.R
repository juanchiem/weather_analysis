browseURL("https://rpubs.com/bradleyboehmke/weather_graphic")

library(tidyverse)
bce_serie <- readxl::read_excel("bce_serie.xlsx") %>%   
  mutate(day = as.numeric(format(date, format = "%d"))) %>% 
  select(year, month, day, tmax, tmin, tmean, rain, date)

bce_71_17 <- bce_serie %>% 
  filter(year!=2018) %>% 
  select(year, month, day, tmean) %>% 
  group_by(year) %>%
  mutate(newDay = seq(1, length(day))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>% 
  select(newDay, everything())

dim(bce_71_17)

Past <- bce_71_17 %>%
  group_by(newDay) %>%
  summarise(upper = max(tmean, na.rm = T), # identify max value for each day
            lower = min(tmean, na.rm = T), # identify min value for each day
            avg = mean(tmean, na.rm = T),  # calculate mean value for each day
            se = sd(tmean, na.rm = T)/sqrt(length(!is.na(tmean)))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()

dim(Past)


# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- bce_71_17 %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(tmean)) # identify lowest temp for each day from 1995-2013

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- bce_71_17 %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(tmean))  # identify highest temp for each day from 1995-2013

save(bce_serie, bce_71_17, Past, PastLows, PastHighs, file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")

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
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.5)+
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")->p0
p0

p1 <- p0 +  
  geom_vline(xintercept = c(31,59, 90, 120,151,181,212,243,273,304,334,365), 
                            colour = "wheat4", linetype=3, size=.5)+
  geom_hline(yintercept = c(10,20,30), colour = "white", linetype=1) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("Ene", "Feb", "Mar", "Abr",
                                "May", "Jun", "Jul", "Ago", "Sep",
                                "Oct", "Nov", "Dic"))
p1

# agregar temp de 2019
bce_19 <- readxl::read_excel("bce_2019.xlsx")

Present <- bce_2019 %>% 
  mutate(newDay = row_number()) %>%  # create matching x-axis as historical data
  select(newDay, day, month, year, avg=tmean) %>% 
  group_by(year, month) %>%
  arrange(newDay) %>%
  ungroup() 

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
df = data.frame(newDay = c(5, NA), avg = c(1,30)) 

p3 <- p2 + 
  geom_point(data=PresentLows, aes(x=newDay, y=avg), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=avg), colour="firebrick3")+ 
  annotate("text", x = 65, y = 1, label = "Días más frios que la serie 1971-2017", size=3, colour="blue3") +
  # annotate("text", x = 298, y = 30, label = "Días más cálidos que la serie 1971-2018", size=3, colour="firebrick3") +
  geom_point(data = df, col = c('blue3', "firebrick3"))

p3
# range(bce$year)
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,25,1))
p4 <- p3 +
  #barras
  annotate("segment", x = 181, xend = 181, y = 20, yend = 30, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 23, yend = 27, colour = "wheat4", size=3) +
  
  geom_line(data=legend_data, aes(x=x,y=y)) +
  
  annotate("segment", x = 183, xend = 185, y = 27, yend = 27, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 23, yend = 23, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 23, yend = 27, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 25, label = "Rango\nnormal", size=2, colour="gray30") +
  annotate("text", x = 162, y = 25, label = "Temperatura\n2019 ", size=2, colour="gray30") +
  annotate("text", x = 193, y = 30, label = "Record alta", size=2, colour="gray30") +
  annotate("text", x = 193, y = 20, label = "Record baja", size=2, colour="gray30")
p4

p5 <- p4 + 
  ggtitle("Temperatura media de Balcarce: 2019 y serie 1971-2018") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=15)) 
p5
