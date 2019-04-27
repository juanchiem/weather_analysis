browseURL("https://rpubs.com/bradleyboehmke/weather_graphic")

library(tidyverse)
library(lubridate)

bce <- readxl::read_excel("./data/bce_serie.xlsx") %>%   
  mutate(day = as.numeric(format(date, format = "%d"))) %>% 
  mutate(julian = yday(date)) %>%
  select(year, month, day, julian, tmax, tmin, tmean, rain)

# construir la banda central de Tmin y Tmax de cada dia  
Past <- bce %>%
  filter(year!=2018) %>% 
  group_by(julian) %>%
  summarise(
    # calcula el valor medio de Tmean para c/dia ("media de medias")
    avg = mean(tmean, na.rm = T),  
    
    # identifica el valor medio de las Tmax para c/dia
    upper_tmax = mean(tmax, na.rm = T), 
    # valor maximo de las Tmean para c/dia
    upper_tmean = max(tmean, na.rm = T),
    
    # identifica el valor medio de las Tmin para c/dia
    lower_tmin = mean(tmin, na.rm = T),
    # identifica el valor medio de las Tmax para c/dia
    lower_tmean = min(tmean, na.rm = T),
    
    # 80% de los años
    lower_80=quantile(tmean, .2, na.rm = T),  
    upper_80=quantile(tmean, .8, na.rm = T),  
    
    # calculate 95% CI for mean
    se = sd(tmean, na.rm = T)/sqrt(length(!is.na(tmean)))) %>% 
  mutate(
    avg_upper = avg+(2.101*se),
    avg_lower = avg-(2.101*se)) %>%  
  ungroup()

PastLows <- Past %>%
  group_by(julian) %>%
  summarise(Pastlow = min(avg)) # identify lowest temp for each day from 1995-2013

PastHighs <- Past %>%
  group_by(julian) %>%
  summarise(Pasthigh = max(avg))  # identify highest temp for each day from 1995-2013

# save(bce_serie, bce_71_17, Past, PastLows, PastHighs, file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")

p0 <- Past %>% 
  ggplot(aes(julian, avg)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(aes(x=julian, ymin=lower_80, ymax=upper_80), 
                 colour = "wheat4", alpha=.5)+
  geom_linerange(aes(x=julian, ymin=avg, ymax=upper_tmean), 
                 colour = "red", alpha=.2)+
  geom_linerange(aes(x=julian, ymin=lower_tmean, ymax=avg), 
                 colour = "blue", alpha=.2) +  
  geom_vline(xintercept = c(31,59, 90, 120,151,181,212,243,273,304,334,365), 
             colour = "wheat4", linetype=3, size=.5)+
  geom_hline(yintercept = c(10,20,30), colour = "white", linetype=3) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("Ene", "Feb", "Mar", "Abr",
                                "May", "Jun", "Jul", "Ago", "Sep",
                                "Oct", "Nov", "Dic")) 
p0

# agregar temp de 2019
bce_18_19 <- readxl::read_excel("./data/bce_2019.xlsx") 

Present <- bce_18_19 %>% 
  filter(year == 2019)%>%  
  mutate(julian = row_number()) %>%  # create matching x-axis as historical data
  select(julian, day, month, year, avg=tmean) %>% 
  group_by(year, month) %>%
  arrange(julian) %>%
  ungroup() 

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

p1 <- p0 + 
  geom_line(Present, mapping=aes(x=julian, y=avg, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)+
  transition_reveal(julian) + 
  coord_cartesian(clip = 'off') + 
  
p1

anim <- ggplot(airq, aes(Day, Temp, group = Month)) + 
  geom_line() + 
  geom_segment(aes(xend = 31, yend = Temp), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = Month), hjust = 0) + 
  transition_reveal(Day) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

anim_save("anim.gif", anim)


# Agregar dias extremos de 2019
df = data.frame(julian = c(5, NA), avg = c(1,30)) 

p2 <- p1 + 
  geom_point(data=PresentLows, aes(x=julian, y=avg), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=julian, y=avg), colour="firebrick3")+ 
  annotate("text", x = 65, y = 1, label = "Días más frios que la serie 1971-2017", size=3, colour="blue3") +
  # annotate("text", x = 298, y = 30, label = "Días más cálidos que la serie 1971-2018", size=3, colour="firebrick3") +
  geom_point(data = df, col = c('blue3', "firebrick3"))

p2
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
