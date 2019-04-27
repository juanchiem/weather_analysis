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

p2 <- p1 + 
  geom_line(Present, mapping=aes(x=julian, y=avg, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)#+
# transition_reveal(julian) + 
# coord_cartesian(clip = 'off') + 

p2

# anim <- ggplot(airq, aes(Day, Temp, group = Month)) + 
#   geom_line() + 
#   geom_segment(aes(xend = 31, yend = Temp), linetype = 2, colour = 'grey') + 
#   geom_point(size = 2) + 
#   geom_text(aes(x = 31.1, label = Month), hjust = 0) + 
#   transition_reveal(Day) + 
#   coord_cartesian(clip = 'off') + 
#   labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
#   theme_minimal() + 
#   theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
anim_save("anim.gif", anim)


# Agregar dias extremos de 2019
df = data.frame(julian = c(5, NA), avg = c(1,30)) 

library(ggrepel)
p2
options(repr.plot.width = 2, repr.plot.height = 2)

p3 <- p2 + 
  geom_point(data=PresentLows, aes(x=julian, y=avg), colour="blue3", size = 1) + 
  geom_text_repel(data=PresentLows, aes(label=paste0(day,"/",month)),
                  size=3,nudge_y=-5, segment.size=0.2,segment.color="grey50", direction="x")+
  geom_point(data=PresentHighs, aes(x=julian, y=avg), colour="firebrick3", size = 1)+ 
  # annotate("text", x = 298, y = 30, label = "Días más cálidos que la serie 1971-2018", size=3, colour="firebrick3") +
  annotate("text", x = 70, y = 1, label = "Días más frios que la serie 1971-2017", size=3, colour="blue3") +
  geom_point(data = df, size = 1, col = c('blue3', "firebrick3"))
p3  


p4 <- p3 + 
  ggtitle("Temperatura media de Balcarce: 2019 y serie 1971-2017") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=15)) 
p4

ggsave(file = "plots/bce_2019.png", w=160, h=80, units="mm", scale=1)
