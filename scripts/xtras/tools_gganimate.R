library(ggplot2)
# library(ggstance)
library(gganimate)

airq <- airquality
airq$Month <- format(ISOdate(2004,1:12,1),"%B")[airq$Month]
airq
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
