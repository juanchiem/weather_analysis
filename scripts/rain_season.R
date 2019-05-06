library(tidyverse)
library(lubridate)

bce_season1819 <- readxl::read_excel("data/bce_2019.xlsx") %>%
  mutate(date = as.Date(paste0(as.character(year), "/", as.character(month), "/", as.character(day)))) %>%
  mutate_at(vars(rain), ~replace(., is.na(.), 0))

bce_season <-
  bce_season1819 %>%
    group_by(date = if_else(day(date) >= 30,
                          floor_date(date, "20 days"),
                          floor_date(date, "10 days"))) %>%
    summarize(rain_acum = sum(rain),
              days=n()) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    ungroup %>%
    group_by(year) %>%
    mutate(decada = row_number()) %>%
    ungroup

# bce_season1819 %>% View

bce_season1 = bce_season %>%
  filter((year == 2018 & decada %in% c(25:36)) |
          (year == 2019 & decada %in% 1:9)) %>%
  mutate(decada_PVO = ifelse(decada>=25, decada-24, decada+12)) #%>% #View

p1 <- p0 + geom_point(data = bce_season1, aes(decada_PVO, rain_acum), col ="brown1")+
  geom_line(data = bce_season1, aes(decada_PVO, rain_acum, group=1), col ="brown1", linetype = "dashed")

p_final <- p1 +
  #barras de floracion
  annotate("rect", xmin=11, xmax=14, ymin=75, ymax=85,
           alpha=0.5, fill="aquamarine")+
  annotate("text", x = 12, y = 80, label = "Floración", size=3) +
  #barras de llenado
  annotate("rect", xmin=14, xmax=19, ymin=75, ymax=85,
           alpha=0.5, fill="bisque")+
  annotate("text", x = 15, y = 80, label = "Llenado", size=3)+
  annotate("text", x = 14, y = 90, label = "Cultivo de girasol", size=3)+

  labs(title = "Precipitaciones en Balcarce: 2018/19 y serie 1971-2017",
       subtitle = "Valores acumulados por períodos de 10 días",
       caption = "- Campaña 2018/19 (en puntos rojos)\n- Serie 1971-2017: mediana (puntos blancos) dentro del intervalo de 80% de probabilidad (barras azules) ")+
  theme(plot.caption = element_text(hjust = 0))

p_final

ggsave(file = "plots/bce_lluvias.png", w=80, h=50, units="mm", dpi=300, scale=2)
