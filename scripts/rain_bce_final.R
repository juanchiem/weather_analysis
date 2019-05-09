suppressWarnings(suppressMessages(library("tidyverse", quietly = T)))
library(lubridate)
source('~/Dropbox/Proyectos/clima bce/scripts/theme_weather.R')

bce <- readxl::read_excel("./data/bce_clima.xlsx") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         julian = lubridate::yday(date))

# Calcular lluvias acumuladas por decada
bce_serie <-
  bce %>%
  filter(year!=2018) %>%
  group_by(date=if_else(day(date) >= 30,
                        floor_date(date, "20 days"),
                        floor_date(date, "10 days"))) %>%
  summarize(rain_acum = sum(rain),
            days = n()) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  # julian = yday(date)) %>%
  ungroup %>%
  group_by(year) %>%
  mutate(decada = row_number()) %>%
  ungroup %>%
  group_by(decada) %>%
  summarise(month = first(month),
            med = quantile(rain_acum, .5, na.rm = T),
            lower_80=quantile(rain_acum, .2, na.rm = T), # Rango 80% de los años
            upper_80=quantile(rain_acum, .8, na.rm = T))

bce_18_19 <-
  bce %>%
  group_by(date = if_else(day(date) >= 30,
                          floor_date(date, "20 days"),
                          floor_date(date, "10 days"))) %>%
  summarize(rain_acum_season = sum(rain)) %>% #, days=n()) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year) %>%
  mutate(decada = row_number()) %>%
  filter(date > '2018-08-31', date < '2019-03-30')

bce_18_19 <-
  bce_18_19 %>%
  left_join(bce_serie, by = c("decada")) %>%
  mutate(date = as.Date(date))

# print(bce_18_19, n=Inf)
# print(bce_serie, n=Inf)

p_5=position_nudge(x = 5)
(p1 <-
    bce_18_19 %>%
    ggplot(aes(x=date)) +
    geom_pointrange(aes(y=med, ymin=lower_80, ymax=upper_80), fill='white', color='deepskyblue',
                    shape=21, fatten=.7, size=3, position=p_5)+
    geom_point(aes(y=rain_acum_season), col ="brown1",
               position=p_5) +
    geom_line(aes(y=rain_acum_season, group=1), col ="brown1", linetype="dashed", position=p_5)+
    scale_y_continuous(limits=c(0, 100), expand=c(0.05, 0))+
    scale_x_date(date_breaks="1 month", date_labels="%b", expand=expand_scale(0.05,0))
)

r5 = c(as.Date("2018-12-15"),  as.Date("2019-01-15")); diff(r5)
r6 = c(as.Date("2019-01-15"),  as.Date("2019-03-10")); diff(r6)
midpoint <- function(interval) { min(interval)+(max(interval)-min(interval))/2 }; midpoint(r5)

(p_final <-
    p1 +
    # barras de floracion y llenado
    annotate("rect", xmin= c(min(r5), min(r6)), xmax=c(max(r5),max(r6)), ymin=70, ymax=90, alpha=0.3, fill=c("grey80","grey60"))+
    annotate("text", x = c(midpoint(r5), midpoint(r6)), y=80,col="red", parse=TRUE, size=3,
             label=c('bold("Floración")','bold("Llenado")')) +
    labs(x=NULL, y=NULL,
         title = "Precipitaciones decádicas en Balcarce y estadíos reproductivos del girasol",
         subtitle = "- Campaña 2018/19 (en puntos rojos)\n- Serie 1971-2017: mediana (puntos blancos) y rango 80% (barras azules)",
         # "mm acumulados por períodos de 10 días",
         caption = "Datos registrados en la estación meteorológica de la EEA INTA Balcarce")
)

# ggsave(file = "plots/bce_lluvias.png", w=80, h=50, units="mm", dpi=300, scale=2)
