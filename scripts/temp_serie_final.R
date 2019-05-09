pacman::p_load(tidyverse, lubridate)
source(here::here('scripts/theme_weather.R'))

bce <- readxl::read_excel(here::here("data/balcarce_clima.xlsx")) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         julian = lubridate::yday(date))

# construir la banda central de Tmin y Tmax de cada dia
bce_serie <- bce %>%
  filter(year<2018) %>%
  group_by(julian) %>%
  summarise(
    month = first(month),
    avg = mean(tmean, .2, na.rm = T),
    # Rango 80% de los años (rango interno)
    lower_80=quantile(tmean, .2, na.rm = T),
    upper_80=quantile(tmean, .8, na.rm = T),
    # Min y max de tmean (rango externo)
    lower_tmean=min(tmean, na.rm = T),
    upper_tmean=max(tmean, na.rm = T)) %>%
  ungroup()

# save(bce, bce_serie, file = "~/Dropbox/Proyectos/clima bce/bce_serie.RData")
# load("~/Dropbox/Proyectos/clima bce/bce_serie.RData")

bce_18_19 <-
  bce %>%
  filter(date > '2018-08-31', date < '2019-03-30') %>%
  left_join(bce_serie, by = c("julian", "month")) %>%
  mutate(date = as.Date(date)) %>%
  droplevels()

(p1 <- bce_18_19 %>%
  ggplot(aes(x=date)) +
  geom_linerange(aes(ymin=avg, ymax=upper_tmean), col="red", alpha=.2)+
  geom_linerange(aes(ymin=lower_tmean, ymax=avg), col="blue", alpha=.2) +
  geom_linerange(aes(ymin=lower_80, ymax=upper_80), col="wheat4")+
  geom_line(aes(y=tmean)) +
  scale_y_continuous(labels=numform::f_celcius, limits=c(0, 30), expand=c(0.05, 0))+
  scale_x_date(date_breaks="1 month", date_labels="%b", expand=expand_scale(0.01,0)))

rec_frio <- bce_18_19[which(bce_18_19$tmean<bce_18_19$lower_tmean),]
rec_cal <- bce_18_19[which(bce_18_19$tmean>bce_18_19$upper_tmean),]

library(ggrepel); options(repr.plot.width = 2, repr.plot.height = 2)
(p2 <- p1 +
  # Agregar medias record frío
  geom_point(data = rec_frio, aes(date, y = tmean), colour = "blue") +
  geom_text_repel(data=rec_frio[1,], aes(y=tmean, label="Récord fríos"), size=3,
                  min.segment.length = unit(0, 'lines'), nudge_y = -2, segment.color="grey50")+
  # Agregar medias record cálido
  geom_point(data =rec_cal, aes(date, y=tmean), col="red")+
  geom_text_repel(data = rec_cal, aes(y=tmean, label="Récord cálido"), size=3,
                  min.segment.length = unit(0, 'lines'), nudge_y=2, segment.color="grey50")+
  # Agregar heladas meteorologicas y agronomicas
  geom_point(data = bce_18_19[which(bce_18_19$tmin<1),],
             aes(date, y=0), shape=8, col="blue") +
  geom_point(data = bce_18_19[which(bce_18_19$tmin>0 & bce_18_19$tmin<3.6),],
             aes(date, y=3.5), shape=8, col="chartreuse")+
  annotate("text", x=c(as.Date("2018-09-3"), as.Date("2018-11-10")), y=c(0,3.5),
           label=c("Helada meteorológica","Heladas\nagronómicas"), size=3, hjust=0))

r5 = c(as.Date("2018-12-15"),  as.Date("2019-01-15")); diff(r5)
r6 = c(as.Date("2019-01-15"),  as.Date("2019-03-10")); diff(r6)
midpoint <- function(interval) { min(interval)+(max(interval)-min(interval))/2 }; midpoint(r5)

(p_final <- p2 +
    # barras de floracion y llenado
    annotate("rect", xmin= c(min(r5), min(r6)), xmax=c(max(r5),max(r6)), ymin=0, ymax=30, alpha=0.2, fill=c("grey80","grey60"))+
    annotate("text", x = c(midpoint(r5), midpoint(r6)), y=3,col="red", parse=TRUE, size=3,
             label=c('bold("Floración")','bold("Llenado")')) +
    labs(x=NULL, y=NULL,
         title = "Temperatura media en Balcarce y estadíos reproductivos del girasol",
         subtitle = "- Campaña 2018/19 (línea negra)\n- Serie 1971-2017: rango 80% (banda interna) y medias extremas (bandas externas)",
         caption = "Datos registrados en la estación meteorológica de la EEA INTA Balcarce"))
p_final
# ggsave(file = "plots/bce_temp.png", w=80, h=50, units="mm", dpi=300, scale=2)
