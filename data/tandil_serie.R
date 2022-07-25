pacman::p_load(tidyverse, lubridate, zoo)

serie_tandil0 <- read_table2("~/Documents/00_data_analysis/datasets/meteo_tandil/tandil.dat", 
                             col_names = FALSE)
serie_tandil <- serie_tandil0 %>% 
  rename(year=X1, julian=X2, tmax=X3, tmin=X4, rain=X5, hr=X6)   

serie_tandil <- transform(serie_tandil, date = as.Date(paste0(year, "-1-1")) + julian - 1)  

serie_tandil <- serie_tandil %>% 
  mutate(month=lubridate::month(as.Date(date)))

serie_tandil %>% 
  filter(year == 2020)

library(readxl)
tandil_2020_21 <- read_excel("~/Documents/00_data_analysis/datasets/meteo_tandil/TANDIL 2020 21.xlsx") %>% rename(date=Fecha, tmax="T.Max.", tmin="T.Min.", rain=Pp, hr=Humedad)   %>% 
  mutate(date=dmy(date), 
         year=year(date),
         month=month(date),
         julian=yday(date))

tandil_2020_21 %>% write_sheet(ss=tan, sheet = "weather")

tan20 <- read_sheet(tan, sheet = "weather") 

# tandil_2020_21 %>% 
#   filter(year == 2021)

tandil_serie_71_21 <- serie_tandil %>% 
  filter(!year == 2021) %>% 
  filter(!year == 2020) %>% 
  bind_rows(tan20)  



tandil_serie_71_21 %>% distinct(year) %>% pull()
tandil_serie_71_21 %>% count(year)

serie_full <- tandil_serie_71_21 %>% 
  mutate(tmin=replace(tmin, tmin<(-10), NA)) %>% 
  mutate(tmin = zoo::na.approx(tmin)) %>% 
  mutate(tmax=replace(tmax, tmax>40, NA)) %>% 
  mutate(tmax=replace(tmax, tmax<0, NA)) %>% 
  mutate(tmax = zoo::na.approx(tmax)) 

serie_full%>%
ggplot()+
  aes(date, tmax)+
  geom_line()+
  facet_wrap(~year, scales = "free")+
  theme_void()+
  geom_hline(yintercept = 0)
  

save(serie_full, file ="data/serie_tandil.RData" )

