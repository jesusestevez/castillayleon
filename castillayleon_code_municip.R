if (!require("pacman")) install.packages("pacman")
pacman::p_load(pheatmap,curl, tidyverse, here, janitor, skimr, readxl,lubridate,tidylog,vtable, data.table,visdat,naniar,simputation)

# Read the csv file from the Junta de Castilla y León website:----

#zbs <- read.csv('https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/tasa-coronavirus-zonas-basicas-salud/1284942912395.csv',
#              sep = ";")
# the read.csv methos is too slow, lets use the fread method of the data.table package:

zbs <- fread('https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/tasa-coronavirus-zonas-basicas-salud/1284942912395.csv',
                sep = ";", encoding="UTF-8")
# Recall that we have to encode it as utf-8 in order to be able to read los acentos



# Save the data into an RDS format to read it faster in the future----
saveRDS(zbs,'zbs.rds')

zbs <-readRDS('zbs.rds')



# Describe the data and harmonize keys:----

vtable(zbs)
# as we can see, we have to clean the keys, lets use the janitor package:
zbs <- clean_names(zbs)

# Missing values:----

zbs_tbl <- zbs %>% as_tibble()

zbs %>% vis_dat(warn_large_data = F) # here we can observe where the nas are

zbs %>% vis_miss(warn_large_data = F)

# Therefore, those rows entirely with nas will be dropped

zbs <- zbs %>% select(-c(sospecha_transmision_comunitaria,column_29,column_30,
                         column_31))
zbs %>% vis_miss(warn_large_data = F)

zbs_tbl <- zbs %>% as_tibble()

# Great! 

# Segovia ----
zbs_sego <- zbs_tbl %>% filter(provincia =='Segovia') 

zbs_sego_rural <- zbs %>%  filter(centro=='C.S. SEGOVIA RURAL')


# PLOT ----


theme_set(hrbrthemes::theme_ipsum())

zbs_sego %>%
  na.omit() %>% 
  rename(IA14x10000=tasapcr_positivos_sintoma_sx10000_14dias) %>% 
  ggplot(aes(fecha,IA14x10000, col=fct_reorder2(centro, fecha,IA14x10000 )))+
  geom_line()

# HEATMAP----

# con esto podemos obtener los datos en formato semanal, lo que hace que
# la visualización gane calidad.
# Source: https://www.statology.org/aggregate-daily-data-in-r/
zbs_sego$week <- floor_date(zbs_sego$fecha, "week")


zbs_sego %>%
  na.omit() %>% 
  rename(IA14x10000=tasapcr_positivos_sintoma_sx10000_14dias) %>% 
  ggplot(aes(week,centro, fill=IA14x10000))+
  geom_tile(col='black')+
  scale_fill_gradient(low = "blue", high = "yellow", limits=c(0, 392 ))+
  labs(
    title = "IA 14 por cada 10.000 hab. según registros en Centros de Salud de Segovia provincia",
    x = "Fecha", y = "Centro",
    caption = "Fuente: Junta de Castilla y León. Autor: Jesús Estévez Sánchez"
  ) +
  theme(legend.title = element_blank()) ## Switch off legend title

