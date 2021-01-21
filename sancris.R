if (!require("pacman")) install.packages("pacman")
pacman::p_load(sfthemes,devtools,pheatmap,curl, tidyverse, here, janitor, skimr, readxl,lubridate,tidylog,vtable, data.table,visdat,naniar,simputation)
# Read the csv file from the Junta de Castilla y León website:----

#zbs <- read.csv('https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/tasa-coronavirus-zonas-basicas-salud/1284942912395.csv',
#              sep = ";")
# the read.csv method is too slow, lets use the fread method of the data.table package:

# Read and clean keys of municipalities names:-----
munic_sego <- fread('https://analisis.datosabiertos.jcyl.es/explore/dataset/indicadores-de-riesgo-covid-19-por-municipios/download/?format=csv&timezone=Europe/Berlin&lang=es&use_labels_for_header=true&csv_separator=%3B',
             sep = ";", encoding="UTF-8")

munic_sego <- clean_names(munic_sego)
vtable(munic_sego, labels = c('fecha', 'Código INE Municipio','Nombre del Municipio','Provincia','Casos en la semana previa',
                              'IA14 del municipio Publicada en el BOE','Valoración de la IA14 por parte del BOE','IA7 del municipio acorde con el BOE','Valoración de la IA7 del municipio por parte del BOE',
                              'IA14 del municipio Publicada en el BOE para la población mayor de 65','Valoración de la IA14 por parte del BOE para la población mayor de 65','IA7 del municipio Publicada en el BOE para la población mayor de 65',
                              'Valoración de la IA7 por parte del BOE para la población mayor de 65','Positividad en el municipio','Valoración de la positividad',
                              'Porcentaje de trazabilidad','Valoración del porcentaje de trazabilidad','geo_point','geo_shape'))




# Read data at the province level:----

province_level <- fread('https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv',
                    sep = ",", encoding="UTF-8")

province_level <- clean_names(province_level) %>% 
  filter(ccaa == 'Castilla y León')


# Read data at the regional level: ----

region_level <- fread('https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-ccaa-spain_consolidated.csv',
                      sep = ",", encoding="UTF-8")

region_level <- clean_names(region_level) %>% 
  filter(ccaa == 'Castilla y León')



# Join province with region data:----

# In this case, since the key names are the same, we are going to introduce a 
# suffix to the region data:
ccaa_join <- province_level %>%  left_join(region_level %>% rename_all( list(~paste0(., ".cyl"))) ,
                                           by=c('date'='date.cyl'))




# Join last outline with municipality data:----

munic_join <- munic_sego %>% 
  left_join(ccaa_join, by = c('fecha' = 'date', 'provincia' = 'province'))

# it is not an exact join since the municipalities data starts in November.

munic_join_tbl <- munic_join %>% as.tibble()





# SANCRIS:----

sancris <- munic_join_tbl %>%
  filter(municipio =='San Cristóbal de Segovia')

theme_set(sfthemes:: theme_sf_light())

sancris <- sancris %>%
  mutate(fecha = as.Date(fecha, "%Y%b%d" )) %>% 
  select(fecha,municipio, provincia, ccaa,ia14_boe,ia14,ia14.cyl)


sancris %>% 
  na.omit() %>% 
  ggplot(aes(fecha,ia14_boe, col=municipio))+
  geom_line()+
  geom_line(aes(fecha, ia14, col=provincia))+
  geom_line(aes(fecha, ia14.cyl, col=ccaa))+
  theme_sf_light(size_class = "Medium")+
  scale_colour_ios_light(accessible = F)+
  scale_fill_ios_light(accessible = F)+
  theme(legend.title = element_blank(),axis.text.x=element_text(angle=90, hjust=1))


