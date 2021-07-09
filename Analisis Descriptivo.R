#Analisis Descriptivo - Parte 1
library(tidyverse)
library(kableExtra)
library(moments)

listMunicipios <- datosMadrid %>%
  select(municipio_distrito, fecha_informe, casos_confirmados_ultimos_14dias,tasa_incidencia_acumulada_ultimos_14dias) %>%
  filter(fecha_informe > "2020/10/01 00:00")
listMunicipios

#Sacamos datos para analisis de variables de forma individual
datosSemanales <- listMunicipios %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  group_by(fecha_informe) %>%
  summarize(mediaSemanalCasos = mean(casos_confirmados_ultimos_14dias), mediaSemanalTasa = mean(tasa_incidencia_acumulada_ultimos_14dias))
print(datosSemanales)

datosTotalesCasos <- listMunicipios %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  summarize(mediaCasos = mean(casos_confirmados_ultimos_14dias), medianaCasos = median(casos_confirmados_ultimos_14dias), 
  Q1 = quantile(casos_confirmados_ultimos_14dias, c(0.25)), Q2 = quantile(casos_confirmados_ultimos_14dias, c(0.5)), Q3 = quantile(casos_confirmados_ultimos_14dias, c(0.75)), desviacion = sd(casos_confirmados_ultimos_14dias), Kurtosis = kurtosis(casos_confirmados_ultimos_14dias), Asimetria = skewness(casos_confirmados_ultimos_14dias))
datosTotalesCasos

datosTotalesTasa <- listMunicipios %>%
  filter(!is.na(tasa_incidencia_acumulada_ultimos_14dias)) %>%
  summarize(mediaCasos = mean(tasa_incidencia_acumulada_ultimos_14dias), medianaCasos = median(tasa_incidencia_acumulada_ultimos_14dias), 
            Q1 = quantile(tasa_incidencia_acumulada_ultimos_14dias, c(0.25)), Q2 = quantile(tasa_incidencia_acumulada_ultimos_14dias, c(0.5)), Q3 = quantile(tasa_incidencia_acumulada_ultimos_14dias, c(0.75)), desviacion = sd(tasa_incidencia_acumulada_ultimos_14dias), Kurtosis = kurtosis(tasa_incidencia_acumulada_ultimos_14dias), Asimetria = skewness(tasa_incidencia_acumulada_ultimos_14dias))
datosTotalesTasa

datosMunicipios <- listMunicipios %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  group_by(municipio_distrito) %>%
  summarize(mediaSemanalCasos = mean(casos_confirmados_ultimos_14dias), mediaSemanalTasa = mean(tasa_incidencia_acumulada_ultimos_14dias))
print(datosMunicipios)

#calculos para correlacion
barrioPuenteVallecas <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/12/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Madrid-Puente de Vallecas')
print(barrioPuenteVallecas)

barrioArganzuela <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/12/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Madrid-Arganzuela')
print(barrioArganzuela)

ggplot(NULL, aes(x = barrioArganzuela$casos_confirmados_ultimos_14dias, y = barrioPuenteVallecas$casos_confirmados_ultimos_14dias)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

correlacionBarrios <- cor(barrioArganzuela$casos_confirmados_ultimos_14dias, barrioPuenteVallecas$casos_confirmados_ultimos_14dias)
correlacionBarrios

#datos barrios de forma individual
barrio_Puente_Vallecas <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/11/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Madrid-Puente de Vallecas')

barrio_Rozas <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/11/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Las Rozas de Madrid')

barrio_VillanuevaPardillo <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/11/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Villanueva del Pardillo')

barrio_Villaviciosa <- datosMadrid %>%
  select( municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/01 00:00") %>%
  filter(fecha_informe < "2020/11/15 00:00") %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(municipio_distrito == 'Villaviciosa de Odón')

ggplot(barrio_Puente_Vallecas, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias)) + geom_col()
ggplot(barrio_Rozas, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias)) + geom_col()
ggplot(barrio_VillanuevaPardillo, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias)) + geom_col()
ggplot(barrio_Villaviciosa, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias)) + geom_col()

#creación tabala con todos los datos
listMunicipios %>%
  kbl() %>%
  kable_styling()

#parte de boxplot

datosBox <- listMunicipios %>%
  select(municipio_distrito, casos_confirmados_ultimos_14dias, fecha_informe) %>%
  filter(fecha_informe > "2020/10/15 00:00") %>%
  filter(fecha_informe < "2020/12/15 00:00") %>%
  group_by(municipio_distrito)

ggplot(datosBox, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias)) + geom_boxplot()

#parte histograma

datosHisto <- datosMadrid %>%
  select(municipio_distrito, fecha_informe, casos_confirmados_ultimos_14dias) %>%
  filter(!is.na(casos_confirmados_ultimos_14dias)) %>%
  filter(fecha_informe > "2020/10/15 00:00") %>%
  filter(fecha_informe < "2020/11/15 00:00") %>%
  group_by(fecha_informe)
datosHisto

ggplot(datosHisto, aes(x = casos_confirmados_ultimos_14dias)) + geom_histogram(binwidth = 100)
  
#graficas en versiones anteriores
#agrupar por meses o similar(municipios), uno para boxplot y otro histograma
#ggplot(listMunicipios, aes(x = casos_confirmados_ultimos_14dias, color = fecha_informe)) + geom_histogram(binwidth = 200)
#ggplot(listMunicipios, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias, color = fecha_informe)) + geom_boxplot()
#representar algo util, si no se puede borrar
#ggplot(listMunicipios, aes(x = fecha_informe, y = casos_confirmados_ultimos_14dias, color = fecha_informe)) + geom_col()
#cambiar la representacion a barrios contiguos y tabien el coeficioente calculado arriba
#ggplot(datosMadrid, aes(x = casos_confirmados_totales, y = tasa_incidencia_acumulada_total)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
