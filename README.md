# indecopi
---
title: "Indecopi"
author: "Mirian Yanet"
date: "25/10/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggalluvial)
library(parcats)
library(easyalluvial)
library(forcats)
```
Cargamos la data
```{r}
setwd("C:/Users/TOSHIBA/Downloads")
barreras <- readxl::read_xls("Data Logros al 31.12.2017.xls",skip = 1)

sapply(barreras,class)
barreras$`FECHA DE MODIF.` # "POSIXct" "POSIXt"  es un formato especial de fecha, más específico.
table(barreras$`TIPO DE ACTUACIÓN`)
table(barreras$ENTIDAD)
unique(barreras$ENTIDAD)
```
```{r}
barreras$TIPO_ENTIDAD <- "Otros" # creando variables poniendo otros como ejemplo
barreras$TIPO_BARRERA <- "Otros" # creando variables poniendo otros como ejemplo
barreras$TIPO_ADECUACION <- "Otros" # creando variables poniendo otros como ejemplo
names(barreras)

View(barreras)
```
```{r}
# Crear la variable categórica Tipo Entidad para indentificar para hacer análisis
barreras <- barreras %>%
  mutate(TIPO_ENTIDAD = case_when(
    grepl("MUNICIPALIDA", ENTIDAD) ~ "Gobierno Regional",
    grepl("UNIVERSIDAD", ENTIDAD) ~ "Universidad",
    grepl("MINISTERIO", ENTIDAD) ~ "Ejecutivo",
    grepl("ASAMBLEA", ENTIDAD) ~ "Universidad",
    grepl("SEGURO SOCIAL DE SALUD", ENTIDAD) ~ "Otras instituciones del Estado",
    grepl("COLEGIO ODONTOLÓGICO", ENTIDAD) ~ "Colegios profesionales",
    grepl("REGISTRO NACIONAL", ENTIDAD) ~ "Otras instituciones del Estado",
    grepl("SUPERINTENDENCIA", ENTIDAD) ~ "Instituciones autónomas del Estado",
    grepl("PRESIDENCIA DEL CONSEJO", ENTIDAD) ~ "Ejecutivo",
    grepl("AUTORIDAD PORTUARIA", ENTIDAD) ~ "Otras instituciones del Estado",
    grepl("DEFENSORIA DEL", ENTIDAD) ~ "Instituciones autónomas del Estado",
    grepl("SERVICIO NACIONAL", ENTIDAD) ~ "Otras instituciones del Estado",
    
  )
  )
```


```{r}
# corrgiendo un error de la BD por la i minúscula
barreras <- barreras %>%
  mutate(`TIPO DE ACTUACIÓN` = case_when(
    grepl("iNVESTIGACIÓN DE OFICIO", `TIPO DE ACTUACIÓN`) ~ "INVESTIGACIÓN DE OFICIO",
    TRUE~`TIPO DE ACTUACIÓN`
  )
  )

unique(barreras$TIPO_ENTIDAD)
```
### Tipo de barrera

```{r}

# a modo de ejemplo
barreras <- barreras %>%
  mutate(TIPO_BARRERA = case_when(
    grepl("ORDEN", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD == "Gobierno Regional" ~ "Ordenanzas de gobiernos regionales"   
  )
  )

# Versión completa
barreras <- barreras %>%
  mutate(TIPO_BARRERA = case_when(
    grepl("ORDEN", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("RESOLUC", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Universidad") ~ "Resolución universitaria",   
    grepl("DECRE", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("Ordenanza", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("ODENANZA", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("DECRETO SUPREMO", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Ejecutivo") ~ "DECRETO SUPREMO"  ,
    grepl("PORTAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Web"  ,
    grepl("RESOLUCIÓN MINISTERIAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN MINISTERIAL"  ,
    grepl("WEB INSTITUCIONA", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Web"  ,
    grepl("RESOLUCIÓN DEFENSORIAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN DEFENSORIAL"  ,
    grepl("DECRETO SUPREMO", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "DECRETO SUPREMO"  ,
    grepl("RESOLUCIÓN JEFATURAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN JEFATURAL"  ,
    grepl("DIRECTIVA", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Directiva"  ,
    grepl("REGLAMENTO", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Reglamento"  ,
    
    
  )
  )


# el siguiente código me sirve para identificar si me pasó algo.

barreras2 <- barreras %>% filter(TIPO_BARRERA %in% c(NA)) # Limpieza completada
unique(barreras2$ENTIDAD)

# Crear la variable categórica Tipo Adecuación para indentificar para hacer análisis


barreras <- barreras %>%
  mutate(TIPO_ADECUACION = case_when(
    grepl("ORDEN", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("RESOLUC", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Universidad") ~ "Resolución universitaria",   
    grepl("DECRE", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("Ordenanza", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("ODENANZA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("DECRETO SUPREMO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Ejecutivo") ~ "DECRETO SUPREMO"  ,
    grepl("PORTAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Web"  ,
    grepl("RESOLUCIÓN MINISTERIAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN MINISTERIAL"  ,
    grepl("WEB INSTITUCIONA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Web"  ,
    grepl("RESOLUCIÓN DEFENSORIAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN DEFENSORIAL"  ,
    grepl("DECRETO SUPREMO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "DECRETO SUPREMO"  ,
    grepl("RESOLUCIÓN JEFATURAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN JEFATURAL"  ,
    grepl("DIRECTIVA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Directiva"  ,
    grepl("REGLAMENTO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Reglamento"  ,
    grepl("CONCLUSIÓN DEL PROCEDIMIENTO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "CONCLUSIÓN DEL PROCEDIMIENTO"  ,
    grepl("Decreto de Alcaldía", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("RESOLUCIÓN DE GERENCIA GENERAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Reglamento"  ,
    
    
  )
  )

barreras2 <- barreras %>% filter(TIPO_ADECUACION %in% c(NA))
unique(barreras2$ENTIDAD)

# Crear la variable categórica reversión para indentificar si el mismo tipo de norma hace que se normalice

barreras <- barreras %>%
  mutate(REVERSION = case_when(TIPO_ADECUACION==TIPO_BARRERA ~ "SI",
                               TRUE ~ "NO"))

```
```{r}
#graficando

# modo 1
barreras3 <-  barreras %>% select(REVERSION,`TIPO DE ACTUACIÓN`,TIPO_ENTIDAD,TIPO_BARRERA,TIPO_ADECUACION)
barreras3 <- lapply(barreras3,function(x) as_factor(x)) # usamos específicamente lapply por el resultado que te resulta al ser factor
barreras3 <- as.data.frame(barreras3)
p <-  alluvial_wide(barreras3, max_variables = 5)
parcats(p, marginal_histograms = TRUE, data_input = barreras3)

```
```{r}
# modo 2
barreras3 <-  barreras %>% select(`TIPO DE ACTUACIÓN`,TIPO_ENTIDAD,TIPO_BARRERA,TIPO_ADECUACION,REVERSION)
barreras3 <- lapply(barreras3,function(x) as_factor(x)) # usamos específicamente lapply por el resultado que te resulta al ser factor
barreras3 <- as.data.frame(barreras3)
p <-  alluvial_wide(barreras3, max_variables = 5)
parcats(p, marginal_histograms = TRUE, data_input = barreras3)
```
