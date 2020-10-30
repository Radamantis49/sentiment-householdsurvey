library(tidyverse)
library(devtools)
library(tm)
library(NLP)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidytext)
library(openxlsx)

setwd("/home/morracos/Documentos/R_scraping/GEIH_SA")

lexico <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1")

#Ocupados
ocupados <- read.table('ocupados_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", fill = TRUE)
ocupados <- 
  ocupados %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  filter(!Pregunta == "ASALARIADOS" & !Pregunta == "INDEPENDIENTES" & !Pregunta == "SOLO REALICE ESTA PREGUNTA, SI I_32 ES MENOR A I_30" & !Pregunta == "TOTAL OCUPADOS (ASALARIADOS, INDEPENDIENTES Y TRABAJADORES SIN REMUNERACION)") %>%
  cbind(Numero_pregunta=c(1,2,"2A",3:7,"7A",8:13,"13A",14:23,"23A","23B","23C",24,25,"25A","25B","25C",26,27,"27A",28,"28A","28R",29:42,"42A",43:46,"46A",47,"47A",48:64,"64A","64B",65),.) %>%
  cbind(Categoria=c("Ocupados"),.)

#Desocupados
desocupados <- read.table('desocupados_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
desocupados <- 
  desocupados %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1,2,"2A",3,"4A",5:12,"12A","12B",13,14),.) %>%
  cbind(Categoria=c("Desocupados"),.)

#Educacion
educacion <- read.table('educacion_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
educacion <- 
  educacion %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:6),.) %>%
  cbind(Categoria=c("Educacion"),.)

#Fecundidad
fecundidad <- read.table('fecundidad_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
fecundidad <- 
  fecundidad %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:3),.) %>%
  cbind(Categoria=c("Fecundidad"),.)

#Formacion_trabajo
formacion_trabajo <- read.table('formacion_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
formacion <- 
  formacion_trabajo %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:16),.) %>%
  cbind(Categoria=c("Formacion_trabajo"),.)

#Fuerza_trabajo
fuerza_trabajo <- read.table('fuerza_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", fill = TRUE)
fuerza_trabajo <- select(fuerza_trabajo, V1)
fuerza <- 
  fuerza_trabajo %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:13),.) %>%
  cbind(Categoria=c("Fuerza_trabajo"),.)

#Inactivos
inactivos <- read.table('inactivos_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
inactivos <- 
  inactivos %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1,"1A","1B","1C", 2:8,"8A","9A",10),.) %>%
  cbind(Categoria=c("Inactivos"),.)

#Micronegocios
micronegocios <- read.table('micronegocios_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
micronegocios <- 
  micronegocios %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:8),.) %>%
  cbind(Categoria=c("Micronegocios"),.)

#Migracion
migracion <- read.table('migracion_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
migracion <- 
  migracion %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=c(1:7),.) %>%
  cbind(Categoria=c("Migracion"),.)

#Vivienda
vivienda <- read.table('vivienda_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", fill = TRUE)
vivienda <- select(vivienda, V1)
vivienda <- 
  vivienda %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta=paste(c(1:5,1:15), c("H")), stringsAsFactors = FALSE) %>%
  cbind(Categoria=c("Vivienda"),.)
  
vivienda[5,3] <- "5V"
vivienda[4,3] <- "4V"
vivienda[3,3] <- "3V"
vivienda[2,3] <- "2V"
vivienda[1,3] <- "1V"

#Seguridad
seguridad <- read.table('seguridad_preguntas.txt', sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
seguridad <- 
  seguridad %>%
  rename("Pregunta"= V1) %>%
  filter(Pregunta > 0) %>%
  cbind(Numero_pregunta= c(1:7),.) %>%
  cbind(Categoria=c("Seguridad"),.)

#Merge
merge <- merge(vivienda, fuerza, all = TRUE)
merge_2 <- merge(desocupados, educacion, all = TRUE)
merge_3 <- merge(fecundidad, formacion, all = TRUE)
merge_4 <- merge(inactivos, migracion, all = TRUE)
merge_5 <- merge(ocupados, seguridad, all = TRUE)
merge_6 <- merge(merge, merge_2, all = TRUE)
merge_7 <- merge(merge_3, merge_4, all = TRUE)
merge_8 <- merge(merge_6, merge_7, all = TRUE)
merge_9 <- merge(merge_8, merge_5, all = TRUE)


#SENTIMENT (con AFINN)
sa_geih <- 
  merge_9 %>%
  unnest_tokens (input = "Pregunta", output = "Palabra") %>%
  inner_join(lexico, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion==0, "Neutro", ifelse(Puntuacion > 0, "Positiva", "Negativa")))

merge_9_final <-
  sa_geih %>% 
  group_by(Numero_pregunta) %>%
  summarise(Puntaje_pregunta = mean(Puntuacion)) %>%
  left_join(merge_9, ., by = "Numero_pregunta") %>%
  mutate(Puntaje_pregunta = ifelse(is.na(Puntaje_pregunta), 0, Puntaje_pregunta))

merge_9_final <- 
  merge_9_final %>%
  mutate(Tipo = ifelse(Puntaje_pregunta == 0, "Neutro", ifelse(Puntaje_pregunta > 0, "Positiva", "Negativa")))
  
map(c("Positiva", "Negativa", "Neutro"), function(sentimiento) {
  sa_geih %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Categoria) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 5, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Categoria) +
    geom_col() +
    facet_wrap("Categoria", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento)
})

merge_9_final%>%
  count(Categoria, Tipo) %>%
  group_by(Categoria) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Categoria, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "top")

#Densidad neutros

merge_9_final %>%  
  ggplot() +
  aes(Puntaje_pregunta, color = Categoria) +
  geom_density() +
  facet_wrap(~Categoria)

#Exportar

write_excel_csv(ocupados, "/home/morracos/Documentos/R_scraping/GEIH_SA/Ocupados.csv", delim = ",")
