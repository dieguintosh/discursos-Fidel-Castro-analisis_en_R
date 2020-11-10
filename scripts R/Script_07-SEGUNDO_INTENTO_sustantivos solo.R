################################################
# Proyecto Final Diego Ortiz González
# Capítulo VII: Topic Modeling. SEGUNDO INTENTO

# Directorio de Trabajo: ProyectoFidel
################################################


######### SEGUNDO INTENTO #########################################################
## Topic Modeling a partir de discursos en los que sólo hay sustantivos lematizados.
## Textos organizados por períodos.
####################################################################################


#####
### PRIMERO: Análisis Pos Tagging con udpipe para obtener los sustantivos lematizados
#####


# Instalación de la librería udpipe:
# sólo si no está ya instalada:
#install.packages("udpipe")
library(udpipe)


#### -> solamente si no se ha hecho en el script anterior:
# modelo de lengua a utilizar --> Descargar al disco duro:
# udpipe_download_model(language = "spanish-ancora")

# Instalación
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.4-190531.udpipe')


library(tidyverse)
library(tidytext)



### 1.- Tabla inicial con todos los discursos (como en el Capítulo 04) añadiéndole la columna Período 

ficheros <- list.files("discursos/porAnnos/")

anno <- as.character(1959:2008)

Periodos <- c(rep("Periodo_I",32), 
              rep("Periodo_II", 10), 
              rep("Periodo_III", 8))

# la tabla que recogerá todo el material
mensajes <- tibble(anno = character(),
                   periodo = character(),
                   texto = character())  

# creación de la tabla mensajes, con una serie de instrucciones para eliminar rayas indeseadas:
for (i in 1:length(ficheros)){
  discurso <- read_lines(paste("discursos/porAnnos",
                               ficheros[i],
                               sep = "/"))
  # Las líneas que se encargan de las rayas
  discurso <- gsub("[-–—]", " – ", discurso)
  discurso <- gsub(" ([\\.,;:])", "\\1", discurso)
  discurso <- gsub("  ", " ", discurso)
  discurso <- gsub("^ ", "", discurso)
  # Sigue…
  temporal <- tibble(anno = anno[i],
                     periodo = Periodos[i],
                     texto = discurso)
  mensajes <- bind_rows(mensajes, temporal)
}
rm(temporal,discurso,i)


### 2.- función udpipe_anotate

# A continuación comenzamos a preparar el bucle donde se ejecutará la función udpipe_anotate:

AnnoMensaje <- anno  
PeriodosMensaje <- Periodos  

# tabla vacía que recogerá el análisis final
Mensajes_Analizado <- tibble()


# bucle que ejecuta la función udpipe_anotate en toda la tabla "mensajes".

###### ATENCIÓN  ####### -->  ESTE BUCLE TARDA UNAS 3 HORAS EN EJECUTARSE.

for (i in 1:length(AnnoMensaje)) {
  temporal <- mensajes %>%
    filter(anno==AnnoMensaje[i]) %>%
    select(texto)
  analisis <- as_tibble(udpipe_annotate(modelo_ancora,
                                        temporal$texto))
  analisis <- analisis %>%
    add_column(periodo = PeriodosMensaje[i],
               anno=AnnoMensaje[i]) %>%
    select(
      -(doc_id),
      -(paragraph_id),
      -(sentence_id),
      -(sentence),
      -(token_id),
      -(head_token_id),
      -(dep_rel),
      -(deps),
      -(misc),
      -(feats),
      -(xpos))
  
  Mensajes_Analizado <- bind_rows(Mensajes_Analizado, analisis)
}
rm(temporal, i)
rm(mensajes, anno, AnnoMensaje, ficheros, Periodos, PeriodosMensaje)


# exportar los sustantivos lematizados de la tabla Mensajes_Analizado por periodos a archivos txt 

dir.create("discursos/SustantivosPorPeriodo")

periodoValor <- c("Periodo_I","Periodo_II","Periodo_III")

for (i in 1:length(periodoValor)) {
temporal <- Mensajes_Analizado %>%
  filter(periodo == periodoValor[i]) %>%
  filter(upos == "NOUN")

  write_lines(temporal$lemma, 
              paste("discursos/SustantivosPorPeriodo/", 
                    paste(periodoValor[i],"NOUN",sep = "_"),
                    ".txt" ,sep = ""))
}
rm(i,temporal)
rm(periodoValor)

# con esto ya tengo guardados tres archivos txt solamente con los sustantivos lematizados,
# uno por período, en la carpeta discursos/SustantivosPorPeriodo:
# -> Periodo_I_NOUN.txt
# -> Periodo_II_NOUN.txt
# -> Periodo_III_NOUN.txt


#####
### SEGUNDO: Topic modeling a partir de los archivos anteriores ###################
##### 
  
  
#librerías a instalar solo una vez
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("scales")
  
# cargando librerías
  library(tm)
  library(topicmodels)
  library(scales)

  
# cargamos nuestras listas de palabras vacías
# Lista de palabras vacías aportada por el profesor (en la carpeta Palabras_Vacias del directorio de trabajo):
vacias <- read_csv("https://tinyurl.com/VaciasEsp",
                   locale = default_locale())
  
vacias_fidel <- tibble(palabra = c("discurso",
                                     "pronunciado",
                                     "comandante",
                                     "fidel",
                                     "castro",
                                     "ruz",
                                     "departamento",
                                     "versiones",
                                     "taquigráficas",
                                     "taquigraficas",
                                     "aplausos",
                                     "aplauso",
                                     "prolongados",
                                     "prolongado",
                                     "exclamaciones",
                                     "abucheos",
                                     "ovación",
                                     "ovacion",
                                     "primer",
                                     "secretario",
                                     "primer",
                                     "ministro",
                                     "consejo",
                                     "000"
  ))

# los ficheros de cada período con sustantivos
ficheros_NOUN <- list.files("discursos/SustantivosPorPeriodo")

# los 3 períodos
periodos <- c("Periodo I","Periodo II","Periodo III")   


## Generación de la Matriz DTM:

# 1º.- Divivir en páginas

TODOS_Los_Discursos <- tibble(texto = character(),
                              periodo = character(),
                              pagina = numeric())


for (j in 1:length(ficheros_NOUN)) {
  
  texto.entrada <- read_lines(paste("discursos/SustantivosPorPeriodo",
                                    ficheros_NOUN[j],
                                    sep = "/"),
                              locale = default_locale())
  
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  
  
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              periodo = periodos[j],
                              pagina = i)
    TODOS_Los_Discursos <- bind_rows(TODOS_Los_Discursos, fragmento.unido)
  }
}

rm(trozos, fragmento,
   fragmento.unido, texto.entrada,
   texto.palabras, texto.todo, por.palabras, i,j)

# 2º.- Dividir en tokens, PERO con una clave para saber a qué página y qué período corresponde cada token

por_pagina_palabras <- TODOS_Los_Discursos %>%
  unite(periodo_pagina, periodo, pagina) %>%
  unnest_tokens(palabra, texto)

# vaciar y contabilizar con count
palabra_conteo <- por_pagina_palabras %>%
  anti_join(vacias) %>%
  anti_join(vacias_fidel) %>% 
  count(periodo_pagina, palabra, sort = T) %>%
  ungroup()  # sigo sin entender este ungroup()

## 3º.-Creación de la Matriz DTM

paginas_dtm <- palabra_conteo %>%
  cast_dtm(periodo_pagina, palabra, n)

## 4º Crear Modelo LDA

# ATENCIÓN: esta orden tarda varios minutos en ejecutarse
paginas_lda <- LDA(paginas_dtm, k = 4, control = list(seed = 1234)) 
# tras varias pruebas, decido elegir un valor de k=4 para trabajar con 4 tópicos


#### Extrayendo los datos a tablas y gráficos:

# calcular la probabilidad de que cada palabra pertenezca a uno u otro tópico y verlo en una tabla
options(scipen=999) 
paginas_lda_td <- tidy(paginas_lda, matrix = "beta")

# ver los 10 términos más frecuentes en cada topic
terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# y el gráfico
terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=2, scales = "free") +
  ggtitle("Todos los discursos de 1959 a 2008 (sólo sustantivos lematizados)",
          subtitle = "Topic Modeling sobre k=4 topics") +
  labs(x = "SUSTANTIVOS",
       y = "VALOR beta") +
  coord_flip()

## fin capítulo VII Segundo Intento
