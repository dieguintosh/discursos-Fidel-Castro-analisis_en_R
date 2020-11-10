################################################
# Proyecto Final Diego Ortiz González
# Capítulo VII: Topic Modeling. PRIMER INTENTO

# Directorio de Trabajo: ProyectoFidel
################################################


#########  PRIMER INTENTO ########################
## Topic Modeling a partir de los textos brutos.
## Textos organizados por Periodos
###################################################


#####
### PRIMERO: Agrupación de los textos por períodos.
# Voy a crear tres archivos txt, cada uno agrupará todos los discursos en bruto de ese período

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

### 2.- Exportar los textos por periodos a archivos txt 

dir.create("discursos/PorPeriodo")

periodoValor <- c("Periodo_I","Periodo_II","Periodo_III")

for (i in 1:length(periodoValor)) {
temporal <- mensajes %>%
    filter(periodo == periodoValor[i])
write_lines(temporal$texto, 
              paste("discursos/PorPeriodo/", 
                    paste(periodoValor[i],"completo",sep = "_"),
                    ".txt" ,sep = ""))
}
rm(i,temporal)
rm(periodoValor)

# con esto ya tengo guardados tres archivos txt, cada uno con todo el texto de cada período
# en la carpeta discursos/PorPeriodo:
# -> Periodo_I_completo.txt
# -> Periodo_II_completo.txt
# -> Periodo_III_completo.txt

##################################### 

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

# los ficheros de cada período
ficheros_Periodo <- list.files("discursos/PorPeriodo")

# los 3 períodos
periodos <- c("Periodo I","Periodo II","Periodo III")   


## Generación de la Matriz DTM:

# 1º.- Dividir en páginas (es necesario para este análisis)

TODOS_Los_Discursos <- tibble(texto = character(),
                              periodo = character(),
                              pagina = numeric())


for (j in 1:length(ficheros_Periodo)) {
  
  texto.entrada <- read_lines(paste("discursos/PorPeriodo",
                                    ficheros_Periodo[j],
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
  ggtitle("Tópicos a partir de los textos en bruto",
          subtitle = "Topic Modeling sobre k=4 topics") +
  labs(x = "SUSTANTIVOS",
       y = "VALOR beta") +
  coord_flip()


# Fin del Capítulo VII Primer Intento
# Este capítulo sigue en el script siguiente: Segundo Intento.