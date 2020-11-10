#########################################################################
# Proyecto Final Diego Ortiz González
# Capítulo V: PoS Tagging, desambiguación léxica, etiquetado gramatical.

# Directorio de Trabajo: ProyectoFidel
########################################################################


# Instalación de la librería udpipe:
# sólo si no está ya instalada:
#install.packages("udpipe")
library(udpipe)


# modelo de lengua a utilizar.
# Descarga al disco duro:
udpipe_download_model(language = "spanish-ancora")

# Instalación
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.4-190531.udpipe')

library(tidyverse)
library(tidytext)

# He realizado este modelo de análisis por Períodos, como los utilizados en el Capítulo 04
# Para ello, a la gran tabla final creada por udpipe_anotate le añadiré columna período

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

###### ATENCIÓN  ####### ESTE BUCLE PUEDE TARDAR HORAS EN EJECUTARSE.

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
  
  # Esta tabla final recoge las 5 columnas que necesito para hacer los gráficos
  Mensajes_Analizado <- bind_rows(Mensajes_Analizado, analisis)
}

### 3.-. Gráficos

# Tabla comparativa de sustantivos (lematizados) por Períodos
Mensajes_Analizado %>%
  filter(upos == "NOUN") %>%
  group_by(periodo)  %>%
  count(lemma, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  top_n(20) %>%
  ggplot(aes(lemma,frecuencia))+
  geom_col(fill="blue") +
  facet_wrap(~periodo, scales = "free_x") + ## posible free_y
  coord_flip()+
  labs(x = "Sustantivos lematizados",
       y = "Frecuencia") +
  ggtitle("Mensajes de Fidel Castro 1959-2008",
          subtitle = "Sustantivos lematizados por Períodos")


# Tabla comparativa de verbos (sin auxiliares) por Períodos
Mensajes_Analizado %>%
  filter(upos == "VERB") %>%
  group_by(periodo)  %>%
  count(lemma, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  top_n(20) %>%
  ggplot(aes(lemma,frecuencia))+
  geom_col(fill="red") +
  facet_wrap(~periodo, scales = "free_x") + ## posible free_y
  coord_flip()+
  labs(x = "Verbos",
       y = "Frecuencia") +
  ggtitle("Mensajes de Fidel Castro 1959-2008",
          subtitle = "Verbos por Períodos")

## fin Capítulo V

