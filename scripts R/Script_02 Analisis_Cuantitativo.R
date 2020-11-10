###############################################################
# Proyecto Final Diego Ortiz González
# Capítulo II: Análisis Cuantitativo

# Directorio de Trabajo: ProyectoFidel
###############################################################

# Trabajaremos a partir de los textos organizados por años.
# Los archivos por años se crearon en el script_01,
# y deben estar en la carpeta discursos/porAnnos.

### Primera parte: creación de una tabla con todos los textos

library(tidyverse)
library(tidytext)

ficheros <- list.files("discursos/porAnnos")

# listado de años (esta es una manera de hacerlo, hay otras más simples):
annos <- gsub("\\.txt",
                         "",
                        ficheros,
                        perl = TRUE)


# Esta será la tabla donde meteremos todos los textos:

tablaTodosLosDiscursos <- tibble(anno=character(),
                                 parrafo=numeric(),
                                 texto=character())

# bucle para dotar de contenido la tabla:
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("discursos/porAnnos",
                        ficheros[i],
                        sep="/"))
  temporal <- tibble(
    anno=annos[i],
    parrafo=seq_along(discurso),
    texto=discurso)
  
tablaTodosLosDiscursos <- bind_rows(tablaTodosLosDiscursos, temporal)
}
rm(temporal)
rm(discurso)
rm(i)


# Una vez con todos los discursos en la tabla puedo obtener algunos datos cuantitativos:

options(scipen=999) 


# división en palabras token:
TodosLosDiscursosToken <- tablaTodosLosDiscursos %>%
  unnest_tokens(tokens,texto)

# palabras tipo con su frecuencia:
palabrasTipoFrecuencias <- TodosLosDiscursosToken %>%
  count(tokens, sort =TRUE) %>%
  mutate(frecuencia = n / sum(n))

# palabras tipo agrupadas por año, con su frecuencia:
palabrasTipoAgrupadas <- TodosLosDiscursosToken %>%
  group_by(anno) %>%
  count(tokens, sort = TRUE) %>%
  mutate(frecuencia = n / sum(n)) %>%
  ungroup()  ## sigo sin entender este ungroup


### Gráfico: cantidad de palabras agrupadas por año:

TodosLosDiscursosToken %>%
  group_by(anno) %>%
  count() %>%
  ggplot() +
  geom_bar(aes(annos,n),
           stat = "identity",
           fill="red") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Año",
       y = "Número de palabras") +
  ggtitle("Mensajes de Fidel Castro 1959-2008",
          subtitle = "Número de palabras por año")

## fin Capítulo II
       
       
