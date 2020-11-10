################################################
# Proyecto Final Diego Ortiz González
# Capítulo VIII: Análisis de Sentimientos con get_sentiments.R

# Directorio de Trabajo: ProyectoFidel
################################################


######### PRIMERA PARTE: preparar el material

library(tidyverse)
library(tidytext)

# vamos a trabajar a partir de los archivos brutos descargados, en la carpeta brutosDescargados
# ¡importante! -> mantener el orden cronológico de los archivos (vease Script_01)


## --> "Limpiar" los discursos brutos: es necesario reorganizar el contenido de los discursos descargados,
# de modo que cada discurso sea una sola cadena de caracteres (como si fuera un solo párrafo). 
## Así, al agrupar los archivos por décadas cada década tendrá tantos índices como discursos agrupe.

discursos_descargados <- list.files("discursos/brutosDescargados")

dir.create("discursos/limpios")

######### este bucle puede tardar unos minutos ######

for (i in 1:length(discursos_descargados)) {
  discurso_limpio <- read_lines(paste("discursos/brutosDescargados",discursos_descargados[i],sep = "/"))
  discurso_limpio <- paste(discurso_limpio, collapse = " ")
  write_lines(discurso_limpio,paste("discursos/limpios/","limpio_",discursos_descargados[i],sep = ""))
}
rm(i,discurso_limpio,discursos_descargados)

### los archivos nuevos deben haberse guardado en discursos/limpios
#### fin de la limpieza de archivos brutos


######### SEGUNDA PARTE: AGRUPACIÓN POR DÉCADAS

# Asociamos cada discurso a una década utilizando tablas:

listado_decadas <- c(rep("Decada_I", length(list.files(path = "discursos/limpios", pattern = "[56][0123456789].txt"))),
                     rep("Decada_II", length(list.files(path = "discursos/limpios", pattern = "7[0123456789].txt"))),
                     rep("Decada_III", length(list.files(path = "discursos/limpios", pattern = "8[0123456789].txt"))),
                     rep("Decada_IV", length(list.files(path = "discursos/limpios", pattern = "9[0123456789].txt"))),
                     rep("Decada_V", length(list.files(path = "discursos/limpios", pattern = "0[012345678].txt"))))

discursos_limpios <- list.files("discursos/limpios")

tablaPorDecadas <- tibble()

for (i in 1:length(discursos_limpios)) {
  discurso <- read_lines(paste("discursos/limpios",discursos_limpios[i],sep = "/"))
  
  decadaTemporal <- tibble(texto = discurso,
                           decada = listado_decadas[i])
  
  tablaPorDecadas <- bind_rows(tablaPorDecadas,decadaTemporal)

}
rm(discurso, i, decadaTemporal)
rm(discursos_limpios,listado_decadas)


# en segundo lugar, crear en el disco duro un archivo txt por cada década con su texto correspondiente

dir.create("discursos/porDecadas")

decadas_Nombre <- c("Decada_I", "Decada_II", "Decada_III", "Decada_IV", "Decada_V")


for (i in 1:length(decadas_Nombre)) {
  temporal <- tablaPorDecadas %>%
    filter(decada == decadas_Nombre[i])
  writeLines(temporal$texto, paste("discursos/porDecadas/", decadas_Nombre[i], "_texto-completo", ".txt", sep = ""))
}

rm(temporal,i)
rm(tablaPorDecadas, decadas_Nombre)
# los archivos nuevos deben haberse guardado en discursos/porDecadas
# fin de la organización por décadas


######### TERCERA PARTE: creación de listas y tablas necesarias para el análisis

# creamos una LISTA en la que cada elemento de la lista es una vector que contiene una década

discursos <- list(read_lines("discursos/porDecadas/Decada_I_texto-completo.txt"),
                  read_lines("discursos/porDecadas/Decada_II_texto-completo.txt"),
                  read_lines("discursos/porDecadas/Decada_III_texto-completo.txt"),
                  read_lines("discursos/porDecadas/Decada_IV_texto-completo.txt"),
                  read_lines("discursos/porDecadas/Decada_V_texto-completo.txt"))


## Creamos un vector con nombres más significativos para cada decada.
titulos <- c("Década 1959-1969","Década 1970-1979","Década 1980-1989","Década 1990-1999","Década 2000-2008")


# creamos una gran tabla llamada serie, donde guardaremos todo

serie <- tibble()

for (i in 1:length(titulos)){
  tabla_temporal <- tibble(texto = discursos[[i]]) %>%  
    unnest_tokens(palabra, texto) %>%
    mutate(decadas = titulos[i]) %>%
    select(decadas, everything())
  serie <- rbind(serie,tabla_temporal)
}


rm(tabla_temporal,i)

######### CUARTA PARTE: Análisis y gráfico de sentimientos

# Estos son los elementos que se necesitan para hacer un análisis de sentimientos en español:
# Lo primero es un listado que nos ofrece el profesor

sentimientos <- readr::read_tsv("https://tinyurl.com/SentiEsp",
                                col_types = "cccn",
                                locale = default_locale())


# después cargamos la función personalizada de usuario: get_sentiments.R
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

# comparamos la tabla serie con la tabla sentimientos:

# comparación con nrc
serie %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)


# un gráfico para ver la valencia emocional temporalmente
# (este gráfico puede tardar en ejecutarse)
serie %>%
  group_by(decadas) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%  # valor de origen = 500
  inner_join(get_sentiments("nrc"))  %>%
  count(decadas, indice = indice , sentimiento) %>%
  ungroup()  %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo, decadas = factor(decadas, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = decadas)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ decadas, ncol = 3, scales = "free_x") +
  ggtitle("Análisis de Sentimientos por Décadas") +
  labs(x = "Progresión temporal",
       y = "Sentimiento Positivo / Negativo")

# Fin del Capítulo VIII
# Este capítulo continúa en el script siguiente
