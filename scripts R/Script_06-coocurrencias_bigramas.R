#########################################################################
# Proyecto Final Diego Ortiz González
# Capítulo VI: Colocación y coocurrencia. Redes Léxicas.

# Directorio de Trabajo: ProyectoFidel
########################################################################


#####
##### PRIMERA PARTE: análisis de n-gram: bigramas (colocaciones)
#####


library(tidyverse)
library(tidytext)


### 1.- Reconstruimos la tabla con todos los textos, con la columna períodos

ficheros <- list.files("discursos/porAnnos", pattern=".txt")

anno <- as.character(1959:2008)

Periodos <- c(rep("Periodo_I", 32),rep("Periodo_II",10),rep("Periodo_III",8))

mensajes <- tibble(anno = character(),
                   periodo = character(),
                   texto = character()) 

for(i in 1:length(ficheros)) {
  discurso <- read_lines(paste("discursos/porAnnos",ficheros[i],sep="/"))
  
  temporal <- tibble (anno=anno[i],
                      periodo=Periodos[i],
                      texto=discurso)
  
  mensajes <- bind_rows(mensajes,temporal)
}

# conversión de las columnas anno y periodo en factor
mensajes$anno <- factor(mensajes$anno)
mensajes$periodo <- factor(mensajes$periodo, levels = c("Periodo_I","Periodo_II","Periodo_III"))

rm(discurso, temporal, i)

### 2.- Cargamos listados de palabras vacías para vaciar bigramas (cuando toque)

vacias <- read_csv("https://tinyurl.com/VaciasEsp",
                   locale = default_locale())

vacias_fidel <- tibble(palabra= c("discurso",
                                  "pronunciado",
                                  "comandante",
                                  "fidel",
                                  "castro",
                                  "ruz",
                                  "departamento",
                                  "version",
                                  "versión",
                                  "versiones",
                                  "taquigráficas",
                                  "taquigrafica",
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

### 3.- Crear bigramas

mensajes_bigramas <- mensajes %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

# contarlos:
mensajes_bigramas %>%
  count(bigrama, sort = T)

# Limpieza de palabras vacías en los bigramas

# a) separar bigramas y ponerlos en paralelo en columnas separadas
bigramas_separados <- mensajes_bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ")

# b) borrar las vacías con %in%
bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra,
         !palabra1 %in% vacias_fidel$palabra,
         !palabra2 %in% vacias_fidel$palabra)

# c) Volvemos a unir bigramas
bigramas_unidos <- bigramas_filtrados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# ya tenemos todos los bigramas sin palabras vacías y asociados a su Período correspondiente


# gráfico comparativo de aparición absoluta de bigramas por Período
bigramas_unidos %>%
  count(periodo, bigrama, sort = T) %>%
  group_by(periodo) %>%
  top_n(15) %>%
  ggplot() +
  geom_col(aes(y = n , x = reorder(bigrama,n)),
           fill = "maroon") +
  coord_flip() +
  facet_wrap(~ periodo, ncol = 3, scales = "free_x") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia absoluta") + 
  ggtitle("Frecuencia absoluta de bigramas en cada Período")


# gráfico comparativo de frecuencia relativa de bigramas por Período
# 1º.- Tabla de frecuencias relativas
tabla_frecuencias <- bigramas_unidos %>%
  group_by(periodo) %>%
  count(bigrama, sort = T) %>%
  transmute(bigrama, frecRelativa=n/sum(n)) 


# 2º.- gráfico comparativo de frecuencia relativa de bigramas por Período
tabla_frecuencias %>%
  group_by(periodo) %>%
  filter(frecRelativa>0.0008)%>%
  ggplot() +
  geom_col(aes(x = reorder(bigrama,frecRelativa), y = frecRelativa),
    fill = "darkgreen") +
  coord_flip() +
  facet_wrap(~ periodo, ncol = 3, scales = "fixed") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia relativa") + 
  ggtitle("Frecuencia relativa de bigramas en cada Período")



#####
##### SEGUNDA PARTE: grafos a partir de bigramas
#####

# Instalar paquetes solo una vez
#install.packages(c("igraph", "ggraph"))

library(igraph)
library(ggraph)
library(grid)

# vamos a usar la tabla donde teníamos los bigramas limpios y en DOS columnas: bigramas_filtrados
recuento_bigramas <- bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

# El grafo se genera así:
# 1.- una tabla que crea las relaciones entre elementos
grafo_bigramas <- recuento_bigramas %>%
  filter(n > 300) %>%  ## este es el dato que hay que valorar
  graph_from_data_frame()   # librería igraph

# 2.- dibujar la tabla
ggraph(grafo_bigramas, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = arrow(type = "closed",
                               length = unit(3, "mm"))) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()




#####
##### TERCERA PARTE: Coocurrencias
#####

library("widyr")

# Vamos a trabajar sobre la tabla "mensajes" que recoge todos los discursos y la columna periodos.
# En caso de que se haya borrado, se puede reconstruir con este código:

mensajes <- tibble(anno = character(),
                   periodo = character(),
                   texto = character()) 

for(i in 1:length(ficheros)) {
  discurso <- read_lines(paste("discursos/porAnnos",ficheros[i],sep="/"))
  temporal <- tibble (anno=anno[i],
                      periodo=Periodos[i],
                      texto=discurso)
  mensajes <- bind_rows(mensajes,temporal)
}
mensajes$anno <- factor(mensajes$anno)
mensajes$periodo <- factor(mensajes$periodo, levels = c("Periodo_I","Periodo_II","Periodo_III"))
rm(discurso, temporal, i)


# A continuación buscamos las coocurrencias

#aquí preparamos los mensajes para procesar:
# -> añadiendo un número a cada fila con row_number
# -> haciendo unnest token
# -> vaciando
mensajes_procesar <- mensajes %>%
  mutate(seccion = row_number()) %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% vacias$palabra) %>%
  filter(!palabra %in% vacias_fidel$palabra)


# función pairwise_count de la librería widyr para encontrar coocurrencias
pares_palabras <- mensajes_procesar %>%
  pairwise_count(palabra,
                 seccion,
                 sort = T)

# con este script puedo ir al detalle de cuáles son las palabras que coocurren junto 
# a la palabra que yo le indique.
# Por ejemplo: "revolución"
pares_palabras %>%
  filter(item1 == "revolución")

# Por ejemplo: "patria"
pares_palabras %>%
  filter(item1 == "patria")




# y ahora vamos a ver correlaciones
# partimos de la misma tabla "mensajes_procesar"
# usamos la función pairwise_cor
palabras_correlacion <- mensajes_procesar %>%
  group_by(palabra) %>%
  filter(n() >= 500) %>%  ## este dato ha sido obtenido tras hacer algunas pruebas.
  pairwise_cor(palabra,
               seccion,
               sort = TRUE)

# Ahora podemos comprobar también cómo las correlaciones de algunas palabras:
palabras_correlacion %>%
  filter(item1 == "revolución")

palabras_correlacion %>%
  filter(item1 == "patria")

palabras_correlacion %>%
  filter(item1 == "pueblo")


# A continuación establecemos un gráfico de correlaciones con algunas palabras seleccionadas
palabras_correlacion %>%
  filter(item1 %in% c("revolución",
                      "patria",
                      "crisis",
                      "pueblo",
                      "país",
                      "trabajo")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation,fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  ggtitle("Gráfica de correlaciones")+
  labs(x = "",
       y = "")

# Y otro gráfico con otras palabras
palabras_correlacion %>%
  filter(item1 %in% c("muerte",
                      "guerra",
                      "revolucionario",
                      "hombre",
                      "educación",
                      "mujer")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation,fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  ggtitle("Gráfica de correlaciones (Versión 2)")+
  labs(x = "",
       y = "")


## fin Capítulo VI