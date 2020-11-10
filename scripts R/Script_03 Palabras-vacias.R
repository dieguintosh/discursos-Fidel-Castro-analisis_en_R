###############################################################
# Proyecto Final Diego Ortiz González
# Capítulo III: Palabras vacías

# Directorio de Trabajo: ProyectoFidel
###############################################################


library(tidyverse)
library(tidytext)

### 1.- Rehaciendo la tabla de todos los discursos:

annos <- as.character(1959:2008)

ficheros <- list.files(path = "discursos/porAnnos",
                       pattern = "*.txt") # este pattern no haría falta en este caso


tablaTodosLosDiscursos <- tibble(anno=character(),
                                 parrafo=numeric(),
                                 texto=character())

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

# Obtenemos palabras token:
TodosLosDiscursosToken <- tablaTodosLosDiscursos %>%
  unnest_tokens(tokens,texto)

### 2.- Borrado de palabras vacías

# Voy a utilizar dos listas de palabras vacías: la aportada por el Profesor y una tabla
# creada por mí para borrar algunas palabras que se repiten y son anotaciones del transcriptor del discurso.

# Lista de palabras vacías aportada por el profesor
vacias <- read_csv("https://tinyurl.com/VaciasEsp",
                   locale = default_locale())

# tengo que rehacer el renombre de la columna, pues el profesor está usando "palabra" y yo "tokens"
vacias <- vacias %>%
  rename(tokens = palabra)

# Lista de palabras que quiero eliminar de los discursos:

vacias_fidel <- tibble(tokens = c("discurso",
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

# Aplicamos la función anti_join
discursosSinVacias <- TodosLosDiscursosToken %>%
  anti_join(vacias) %>%
  anti_join(vacias_fidel)



### 3.-  Gráfico de palabras más usadas

discursosSinVacias %>%
  count(tokens, sort = T) %>%
  filter(n>10000) %>%  ## -> Valores usados: 5.000 y 10.000
  mutate(tokens = reorder(tokens,n))  %>% 
  ggplot(aes(x=tokens,y=n,fill=tokens)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Palabras más usadas a lo largo de 50 años de discursos de Fidel Castro",
       subtitle = "1959-2008",
       x = NULL,
       y = "Número total de veces que aparecen") +
  coord_flip()


## fin Capítulo III


