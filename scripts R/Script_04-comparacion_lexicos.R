###############################################################
# Proyecto Final Diego Ortiz González
# Capítulo IV: Comparar léxicos por períodos

# Directorio de Trabajo: ProyectoFidel
###############################################################

# Vamos a distinguir tres períodos en la historia de Fidel Castro como jefe de estado cubano:
# Periodo_I: de 1959 a 1990. Revolución y desarrollo del estado cubano moderno
# Periodo_II: 1991 a 2000. El llamado "Período Especial", una severa crisis económica motivada por la
# disolución de la Unión Soviética.
# Período_III: de 2001 a 2008, año en el que Fidel Castro renunció al poder por motivos de salud.


# Vamos a partir de la tabla realizada en el Script_02, añadiéndole una columna nueva de Período
# Cada palabra irá así asociada al período en el que fue usada.

### 1.- Rehaciendo la tabla de todos los discursos + columna "periodo":

ficheros <- list.files("discursos/porAnnos")

annos <- as.character(1959:2008)

# NUEVO: vector con los períodos
periodos <- c(rep("Periodo_I",32),rep("Periodo_II",10),rep("Periodo_III",8))


library(tidyverse)
library(tidytext)

## la tabla donde vamos a meter todos los discursos
# -> con la variable periodos
tablaTodosLosDiscursos <- tibble(anno=character(),
                                 periodo=character(),
                                 parrafo=numeric(),
                                 texto=character())


# bucle modificado para dar contenido a la columna "periodo":
for (i in 1:length(ficheros)){
  discurso <- readLines(paste("discursos/porAnnos",
                              ficheros[i],
                              sep="/"))
  temporal <- tibble(
    anno=annos[i],
    periodo=periodos[i],
    parrafo=seq_along(discurso),
    texto=discurso)
  
  tablaTodosLosDiscursos <- bind_rows(tablaTodosLosDiscursos, temporal)
}
rm(temporal)
rm(discurso)
rm(i)


# A continuación seguimos igual que en Capítulo anterior: obtención de palabras token y vaciado:


# palabras token:
TodosLosDiscursosToken <- tablaTodosLosDiscursos %>%
  unnest_tokens(tokens,texto)

# Borrado de palabras vacías
vacias <- read_csv("https://tinyurl.com/VaciasEsp",
                   locale = default_locale())

vacias <- vacias %>%
  rename(tokens = palabra)

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

rm(vacias, vacias_fidel)

# --> Aquí ya tengo mi tabla discursosSinVacias con la columna "periodo" integrada


### 2.- Gráficos de frecuencias absolutas y frecuencias relativas:

# Tabla de frecuencias relativas
tabla_frecuencias <- discursosSinVacias %>%
  group_by(periodo) %>%
  count(tokens, sort = T) %>%
  transmute(tokens, frecRelativa=n/sum(n))

# Gráfico de frecuencias relativas
tabla_frecuencias %>%
  group_by(periodo) %>%
  filter(frecRelativa>0.003)%>%
  ggplot(aes(reorder(tokens, frecRelativa),
             frecRelativa,
             fill = periodo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~periodo,
             scales = "free_x") +
  theme_linedraw() +
  ggtitle("Frecuencia relativa de aparición de las palabras por Períodos")+
  labs(x = "",
       y = "Frecuencia") +
  coord_flip()+
  theme(legend.position="none")


## Gráfico de frecuencias Absolutas
discursosSinVacias %>%
  group_by(periodo) %>%
  count(tokens, sort = T) %>%
  top_n(25) %>%
  ggplot(aes(reorder(tokens, n),
             n,
             fill = periodo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~periodo,
             scales = "free_x") +
  theme_linedraw() +
  ggtitle("Frecuencia absoluta de aparición de las palabras por Períodos")+
  labs(x = "",
       y = "Frecuencia") +
  coord_flip()+
  theme(legend.position="none")



## fin Capítulo IV









