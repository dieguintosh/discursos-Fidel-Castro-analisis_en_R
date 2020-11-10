################################################
# Proyecto Final Diego Ortiz González
# Capítulo VIII: Análisis de Sentimientos con syuzhet

# Directorio de Trabajo: ProyectoFidel
################################################

# si no está instalado:
# install.packages("syuzhet")

library(tidyverse)
library(tidytext)

library(syuzhet)

# cargamos diccionario y función get_sentiments.R, como en el script anterior
sentimientos <- read_tsv("https://tinyurl.com/SentiEsp",
                         col_types = "cccn",
                         locale = default_locale())

source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")


######## Análisis para la Década I (1959-1970)

# Usaremos los archivos por décadas que habíamos creado en la primera y segunda partes del script anterior
# y que están en discursos/porDecadas:
# -> Decada_I_texto-completo.txt
# -> Decada_II_texto-completo.txt
# -> Decada_III_texto-completo.txt
# -> Decada_IV_texto-completo.txt
# -> Decada_V_texto-completo.txt

# cargamos la Década I (1959-1970)
texto_entrada <- read_lines("discursos/porDecadas/Decada_I_texto-completo.txt",
                            locale = default_locale())

         #### NOTA ####: 
#para realizar los cálculos del resto de décadas, sólo hay que cambiar el código anterior,
# sustituyendo el archivo Decada_I_texto-completo.txt por cualquier de los otros cuatro.
         ####      ####


# Lo metemos en una tabla
texto_analizar <- tibble(texto = texto_entrada)

# Procesado get_sentiments y además
# -> separación de tokens
# -> división por páginas de 500 palabras

texto_analizar <- texto_analizar %>%
  unnest_tokens(palabra,texto)  %>%
  mutate(pagina = (1:n()) %/% 500 +1) %>% # agrupar por páginas de 500 palabras
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate (negativo = negativo*-1) # para cambiar la polaridad se multiplica por -1

# Calcular la valencia de cada página, restando el valor positivo del negativo
puntuacion <- texto_analizar %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

# Así obtenemos un gráfico de lo hecho hasta ahora
ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "red") +
  theme_minimal() +
  ylab("Sentimiento Positivo / Negativo") +
  xlab("Línea temporal") +
  ggtitle(expression("Linea temporal de Sentimientos durante la Década I")) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


#### Cálculos syuzhet

texto_trans <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 100,  ## jugar con este valor. Por defecto es 5
                                 x_reverse_len = nrow(puntuacion),
                                 scale_range = TRUE) # garantiza valor de -1 a 1


# la orden anterior ofrece un vector de 100 valores, de rango -1 a 1, con decimales
# para hacer un gráfico lo metemos en una tabla
texto_trans <- tibble(pagina = seq_along(texto_trans),
                      ft = texto_trans)  #ft es "frecuencia temporal"

ggplot(texto_trans, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "red", fill = "red") +
  theme_minimal() +
  labs(x = "Linea temporal",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle("Análisis syuzhet, Década I 1959-1970")

# Fin del Capítulo VIII