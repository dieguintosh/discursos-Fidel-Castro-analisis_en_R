###############################################################
# Proyecto Final Diego Ortiz González
# Capítulo I: web scraping

# Directorio de Trabajo: ProyectoFidel
###############################################################



##### PRIMERA PARTE: bajar al disco duro todos los discursos de Fidel Castro.
# Los enlaces se encuentran en la página http://www.cuba.cu/gobierno/discursos/


### 1.- Unir y limpiar las URL:
# instalar solo una vez:
#install.packages('rvest')

library(rvest)

#carpetas donde voy a guardar los discursos
dir.create("discursos")
dir.create("discursos/brutosDescargados")

# El HTML de la página que agrupa todos los enlaces
PaginaCentral <- read_html("http://www.cuba.cu/gobierno/discursos/")

# Obtengo los contenidos del atributo href: 
discursos <- PaginaCentral %>%
  html_nodes("a") %>%
  html_attr("href")

# Nota acerca de “reflexiones”. Hay algunos enlaces que están marcados con el nombre “reflexiones”. 
# He interpretado que estos enlaces no llevan a discursos propiamente dichos, sino a otro tipo de textos, 
# por lo que los voy a sacar de mi lista de enlaces:
discursosNoreflex <- grep("reflexiones",discursos,invert = TRUE)
discursos <- discursos[discursosNoreflex] 

# Seleciono sólo los enlaces que contienen la partícula “esp”, obviando los discursos traducidos a otros idiomas: 
discursosesp <- grep("esp",discursos)
discursos <- discursos[discursosesp]

# Revisando el vector discursos veo que las URL no tienen una forma homogénea, 
# pues algunas tienen la URL completa y otras sólo un fragmento. 
# Los voy a homogeneizar primero borrando una parte de la URL “larga”, para luego añadírsela a todos por igual:

for (i in 1:length(discursos)){
  discursos <- gsub(".*discursos/*","",discursos)
}

# Ahora añado a todos los elementos la misma estructura de URL:
discursos <- paste("http://www.cuba.cu/gobierno/discursos/", discursos, sep = "")
## Ya tengo todas las URL que apuntan a los 1151 discursos

rm(discursosesp)
rm(discursosNoreflex)
rm(i)


### 2.- Bajar los discursos al disco duro en archivos txt.

# Cada archivo bajado va a tener el mismo nombre que tiene la parte final de su URL correspondiente,
# con algunas pequeñas modificaciones

# En primer lugar creo un vector de nombres de archivo, limpiando algunas irregularidades 
# que he detectado (htm en lugar de html) y eliminando las letras para quedarme sólo con la fecha:
nombresdearchivo <- gsub("http://www.cuba.cu/gobierno/discursos/\\w{4}/esp/","",discursos)
nombresdearchivo <- gsub("e.html","",nombresdearchivo)
nombresdearchivo <- gsub("e.htm","",nombresdearchivo)
nombresdearchivo <- gsub(".html","",nombresdearchivo)
nombresdearchivo <- gsub("^\\w","",nombresdearchivo)



# Número de orden. Voy a añadirle un número de orden al principio de cada archivo, de modo que sea
# sencillo ordenarlo alfabéticamente (numéricamente)

# Nótese que el vector actual nombresdearchivo tiene 1151 elementos, pero están ordenados temporalmente de último a primero.
# El primer elemento es el último temporalmente (220208 - 22 de febrero de 2008) con lo cual debería ser el número 1151
# El último elemento en cambio es el primer discurso (010159), por lo que debería ser el número 1.

# voy a hacerlo con tablas
library(tidyverse)
library(tidytext)

ceros <- c(rep("",152),rep("0",900),rep("00",90),rep("000",9))

tablaNombresArchivo <- tibble(contador = seq_along(nombresdearchivo),
                              nombresdearchivo)

tablaReordenada <- tablaNombresArchivo %>%
  mutate(inverso = length(contador)+1 - contador) %>%
  mutate(ceros = ceros) %>%
  unite(numeracion, ceros,inverso,sep = "") %>%
  unite(numeracion, numeracion, nombresdearchivo, sep = "_")

nombresdearchivo <- c(tablaReordenada$numeracion)

rm(ceros, tablaNombresArchivo,tablaReordenada)

# Descarga

# Con este bucle cada URL se descarga a un archivo .txt (puede tardar unos minutos)
for (i in 1:length(discursos)){
  discursoindividual <- read_html(discursos[i]) %>%
    html_nodes("p") %>%
    html_text("span")
  writeLines(discursoindividual,paste("discursos/brutosDescargados/",nombresdearchivo[i], ".txt", sep = ""))
}

rm(i,discursoindividual,discursos,nombresdearchivo,PaginaCentral)

####################################################################
## SEGUNDA PARTE: AGRUPAR LOS ARCHIVOS POR AÑOS
## Los archivos están fechados desde 1959 a 2008, 50 años.
## Para facilitar su procesamiento posterior voy a agrupar los archivos por año, 
# de modo que pueda trabajar con 50 archivos .txt, cada uno de ellos conteniendo los discursos de ese año


#carpeta donde voy a guardar los archivos txt
dir.create("discursos/porAnnos")

annos <- as.character(1959:2008)

# Para hacer la agrupación ejecutamos un doble bucle:

# Bucle Exterior
for (i in 1:length(annos)){
  annos2Cifras <- gsub("^\\d{2}",
                       "",
                       annos[i],
                       perl=TRUE)
  agrupacionPorAnno <- list.files("discursos/brutosDescargados",
                                  pattern = paste(annos2Cifras,"txt", sep = ".")) 
  tablaPorAnno <- tibble(textoDeTodoElAnno = character())
  
  # Bucle interior
  for (n in 1:length(agrupacionPorAnno)){
    discurso <- readLines(paste("discursos/brutosDescargados",
                                agrupacionPorAnno[n],
                                sep = "/"))
    
    temporal <- tibble(textoDeTodoElAnno = discurso)
    tablaPorAnno <- bind_rows(tablaPorAnno, temporal)
  } # Fin del bucle interior
  
  writeLines(tablaPorAnno$textoDeTodoElAnno , paste("discursos/porAnnos", paste(annos[i],"txt", sep = "."), sep = "/"  ))
} # Fin de bucle exterior

## Con esto tengo en el disco duro 50 archivo txt, cada uno con todo el texto de cada año completo.
## fin Capítulo I