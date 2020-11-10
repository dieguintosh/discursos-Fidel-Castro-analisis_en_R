# discursos-Fidel-Castro-analisis_en_R

Trabajo fin de curso de la _Asignatura Análisis de textos y Estilometría usando R_, del Máster en Humanidades Digitales de la UNED. 
Realizado en el verano de 2020.

## Textos a analizar
He realizado este trabajo a partir de una colección de 1151 discursos del fallecido líder cubano Fidel Castro que abarcan de 1959 a 2008. Los discursos son fácilmente accesibles a través de un sitio oficial del Gobierno de Cuba.

El mayor problema que presentan estos textos es que no se trata solamente de discursos orales leídos en el atril de un acto político. Si bien en su mayoría son así, en la misma colección hay entrevistas, textos escritos de puño y letra por Fidel Castro para ser publicados en prensa, comentarios escritos de sus últimos años y algún que otro texto de índole variada. La única manera de diferenciarlos era abrir los discursos uno a uno y seleccionarlos. Ante la imposibilidad material de hacerlo, he optado por tratar todos los textos como si fueran iguales, aún a riesgo de alterar ligeramente la interpretación de los resultados.

Una revisión de los textos revela también mucha suciedad debida a la transcripción literal: entrecomillados, puntuaciones y espaciados inconsistentes, paréntesis con interjecciones... Tampoco he hecho una limpieza de este tipo de detalles, que me habría llevado semanas de trabajo, si bien en algunos ejercicios, como en la eliminación de palabras vacías, se aborda la manera de limpiar algunas de estas suciedades.

## Directorio de Trabajo: ProyectoFidel
El directorio de trabajo que he utilizado en todo momento se llama **ProyectoFidel**.

**NOTA WINDOWS:** he realizado el trabajo en un ordenador Apple Mac, por lo que tal vez el código necesite ajustes para ser ejecutado en Windows.

## Capítulos
El trabajo consta de 8 capítulos. Cada capítulo corresponde a un script de R, que incluyen comentarios. Hay capítulos que han sido desarrollados en dos scripts, pues se han hecho análisis alternativos. 

Estos son los capítulos del trabajo:

### Capítulo I: web scraping 

> El código de este capítulo se encuentra en el archivo:
Script_01-web_scraping.R

La dirección web que recoge los enlaces a los textos con los que he trabajado es www.cuba.cu/gobierno/discursos/

Inspeccionando el HTML de la página veo que los enlaces que llevan a los discursos tienen la misma estructura:

...

http://www.cuba.cu/gobierno/discursos/1998/esp/f040998e.html

http://www.cuba.cu/gobierno/discursos/1999/ing/f030999i.html

http://www.cuba.cu/gobierno/discursos/2000/ale/f290300a.html

http://www.cuba.cu/gobierno/discursos/2001/fra/f110801f.html

http://www.cuba.cu/gobierno/discursos/2002/ita/n210302t.html

http://www.cuba.cu/gobierno/discursos/2002/rus/f300802r.html

http://www.cuba.cu/gobierno/discursos/2002/ara/f081202b.html

...


Estas URL aportan varios datos importantes:

* IDIOMA: los fragmentos de código /esp/, /ing/, /ale/, /fra/, /ita/, /rus/, /ara/… indican el idioma en el que está el archivo. Obviamente nos interesan los que están en español /esp/.
* FECHA: La parte final de la URL contiene la fecha exacta del discurso:
    * http://www.cuba.cu/gobierno/discursos/1998/esp/f010159e.html es el discurso en español de fecha 1 de enero de 1959.
    * http://www.cuba.cu/gobierno/discursos/2002/rus/f300802r.html es el discurso del 30 de agosto de 2002 traducido al ruso.
    * etc.
       
El objetivo de este primer capítulo es descargar los discursos en español, grabarlos en nuestro disco duro en ficheros txt, cada uno de ellos nombrado con la fecha exacta que figura en su propia URL, por ejemplo: 

`040998.txt` obtenido de la URL
http://www.cuba.cu/gobierno/discursos/1998/esp/f040998e.html

`030999.txt` obtenido de la URL
http://www.cuba.cu/gobierno/discursos/1999/ing/f030999i.html

etc.

Pero hacer así la descarga tiene una consecuencia indeseada: cuando llegan al disco duro de nuestro ordenador el sistema operativo los ordena alfabéticamente, con lo que pierden el orden temporal. En el ejemplo anterior, el sistema listaría antes un discurso de 1999 que empieza por 03 que uno de 1998 que empieza por 04.
La manera que he encontrado de solucionarlo es añadirle una numeración correlativa antes de la fecha, de modo que el primer discurso sería el número 0001 y el último el 1151. El objetivo es obtener esto:

`0001_010159.txt`
`0002_040159.txt`
...
`0272_290666.txt`
...
`0918_190401.txt`
...
`1151_220208.txt`

El script permite descargar 1151 documentos en español, que es el corpus con el que voy a trabajar.

En la segunda parte de este script he agrupado los archivos por años, de modo que he generado 50 archivos txt, cada uno de ellos nombrado con su año (1959.txt, 1960.txt, etc). Cada archivo contiene todos los textos brutos de los discursos de ese año. 

### Capítulo II: Análisis cuantitativo 

>El código de este capítulo se encuentra en el archivo:
Script_02 Analisis_Cuantitativo.R

Este capítulo simplemente hace una reorganización de los discursos en tablas y un conteo de palabras.

### Capítulo III: Palabras vacías 

> El código de este capítulo se encuentra en el archivo:
Script_03 Palabras-vacias.R

Casi todos los archivos comienzan con una fórmula muy parecida a este ejemplo:

_"Discurso pronunciado por el Comandante en Jefe Fidel Castro Ruz, Primer Secretario del Comité Central del Partido Comunista de Cuba y Presidente de los Consejos de Estado y de Ministros, en la clausura del VIII Congreso de la FEEM, en el Palacio de las Convenciones, el 6 de diciembre de 1991."_

Muchas de estas palabras (_discurso, pronunciado, Comandante, Jefe, Fidel, Castro, Ruz..._) no tiene sentido que estén dentro de los análisis, por lo que he creado una pequeña lista que trata de limpiar la mayoría de estos encabezamientos.

Por otro lado, las personas que hicieron la transcripción anotaron entre paréntesis comentarios como aplausos, ovación... que también considero eliminables en este contexto.

Tras el vaciado, se realiza un gráfico con las palabras más usadas. He realizado dos gráficos, modificando el filtro que selecciona las palabras más usadas (más de 5.000 apariciones o más de 10.000 apariciones)

En ambos gráficos se aprecia como las tres palabras más repetidas (_pueblo, país, revolución_) lo son con bastante diferencia sobre el resto, y tienen, desde luego, una simbología muy clara en el contexto político del régimen cubano.

### Capítulo IV: Comparación de Léxicos 

> El código de este capítulo se encuentra en el archivo:
Script_04-comparacion_lexicos.R

Para establecer comparaciones he dividido los discursos en tres períodos, basándome en acontecimientos de la historia reciente de Cuba:
* Periodo I: de 1959 a 1990. Triunfo de la Revolución y desarrollo del estado cubano moderno
* Periodo II: 1991 a 2000. El llamado Período Especial, una severa crisis económica motivada por la disolución de la Unión Soviética.
* Período III: de 2001 a 2008, año en el que Fidel Castro renunció al poder por motivos de salud.

En este capítulo he asociado cada palabra de cada discurso a su período correspondiente, y he realizado dos gráficos comparativos, uno de frecuencia relativa de aparición de palabras y otro de frecuencia absoluta.

El interés de estos gráficos está en apreciar las palabras –y por tanto los temas– que han estado presentes en cada Período, cuáles han de dejado de ser importantes y cuáles han entrado en los discursos con el paso de los años.

### Capítulo V: PoS Tagging, desambiguación léxica, etiquetado gramatical 

> El código de este capítulo se encuentra en el archivo:
Script_05-PoStagging.R

Utilización de la librería **udpipe**. En este capítulo he aplicado este modelo de análisis a los discursos agrupados por períodos.
A la hora de obtener gráficos comparativos he realizado uno por sustantivos lematizados y otro por verbos (sin verbos auxiliares).

La comparativa de verbos por períodos no me parece que aporte datos interesantes. En cambio en la comparativa de sustantivos puede verse la evolución temporal de algunos términos con mucho trasfondo político (_trabajo, trabajador, obrero, lucha, esfuerzo… o dólar_).

### Capítulo VI: Colocación y coocurrencia. Redes Léxicas.

>El código de este capítulo se encuentra en el archivo:
Script_06-coocurrencias_bigramas.R

**Primera Parte.-** En la primera parte de este capítulo he encontrado, vaciado y contado los bigramas que aparecen en todos los discursos, organizados por períodos. He creado dos gráficos, uno de Frecuencia Absoluta y otro de Frecuencia Relativa de aparición de bigramas por Período:

En ambos gráficos se aprecia ya muy bien los temas políticos más importantes de cada época, con un líder destacado que fue creciendo en importancia a lo largo de los años: el bigrama _“estados unidos”_.

Llama también mucho la atención el bigrama _“posada carriles”_, referido a Luis Posada Carriles, opositor al régimen castrista y al que se le atribuyen actos terroristas y varios intentos de asesinar a Fidel Castro a lo largo de décadas. Está claro que el líder cubano le tuvo muy presente incluso en sus alocuciones públicas.

**Segunda Parte .-** grafo a partir de los bigramas, utilizando las librerías **igraph**, **ggraph** y **grid**.

**Tercera Parte.-** Coocurrencias y Correlaciones. He utilizado la librería **widyr** para analizar coocurrencias y correlaciones de palabras (pairwise_count y pairwise_cor). A partir de las correlaciones he elaborado dos gráficos con palabras diferentes.

### Capítulo VII: Topic Modeling. 

>Los código de este capítulo se encuentra en los archivos:
Script_07-PRIMER_INTENTO_TextosBrutos.R
Script_07-SEGUNDO_INTENTO_sustantivos solo.R

**Utilización de las librerías tm, topicmodels y scales.**

En una primera versión de este análisis (Script_07-PRIMER_INTENTO_TextosBrutos.R), partiendo de todos los textos en bruto, obtuve resultados muy decepcionantes, pues el script realiza agrupaciones muy poco significativas, casi iguales y sin un sentido muy claro.

Por tanto decidí hacer un segundo intento (Script_07-SEGUNDO_INTENTO_sustantivos solo.R) preparando antes los textos, seleccionando sólamente los sustantivos lematizados. En la práctica esto supone hacer primero un procesado PoS Tagging como el realizado en el Script 5. De ahí pude extraer los sustantivos lematizados y hacer el análisis únicamente con ellos, con lo que los resultados del Topic Modeling finalmente fueron más significativos.

En este gráfico ya se aprecian 4 agrupaciones de palabras con tópicos más consistentes: un tópico centrado en la economía agraria (_producción, trabajo, caña, millón, problema, tierra, industria..._); educación (_escuela, niño, maestro, estudiante..._); y dos tópicos centrados en conceptos políticos: _pueblo, país, mundo, guerra, gobierno, revolución, patria, lucha, imperialismo, dólar..._

Respecto a otro tipo de análisis como el gráfico de tópicos o las asignaciones por palabras, no he conseguido resultados interesantes, tal vez porque los textos con los que estoy trabajando son demasiado homogéneos en cuanto a temática.

### Capítulo VIII: Análisis de sentimientos. 

> Los código de este capítulo se encuentra en los archivos:
Script_08-get_sentiments.R
Script_08-syuzhet.R

He realizado este análisis por décadas, buscando una división temporal más homogénea que los períodos que venía usando hasta ahora.

**Limpieza de los discursos**

Para este análisis ha habido que hacer un trabajo de preparación de los textos un poco más complejo. Se ha hecho una reordenación de los contenidos de cada archivo descargado: el contenido de cada archivo txt de la carpeta **brutosDescargados** se ha recolocado en una sola cadena de caracteres, como si cada discurso estuviera almacenado en un solo párrafo. Así, al agruparlos por décadas, cada década tiene tantas cadenas de caracteres como archivos anuales agrupa. Los archivos limpios se colocan en la carpeta discursos/limpios.


#### Primer análisis con get_sentiments.R
> Archivo: Script_08-get_sentiments.R

Este análisis no ofrece un resultado muy interesante, en mi opinión, tal vez porque los discursos son muy homogéneos en su temática. La inmensa mayoría parecen enviar sentimientos positivos, y son pocos y puntuales los que bajan hasta niveles negativos. Quizás se puede apreciar que la década 1990-1999 tiene un perfil más positivo aún, lo cual es peculiar dado que coincide con el Período Especial, una gran crisis económica y social vivida en Cuba en esos años.

#### Análisis syuzhet
>Archivo: Script_08-syuzhet.R

Para realizar este análisis se han utilizado los archivos por décadas que había creado en el script anterior.

En esta ocasión he optado por hacer un análisis de cada década por separado, obteniendo un gráfico por década.

No puedo decir que haya sacado ninguna conclusión con este último análisis, habida cuenta de que este gráfico parece muy distinto a los realizados con get_sentiments.R. Me temo que no sé interpretarlo muy bien y no soy capaz de explicarme la –aparente– incosistencia en los resultados de uno y otro análisis. Haciendo los cálculos con el resto de décadas obtengo inconsistencias similares, por lo que no los he añadido al trabajo.
