###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TALLER DE INTRODUCCIÓN A R                                            -----      
### MANIPULACIÓN Y VISUALIZACIÓN DE DATOS EN R Y TIDYVERSE (PARTE 1)      -----
### 
### Autor: Amadeo Guzmán
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#
##
### 1.- Algunas definiciones y consideraciones previas ----
##
#

# Cada línea de texto que se escribe después del símbolo # R no lo considera como un elemento a ejecutar --> Sirve para hacer comentarios, poner títulos de sección, links de pag. web, etc.
# En general, NO se recomienda usar acentos y ocupar la letra ñ
# R es un lenguaje de programación orientado a OBJETOS --> todo lo podemos guardar en un objeto y ocuparlo cuando sea necesario
# Un paquete es un conjunto de funciones y datos específicos para cada tema.
# Una función es una secuencia de códigos guardada como un OBJETO de R que toma variables de entrada, realiza un proceso y nos devuelve un resultado
# Un data frame es una colección rectangular de variables (columnas) y observaciones (filas) -> Cada columna DEBE ser una variable y cada fila DEBE ser una observación
# IMPORTANTE --> Para ejecutar un código en R puedes: (1) poner el cursor sobre el código y apretar Ctrl + Enter; (2) seleccionar el código y hacer clik en "Run"



#
##
### 2.- Cargar librerías al entorno de trabajo -----
##
#

# instalar paquetes (librerías) en el pc --> Solo se hace 1 vez 
#install.packages(c("tidyverse", "lubridate", "janitor", "ggthemes", "scales", "stringr", "forcats", "remotes"))
#remotes::install_github("allisonhorst/palmerpenguins")

#Cada vez que abrimos una nueva sesión de R debemos cargar las librerías que vamos a ocupar (No instalar, solo cargar)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(stringr)
library(forcats)
library(palmerpenguins)
library(skimr)
library(gapminder)



#
##
### 3.- Datos ----- 
##
#

# En esta primera parte vamos a trabajar con datos que están precargados (y ordenados) en R... después trabajaremos con nuestras bases de datos (requieren más trabajo de manipulación de datos)

# Datos penguins -> Mediciones de tamaño en pingüinos adultos cerca de la estación Palmer (Antártica)......
penguins

# con la función head() vemos por defecto las primeras 6 líneas de cada base de datos
head(penguins)
tail(penguins)

head(penguins, 1) #tambien podemos elegir el número de filas que queremos ver

View(penguins)    #para ver toda la base de datos

?penguins         #obtener información de la base de datos





#
##
### 4.- Explorar la base de datos -----
##
#


dim(penguins)     #cantidad de filas y columnas del dataframe
names(penguins)   #nombres de las variables
glimpse(penguins) #resumen de las características de cada variable

# ver los niveles de cada factor (variable categórica)
levels(penguins$species) # con el símbolo $ podemos acceder a las variables (columnas) que están en la base de datos
levels(penguins$sex)


#################################### -- EJERCICIO -- ##############################################+
# Cuales son los niveles de la variable factor que nos falta observar??


# Que pasa si aplicamos la función levels() a una variable numérica de la base de datos penguins??


###################################################################################################+ 


#resumen general de todo el data set
skim(penguins)

#otra forma de obtener un resumen estadístico
summary(penguins)

#Tambien podemos generar tablas de contingencia que nos permite conocer la distribución de número datos entre categorías
table(penguins$species, penguins$island)


########################## -- EJERCICIO -- #############################+
# como es la distribución de datos según la especie de pingüino y sexo

########################################################################+




#
##
### 5.- Visualizar datos con ggplot2 -----
##
#


# ggplot2 es un paquete que pertenece al universo Tidyverse y utiliza la gramática de los gráficos para construir sus visualizaciones
# funciona en base a capas de información


#
# _5.1 Elementos de Estética o aes() ----
#

ggplot()

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g))
  
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()


# Podemos modificar algunas características generales FIJAS: color y forma
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "blue", shape=3)


#Podemos agregar más elementos al gráfico, pero esta vez asociado a las variables de la base de datos
#Esto es necesario hacerlo dentro del parámetro aes()

#color
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

#Reciclar código: copiar + pegar ;) 





#color + forma
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_point()


#alternar estéticas visuales y características fijas
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3)


#agregar transparencia a los puntos (para disminuir el efecto de la sobreposición de puntos)
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3, alpha = 0.4) #alpha toma valores entre 0 y 1

#NOTA: Una vez que mapeamos variables con propiedades estéticas, el paquete ggplot2 selecciona una escala razonable para usar con la estética elegida y construye una leyenda que explica la relación entre niveles y valores.



#################################### -- EJERCICIO -- ######################################################+
# Modificar los gráficos anteriores esta vez utilizando las variables "culmen_length_mm" y "culmen_depth_mm"
# y agregar una capa de color con una variable categórica que no sea la especie



#############################################################################################################+


#de momento hemos asignado el color solo a variables categóricas. Qué pasa si lo hacemos con una variable numérica?
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point()



##########################################  EJERCICIO  ####################################################+
# ¿Qué pasa si asignamos el color a una variable cuantitativa, pero con una condición?? 
# ocupa la siguiente condición: color = culmen_length_mm >45

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm > 45)) +
  geom_point()

##########################################################################################################+

#al mismo ejemplo anterior le agregamos una nueva capa para mejorar la escala de color
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point() +
  scale_color_viridis_c() # existen una gran cantidad de escalas de color prediseñadas..... y también puedes hacer las tuyas



#IMPORTANTE: Hacer visualizaciones no es solo poner alguna variable en cada eje, algo de color y listo. Debemos explorar los datos e intentar encontrar las relaciones que puedan existir. Debemos abordar este problema desde diferentes perspectivas y evitar caer en posibles "sesgos" o malas interpretaciones.

#Ejemplo
#Existe algún tipo de relación entre las variables "culmen_length_mm" y "culmen_depth_mm"??

ggplot(data = penguins,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5)

#Ajustamos una recta de regresión a estos datos
ggplot(data = penguins,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")


#Esta relación es real?? Existen otras variables que puedan modificar el sentido de esta relación??
ggplot(data = penguins,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm, color=species)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")

# Que cambió?

#Nota: Lo que acabamos de ver es un ejemplo de lo que en estadística y probabilidad se conoce como la "Paradoja de Simpson" --> Una tendencia que aparece en varios grupos de datos desaparece cuando estos grupos se combinan y en su lugar aparece la tendencia contraria para los datos agregados (https://es.wikipedia.org/wiki/Paradoja_de_Simpson#:~:text=En%20probabilidad%20y%20estad%C3%ADstica%2C%20la,contraria%20para%20los%20datos%20agregados.)




#
# _5.2 Otros objetos geométricos disponibles ----
#

## Histogramas
ggplot(data = penguins, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white")


count(penguins, cut_width(flipper_length_mm, 2))


#el histograma tiene 1 problema --> Su aspecto puede variar dependiendo del número de rangos-barras (bins) que se ocupen
ggplot(data = penguins, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white", bins = 5)


## Una buena alternativa - > Distribución de densidad
ggplot(data = penguins, 
       mapping = aes(x=flipper_length_mm)) +
  geom_density()



#################################### -- EJERCICIO -- ############################################################+
# Como es la distribución de datos de la variable "flipper_length_mm" por especie. Puedes usar histograma,
# o un gráfico de densidad


# Si pudiste hacer el gráfico anterior..... ahora prueba usando el argumento "fill" dentro de aes()


#################################################################################################################+


## Gráfico de barras -> Recuentos
ggplot(data = penguins,
       mapping = aes(x=species)) +
  geom_bar()

#como podemos ordenar la barras -> paquete "forcats"
ggplot(data = penguins,
       mapping = aes(x = forcats::fct_infreq(species))) +
  geom_bar()


ggplot(data = penguins,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)))) +
  geom_bar()



#vamos a guardar nuestro gráfico como un objeto de nombre "g1"
(g1 <- ggplot(data = penguins,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)), fill=sex)) +
  geom_bar()
)

class(g1)

g1 + 
  coord_flip()

#Que pasa si olvidamos los paréntesis -> Mira los warnings en la consola
g1 + 
  coord_flip

#podemos probar otros sistemas de coordenadas... no siempre es la mejor idea, pero sirve para explorar
g1 + 
  coord_polar()



## Tipo Violin
ggplot(data = penguins,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_violin()

## Boxplot 
ggplot(data = penguins,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot()

## Boxplot  - horizontal
ggplot(data = penguins,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot() +
  coord_flip()

## Boxplot agregando otra variable de interés
ggplot(data = penguins,
       mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot()


#################################### -- EJERCICIO -- ############################################################+
# Una de las ventajas de hacer gráficos con ggplot2 es la posibilidad de combinar capas geométricas....
# Haz un gráfico combinando dos capas geométricas 



#################################################################################################################+



#si quieres eliminar la categoría NA (datos faltantes) puedes correr el siguiente código
penguins %>% 
  filter(!is.na(sex)) %>% 
ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot()

#Nota: eliminar los datos faltantes y realizar otro tipo de manipulaciones de datos te permitirá explorar de mejor forma tus datos... esto es todo un tema por si mismo y se podría abordar en otro taller..... quizás......¿?




#
# _5.3 Agregar otra capa de información con facet_ ----
#

penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_wrap(~island)


penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_wrap(~island, scales = "free_x")


penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_grid(~island, scales = "free_x", space = "free")



#Otro ejemplo de la utilidad de facets.....
penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point()


penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  facet_wrap(~sex)



#
# _5.4 Agregar títulos, modificación de escalas y del aspecto general del gráfico con theme() ----
#

library(ggthemes)

(gra_final <- penguins %>% 
  #manipulación de datos
  filter(!is.na(sex)) %>% 
  #gráfico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  geom_smooth(method = "lm", alpha=.1) +
    #titulos
  labs(title="Relación entre el largo y profundidad del culmen en pingüinos de la estación de investigación PALMER",
       subtitle="Dimensiones del culmen en las especies Adelle, Chinstrap y Gentoo.",
       x="\n Longitud del culmen (mm)",
       y="Profundidad del culmen (mm) \n", 
       color="Especie",
       caption = "Fuente: Long Term Ecological Research Network") +
    #escalas
  scale_x_continuous(limits = c(30, 60), breaks = seq(20, 60, 5)) +
  scale_y_continuous(limits = c(12, 24), breaks = seq(10, 24, 2)) +
    #aspecto general de la trama
  theme_minimal()
)

gra_final + theme_dark()
gra_final + theme_fivethirtyeight()
gra_final + theme_solarized()
gra_final + theme_igray()
gra_final + theme_map()
gra_final + theme_economist()
gra_final + theme_hc()


#Quieres guardar tú gráfico???
ggsave("gráfico_final.png", gra_final, dpi = 500, units = "cm", width = 25, height = 12)

  

  
#Nota: Esto es lo general y el punto de partida para poder hacer mejores visualizaciones y explorar tus datos con mayor profundidad.

#########################################  FIN  #########################################+

### Bonus Track

#
# Errores comunes -----
#

#Comprueba la parte izquierda de tu consola: si es un +, significa que R no cree que hayas escrito una expresión completa y está esperando que la termines. En este caso, generalmente es mejor comenzar de nuevo desde cero presionando ESCAPE para cancelar lo que estabas haciendo y que quedo incompleto.

#Colocar el + en el lugar equivocado: SIEMPRE debe ubicarse al final de la línea, no al inicio.


#
# Donde buscar ayuda -----
#

#En la consola de R escribe ?nombre_de_la_funcion y presiona enter. Te aparecerá la ayuda oficial que tiene R de capa paquete o función
#SAN GOOGLE -> una buena opción es copiar y pegar el mensaje de error tal cual en google o escribir lo que quieres hacer (es mejor si buscas en inglés). Generalmente las primeras opciones en la búsqueda te van a enviar a un foro de "Stack Overflow"..... ahí estas en el lugar correcto
#Puedes encontrar información y ejemplos de cómo usar cada paquete o función escribiendo el nombre de lo que quieres buscar agregando la letra R. Existen muchas páginas, blogs y talleres sobre diferentes temas relacionados a R
#En Twitter usando #Rstats


#Como no perderme en estas casi 500 líneas de código?? --> Mira en la parte superior derecha del editor (script) aparecen unas líneas horizonatales en forma de índice... haz click en ellas.....




