###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TALLER DE INTRODUCCI�N A R                                            -----      
### MANIPULACI�N Y VISUALIZACI�N DE DATOS EN R Y TIDYVERSE (PARTE 1)      -----
### 
### Autor: Amadeo Guzm�n
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#
##
### 1.- Algunas definiciones y consideraciones previas ----
##
#

# Cada l�nea de texto que se escribe despu�s del s�mbolo # R no lo considera como un elemento a ejecutar --> Sirve para hacer comentarios, poner t�tulos de secci�n, links de pag. web, etc.
# En general, NO se recomienda usar acentos y ocupar la letra �
# R es un lenguaje de programaci�n orientado a OBJETOS --> todo lo podemos guardar en un objeto y ocuparlo cuando sea necesario
# Un paquete es un conjunto de funciones y datos espec�ficos para cada tema.
# Una funci�n es una secuencia de c�digos guardada como un OBJETO de R que toma variables de entrada, realiza un proceso y nos devuelve un resultado
# Un data frame es una colecci�n rectangular de variables (columnas) y observaciones (filas) -> Cada columna DEBE ser una variable y cada fila DEBE ser una observaci�n
# IMPORTANTE --> Para ejecutar un c�digo en R puedes: (1) poner el cursor sobre el c�digo y apretar Ctrl + Enter; (2) seleccionar el c�digo y hacer clik en "Run"



#
##
### 2.- Cargar librer�as al entorno de trabajo -----
##
#

# instalar paquetes (librer�as) en el pc --> Solo se hace 1 vez 
#install.packages(c("tidyverse", "lubridate", "janitor", "ggthemes", "scales", "remotes"))
#remotes::install_github("allisonhorst/palmerpenguins")

#Cada vez que abrimos una nueva sesi�n de R debemos cargar las librer�as que vamos a ocupar (No instalar, solo cargar)
library(tidyverse)
library(scales)
library(ggthemes)
library(palmerpenguins)

library(skimr)




#
##
### 3.- Datos ----- 
##
#

# En esta primera parte vamos a trabajar con datos que est�n precargados (y ordenados) en R... despu�s trabajaremos con nuestras bases de datos (requieren m�s trabajo de manipulaci�n de datos)

# Datos penguins -> Mediciones de tama�o en ping�inos adultos cerca de la estaci�n Palmer (Ant�rtica)......
penguins



# Si queremos cargar la base de datos (.csv) desde nuestra carpeta de trabajo
(datos <- read_csv2("bd_pinguinos.csv")
  )

# con la funci�n head() vemos por defecto las primeras 6 l�neas de cada base de datos
head(datos)
tail(datos)

head(datos, 1) #tambien podemos elegir el n�mero de filas que queremos ver

View(datos)    #para ver toda la base de datos

#?penguins         #obtener informaci�n de la base de datos





#
##
### 4.- Explorar la base de datos -----
##
#


dim(datos)     #cantidad de filas y columnas del dataframe
names(datos)   #nombres de las variables
glimpse(datos) #resumen de las caracter�sticas de cada variable


#resumen general de todo el data set
skim(datos)

#otra forma de obtener un resumen estad�stico
summary(datos)

#Tambien podemos generar tablas de contingencia que nos permite conocer la distribuci�n de n�mero datos entre categor�as
table(datos$species, datos$island)


########################## -- EJERCICIO -- #############################+
# como es la distribuci�n de datos seg�n la especie de ping�ino y sexo


########################################################################+




#
##
### 5.- Visualizar datos con ggplot2 -----
##
#


# ggplot2 es un paquete que pertenece al universo Tidyverse y utiliza la gram�tica de los gr�ficos para construir sus visualizaciones
# funciona en base a capas de informaci�n


#
# _5.1 Elementos de Est�tica o aes() ----
#

ggplot()

ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g))
  
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()


# Podemos modificar algunas caracter�sticas generales FIJAS: color y forma
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "blue", shape=3)


#Podemos agregar m�s elementos al gr�fico, pero esta vez asociado a las variables de la base de datos
#Esto es necesario hacerlo dentro del par�metro aes()

#color
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()

#Reciclar c�digo: copiar + pegar ;) 





#color + forma
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_point()


#alternar est�ticas visuales y caracter�sticas fijas
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3)


#agregar transparencia a los puntos (para disminuir el efecto de la sobreposici�n de puntos)
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3, alpha = 0.4) #alpha toma valores entre 0 y 1

#NOTA: Una vez que mapeamos variables con propiedades est�ticas, el paquete ggplot2 selecciona una escala razonable para usar con la est�tica elegida y construye una leyenda que explica la relaci�n entre niveles y valores.



#################################### -- EJERCICIO -- ######################################################+
# Modificar los gr�ficos anteriores esta vez utilizando las variables "culmen_length_mm" y "culmen_depth_mm"
# y agregar una capa de color con una variable categ�rica que no sea la especie



#############################################################################################################+


#de momento hemos asignado el color solo a variables categ�ricas. Qu� pasa si lo hacemos con una variable num�rica?
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point()



##########################################  EJERCICIO  ####################################################+
# �Qu� pasa si asignamos el color a una variable cuantitativa, pero con una condici�n?? 
# ocupa la siguiente condici�n: color = culmen_length_mm >45




##########################################################################################################+

#al mismo ejemplo anterior le agregamos una nueva capa para mejorar la escala de color
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point() +
  scale_color_viridis_c() # existen una gran cantidad de escalas de color predise�adas..... y tambi�n puedes hacer las tuyas



#IMPORTANTE: Hacer visualizaciones no es solo poner alguna variable en cada eje, algo de color y listo. Debemos explorar los datos e intentar encontrar las relaciones que puedan existir. Debemos abordar este problema desde diferentes perspectivas y evitar caer en posibles "sesgos" o malas interpretaciones.

#Ejemplo
#Existe alg�n tipo de relaci�n entre las variables "culmen_length_mm" y "culmen_depth_mm"??

ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5)

#Ajustamos una recta de regresi�n a estos datos
ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")


#Esta relaci�n es real?? Existen otras variables que puedan modificar el sentido de esta relaci�n??
ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm, color=species)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")

# Que cambi�?

#Nota: Lo que acabamos de ver es un ejemplo de lo que en estad�stica y probabilidad se conoce como la "Paradoja de Simpson" --> Una tendencia que aparece en varios grupos de datos desaparece cuando estos grupos se combinan y en su lugar aparece la tendencia contraria para los datos agregados (https://es.wikipedia.org/wiki/Paradoja_de_Simpson#:~:text=En%20probabilidad%20y%20estad%C3%ADstica%2C%20la,contraria%20para%20los%20datos%20agregados.)




#
# _5.2 Otros objetos geom�tricos disponibles ----
#

## Histogramas
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white")


count(datos, cut_width(flipper_length_mm, 2))


#el histograma tiene 1 problema --> Su aspecto puede variar dependiendo del n�mero de rangos-barras (bins) que se ocupen
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white", bins = 5)


## Una buena alternativa - > Distribuci�n de densidad
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_density()



#################################### -- EJERCICIO -- ############################################################+
# Como es la distribuci�n de datos de la variable "flipper_length_mm" por especie. Puedes usar histograma,
# o un gr�fico de densidad



# Si pudiste hacer el gr�fico anterior..... ahora prueba usando el argumento "fill" dentro de aes()



#################################################################################################################+


## Gr�fico de barras -> Recuentos
ggplot(data = datos,
       mapping = aes(x=species)) +
  geom_bar()

#como podemos ordenar la barras -> paquete "forcats"
ggplot(data = datos,
       mapping = aes(x = forcats::fct_infreq(species))) +
  geom_bar()


ggplot(data = datos,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)))) +
  geom_bar()



#vamos a guardar nuestro gr�fico como un objeto de nombre "g1"
(g1 <- ggplot(data = datos,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)), fill=sex)) +
  geom_bar()
)

class(g1)

g1 + 
  coord_flip()

#Que pasa si olvidamos los par�ntesis?? -> Mira los warnings en la consola
g1 + 
  coord_flip

#podemos probar otros sistemas de coordenadas... no siempre es la mejor idea, pero sirve para explorar
g1 + 
  coord_polar()



## Tipo Violin
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_violin()

## Boxplot 
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot()

## Boxplot  - horizontal
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot() +
  coord_flip()

## Boxplot agregando otra variable de inter�s
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot()

#si quieres eliminar la categor�a NA (datos faltantes) puedes correr el siguiente c�digo
datos %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot()

#Nota: eliminar los datos faltantes y realizar otro tipo de manipulaciones de datos te permitir� explorar de mejor forma tus datos... esto es todo un tema por si mismo y se podr�a abordar en otro taller..... quiz�s......�?



#################################### -- EJERCICIO -- ############################################################+
# Una de las ventajas de hacer gr�ficos con ggplot2 es la posibilidad de combinar capas geom�tricas....
# Haz un gr�fico combinando dos capas geom�tricas 




#################################################################################################################+


#
# _5.3 Agregar otra capa de informaci�n con facet_ ----
#

datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_wrap(~island)


datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_wrap(~island, scales = "free_x")


datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot() +
  facet_grid(~island, scales = "free_x", space = "free")



#Otro ejemplo de la utilidad de facets.....
datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point()


datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  facet_wrap(~sex)



#
# _5.4 Agregar t�tulos, modificaci�n de escalas y del aspecto general del gr�fico con theme() ----
#


(gra_final <- datos %>% 
  #manipulaci�n de datos
  filter(!is.na(sex)) %>% 
  #gr�fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  geom_smooth(method = "lm", alpha=.1) +
    #titulos
  labs(title="Relaci�n entre el largo y profundidad del culmen en ping�inos de la estaci�n de investigaci�n PALMER",
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


#Quieres guardar t� gr�fico???
ggsave("gr�fico_final.png", gra_final, dpi = 500, units = "cm", width = 25, height = 12)

  
#################################### -- EJERCICIO -- ############################################################+
# Siguiendo los pasos vistos en el taller y con todos los elementos del gr�fico anterior, genera uno nuevo 
# y aguardalo en tu pc 




#################################################################################################################+


  
#Nota: Esto es lo general y el punto de partida para poder hacer mejores visualizaciones y explorar tus datos con mayor profundidad.

#########################################  FIN  #########################################+



### Bonus Track

#
# Errores comunes -----
#

#Comprueba la parte izquierda de tu consola: si es un +, significa que R no cree que hayas escrito una expresi�n completa y est� esperando que la termines. En este caso, generalmente es mejor comenzar de nuevo desde cero presionando ESCAPE para cancelar lo que estabas haciendo y que quedo incompleto.

#Colocar el + en el lugar equivocado: SIEMPRE debe ubicarse al final de la l�nea, no al inicio.


#
# Donde buscar ayuda -----
#

#En la consola de R escribe ?nombre_de_la_funcion y presiona enter. Te aparecer� la ayuda oficial que tiene R de capa paquete o funci�n
#SAN GOOGLE -> una buena opci�n es copiar y pegar el mensaje de error tal cual en google o escribir lo que quieres hacer (es mejor si buscas en ingl�s). Generalmente las primeras opciones en la b�squeda te van a enviar a un foro de "Stack Overflow"..... ah� estas en el lugar correcto
#Puedes encontrar informaci�n y ejemplos de c�mo usar cada paquete o funci�n escribiendo el nombre de lo que quieres buscar agregando la letra R. Existen muchas p�ginas, blogs y talleres sobre diferentes temas relacionados a R
#En Twitter usando #Rstats


#Como no perderme en estas casi 500 l�neas de c�digo?? --> Mira en la parte superior derecha del editor (script) aparecen unas l�neas horizonatales en forma de �ndice... haz click en ellas.....




