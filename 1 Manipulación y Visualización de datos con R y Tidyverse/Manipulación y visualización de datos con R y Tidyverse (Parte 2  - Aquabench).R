###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TALLER DE INTRODUCCIÓN A R (PARTE 2)                                   -----      
### MANIPULACIÓN Y VISUALIZACIÓN DE DATOS CON R Y TIDYVERSE                -----
### 
### Autor: Amadeo Guzmán
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Hoy vamos a aprender a manipular bases datos con las librerias que forman parte de Tidyverse, principalmente "dplyr"
#prenderemos a usar 6 funciones (existen muchas funciones que no vamos a ver hoy, pero estas son algunas de las más importantes)

#   - select()      -> Seleccionar un subconjunto de variables de nuestra base de datos
#   - filter()      -> Seleccionar las filas que cumplen con una o más condiciones ( ==, >=, <=, <, >, !=)
#   - mutate()      -> Crear nuevas variables o modificar las existentes
#   - group_by()    -> Agrupar datos en base a una(s) característica(s)
#   - summarize()   -> Aplicar cálculos a diferentes subconjuntos de datos
#   - arrange()     -> Ordenar resultados de formas ascendente o descendente

#Tambien aprenderemos a vincular estas funciones y los gráficos de ggplot mediante el uso del operador pipe %>%



#
##
###  MANIPULACIÓN Y TRANSFORMACIÓN DE DATOS -----------------------------------------------------------------------
##
#


#librerias
library(tidyverse)
library(skimr)


#Datos
starwars   #(libreria precargada en el paquete dplyr)
?starwars   #obtener ayuda en R

names(starwars)

#Explorar base de datos
dim(starwars)
glimpse(starwars)
skim(starwars)


#---------------+
### select() ----
#---------------+
#Seleccionar un subconjunto de variables de nuestra base de datos
?select

#Ejemplo1: Seleccionamos variables especificando sus nombres
select(starwars, "name", "species", "height")

#Ejemplo2: seleccionamos en base a posición
select(starwars, 1,2,3)   #Seleccionamos las 3 primeras variables
select(starwars, 1,3:6)   #Seleccionamos la variable 1 y despues de la 3 a la 6

#Ejemplo3: eliminamos 1 o más variables
select(starwars, -12, -13, -14)
select(starwars, -c(12,13,14))


#ejemplo4: seleccionamos en base a alguna caracteristica en el nombre de la variable
select(starwars, contains("color"))   #seleccionamos variables que contengan la palabra "color"
select(starwars, starts_with("h"))    #seleccionamos variables que comienzan con "h"
select(starwars, 1, ends_with("r"))   #seleccionamos la primera variable de la base de datos y las variables que terminan con "r"


#Ejemplo5: cambiamos el orden y eliminamos algunas variables. 
#Además, vamos a guardar este proceso en el objeto datos... será nuestra nueva base de datos
(datos <- select(starwars, "name", "sex", "homeworld", everything(), -c(12,13,14))
)

#comparemos....
starwars
datos



#---------------+
### filter() -----
#---------------+
#Seleccionar las filas que cumplen con alguna condición( ==, >=, <=, <, >, !=)
?filter

#algunos ejemplos de como podemos aplicar filtros en la base de datos
filter(datos, species == "Human")
filter(datos, species != "Human") #para eliminar un elemento ocupamos !=
filter(datos, mass >= 1000)


### filtrar en base a multiples variables o condiciones
#&
filter(datos, hair_color == "black", sex == "female")  #Utilizar la coma "," entre cada variable es equivalente a la expresión "&"

#o
filter(datos, hair_color == "black" | sex == "female") #o 


#Varias condiciones dentro de una variable...
#en este caso aplicaremos el filtro en nuestra base de datos "datos"
#y lo guardaremos en otro objeto

(datos_con_filtro <- filter(datos, homeworld %in% c("Alderaan", "Tatooine", "Naboo")) #con %in% podemos seleccionar diferentes elementos de una variable
)


############################# EJERCICIO ##################################
# selecciona las filas que corresponden a humanos con altura mayor a 180
#
#
##########################################################################



#---------------+
### mutate() -----
#---------------+
#crear nuevas variables o sobreescribir las existentes... por defecto siempre se agregan al final
?mutate

#crear una nueva variable numérica "IMC"
#guardamos el resultado en un nuevo objeto
(datos_con_filtro_imc <- mutate(datos_con_filtro, IMC = mass/height))

#crear una nueva variable categórica
(datos_con_filtro_imc_human_cat <- mutate(datos_con_filtro_imc, species_human = if_else(species == "Human", "Sí", "No"))
)

#IMPORTANTE: En este proceso de seleccionar (select), filtrar (filter) y crear nuevas variables (mutate)
#hemos generado una serie de nuevas bases de datos que vamos guardando en diferentes objetos... 
#lo cual en un flujo de trabajo no es muy practico.... para solucionar este problema existe un operador
#el cual combina las salidas de una función con las entradas de una siguiente función -> pipe %>%



#-----------------------------+
# Uso del operador pipe  %>% -----
#-----------------------------+
# %>% este operador se podría traducir como "luego"... vincula lo que se va haciendo en cada función dentr de un mismo flujo de trabajo

starwars %>%  
  filter(homeworld=="Naboo")

starwars %>%  
  filter(homeworld=="Naboo") %>% 
  select("name", "species", everything(), -c("films", "vehicles", "starships"))



#replicaremos todos los procesos que hicimos anteriormente (select, filter y mutate) en un solo flujo de trabajo con %>%
#..... y lo guardaremos en el objeto "datos_ok"

(datos_ok <- starwars %>% 
  select("name", "species", everything(), -c("films", "vehicles", "starships")) %>% 
  filter(homeworld %in% c("Alderaan", "Tatooine", "Naboo")) %>% 
  mutate(IMC = mass/height,
         species_human = if_else(species == "Human", "Sí", "No"))
)



#-----------------------------+
### group_by() + summarize() -----
#-----------------------------+
#las funciones group_by() y summarize() nos permiten aplicar calculos a diferetes subconjuntos de datos, 
#generando un nuevo data frame con estos resultados
?group_by
?summarize


#Ejemplo1: determinar el promedio de altura de los habitantes de Naboo por genero
starwars %>%  
  filter(homeworld=="Naboo") %>% 
  group_by(sex) %>% 
  summarize(total= mean(height, na.rm=TRUE)) 


#################### Ejercicio ###################################################+
# Determinar el promedio de altura de los humanos por planeta de origen y sexo
#
#
##################################################################################+


# determinar la mediana, percentil 25 y percentil 75 de peso por especie
starwars %>% 
  filter(species %in% c("Droid", "Human", "Wookiee")) %>% #aplicaremos el ejercicio a 3 especies
  group_by(species) %>% 
  summarise(mediana= median(mass, na.rm = TRUE),
            p25 = quantile(mass, 0.25, na.rm=TRUE),
            p75 = quantile(mass, 0.75, na.rm=TRUE)
            )


# aplicando diferentes calculos en varias columnas con accross()
starwars %>% 
  filter(!is.na(species)) %>% 
  group_by(species) %>%
  summarise(across(c("height", "mass"), mean, na.rm = TRUE),
            n = n()) %>% 
  filter(n >= 2)


starwars %>% 
  filter(!is.na(species)) %>% 
  group_by(species) %>%
  summarise(across(c("height", "mass"),list(media = mean, 
                                            mediana = median, 
                                            mínimo = min, 
                                            máximo = max, 
                                            desv.estd = sd), na.rm = TRUE),
            n = n()) %>% 
  filter(n >= 2) #filtro de salida para dejar solo las especies con al menos 2 personajes



#----------------+
### arrange()  -----
#----------------+
#Ordenar resultados de formas ascendente o descendente (por defecto es en forma ascendente)
?arrange

#obtener el IMC por especie y ordenarlo en forma ascendente
starwars %>%
  filter(!is.na(species)) %>%    #filtramos valores ausentes en variable especie    
  mutate(IMC = mass/height) %>%  #creamos una nueva variable IMC
  group_by(species) %>%          #agrupamos por especie
  summarize(promedio.IMC = mean(IMC, na.rm = TRUE))  %>%  #obtenemos el promedio de IMC por especie
  arrange(promedio.IMC)          #ordenamos los resultados de foma ascendente (IMC)


#descendente + ggplot
starwars %>%
  filter(!is.na(species)) %>% 
  mutate(IMC = mass/height) %>% 
  group_by(species) %>% 
  summarize(promedio.IMC = mean(IMC, na.rm = TRUE)) %>% 
  arrange(desc(promedio.IMC)) %>% 
  filter(!is.na(promedio.IMC)) %>% 
   #gráfico
  ggplot(aes(fct_reorder(species, promedio.IMC), promedio.IMC)) +
  geom_col(aes(fill=if_else(promedio.IMC > mean(promedio.IMC), "firebrick", "grey50"))) +
  scale_fill_manual(values = c("firebrick", "grey70")) +
  coord_flip(expand = FALSE) +
  theme_minimal() +
  theme(axis.line.x = element_line(),
        axis.ticks = element_line(),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_hline(aes(yintercept = mean(promedio.IMC)), lty=2, color="darkblue") +
  geom_text(aes(label=round(promedio.IMC,1)), size=2.5, hjust=1.5, vjust=0.25, color="white")




