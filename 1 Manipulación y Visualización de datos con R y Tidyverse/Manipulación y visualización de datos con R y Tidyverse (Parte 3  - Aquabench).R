###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TALLER DE INTRODUCCIÓN A R  (PARTE 3)                                 -----      
### MANIPULACIÓN Y VISUALIZACIÓN DE DATOS CON R Y TIDYVERSE               -----
### 
### Autor: Amadeo Guzmán
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#LIBRERIAS
library(tidyverse)
library(scales)
library(ggthemes)
library(ggrepel)
library(lubridate)
library(skimr)



## BASE DE DATOS

#CSV
serie.SRS <- read.csv2("C:/Users/aguzman/Desktop/Informe semanal/BD_informe_semanal.csv", header=TRUE)

glimpse(serie.SRS)
skim(serie.SRS)


#vamos a ver en detalle algunos datos que parecen extraños....(empresas y macrozona)
levels(serie.SRS$empresa)
table(serie.SRS$empresa)

levels(serie.SRS$macrozona)
table(serie.SRS$macrozona)


#Vamos a obtener información que usaremos más adelante...
#....semana actual
(semana.actual <- serie.SRS %>% 
    filter(ano==max(ano)) %>% 
    summarize(semana_actual = max(semana))
)

#.....año actual
(year.act <- serie.SRS %>% 
    filter(ano==max(ano)) %>% 
    summarize(year_act = max(ano))
)


#.....Mortalidad GENERAL semana actual SALAR
(promedio.general <- serie.SRS %>% 
  filter(ano==max(ano)) %>% 
  filter(semana==max(semana), especie=="Salar", region!="XII Región", macrozona %in% c(1,2,3,4,5,6,7,8), centro!="Colaco 1", !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
  group_by(ano, semana) %>% 
  mutate(srs= ifelse(is.na(srs),0,srs),
         Npeces = sum(num_inicio_sem, na.rm = TRUE),
         Nsrs = sum(srs, na.rm = TRUE),
         SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
  mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
         SRSproyecto = srs + sca,
         Nsrsp = sum(SRSproyecto, na.rm = TRUE),
         SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>% 
  summarize(MortSRSp.pond.general = mean(SRSp_calc, na.rm = TRUE),
            MortSRS.pond.general = mean(SRS_calc, na.rm = TRUE))
)





#
##
#### Gráficos de mortalidad anual -----------------------------
##
#


### Ejemplo 1:
(ejercicio1 <- serie.SRS %>% 
  filter(especie=="Salar", region!="XII Región", !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
  select(ano_semana, ano, semana, empresa, id_jaula, especie, macrozona, num_inicio_sem, srs, sin_causa_aparente) %>% 
  group_by(ano_semana, ano, semana) %>% 
  mutate(srs= ifelse(is.na(srs),0,srs),
         Npeces = sum(num_inicio_sem, na.rm = TRUE),
         Nsrs = sum(srs, na.rm = TRUE),
         SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
  mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
         SRSproyecto = srs + sca,
         Nsrsp = sum(SRSproyecto, na.rm = TRUE),
         SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>%
  summarize(MortSRSp.pond = mean(SRSp_calc, na.rm = TRUE),
            MortSRS.pond = mean(SRS_calc, na.rm = TRUE)) %>% 
  filter(ano > 2014) %>% 
      #gráfico
  ggplot(aes(semana, MortSRSp.pond, color=as.factor(ano))) +
  geom_line() +
  scale_color_tableau(name="Año") +
  scale_x_continuous(breaks = seq(1,53, 2), expand = c(0,0)) +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(axis.line.x = element_line())
)

ejercicio1 + 
  theme_light() +
  facet_wrap(~ano, ncol=2)



### Ejemplo 2: mejoremos el gráfico anterior
#......primero necesitamos calcular algunos datos

#mortalidad semanal salar 2020
(serie_año_acutual_salar<- serie.SRS %>% 
  filter(ano %in% c((as.integer(year.act)-1):as.integer(year.act)), especie=="Salar", region!="XII Región", !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
  select(ano_semana, ano, semana, empresa, id_jaula, especie, macrozona, num_inicio_sem, srs, sin_causa_aparente) %>% 
  group_by(ano_semana) %>% 
  mutate(srs= ifelse(is.na(srs),0,srs),
         Npeces = sum(num_inicio_sem, na.rm = TRUE),
         Nsrs = sum(srs, na.rm = TRUE),
         SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
  mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
         SRSproyecto = srs + sca,
         Nsrsp = sum(SRSproyecto, na.rm = TRUE),
         SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>%
  summarize(MortSRSp.pond = mean(SRSp_calc, na.rm = TRUE),
            MortSRS.pond = mean(SRS_calc, na.rm = TRUE)) %>% 
  mutate(año= str_extract(ano_semana, "...."),
         semana= as.numeric(str_extract(ano_semana,"[^-]+$")),
         año = as.numeric(año)) %>%
  filter(año==as.integer(year.act))
)


#mortalidad semanal salar 2019
(serie_año_anterior_salar<- serie.SRS %>% 
    filter(ano %in% c((as.integer(year.act)-2),as.integer(year.act)-1), especie=="Salar", region!="XII Región", !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
    select(ano_semana, empresa, id_jaula, especie, macrozona, num_inicio_sem, srs, sin_causa_aparente) %>% 
    group_by(ano_semana) %>% 
    mutate(srs= ifelse(is.na(srs),0,srs),
           Npeces = sum(num_inicio_sem, na.rm = TRUE),
           Nsrs = sum(srs, na.rm = TRUE),
           SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
    mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
           SRSproyecto = srs + sca,
           Nsrsp = sum(SRSproyecto, na.rm = TRUE),
           SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>%
    summarize(MortSRSp.pond = mean(SRSp_calc, na.rm = TRUE),
              MortSRS.pond = mean(SRS_calc, na.rm = TRUE)) %>% 
    mutate(año= str_extract(ano_semana, "...."),
           semana= as.numeric(str_extract(ano_semana,"[^-]+$")),
           año = as.numeric(año)) %>%
    filter(año==as.integer(year.act)-1)
)



#Grafico area (años anteriores) y linea (año actual)
(graf_area_periodo_salar_v2 <-serie.SRS %>% 
    filter(ano %in% c(2015:(as.integer(year.act)-2)), especie=="Salar", region!="XII Región", !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
    select(ano_semana, ano, semana, empresa, id_jaula, especie, macrozona, num_inicio_sem, srs, sin_causa_aparente) %>% 
    group_by(ano, semana) %>% 
    mutate(srs= ifelse(is.na(srs),0,srs),
           Npeces = sum(num_inicio_sem, na.rm = TRUE),
           Nsrs = sum(srs, na.rm = TRUE),
           SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
    mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
           SRSproyecto = srs + sca,
           Nsrsp = sum(SRSproyecto, na.rm = TRUE),
           SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>%
    summarize(MortSRSp.pond = mean(SRSp_calc, na.rm = TRUE),
              MortSRS.pond = mean(SRS_calc, na.rm = TRUE)) %>% 
    group_by(semana) %>% 
    summarize(SRS = mean(MortSRS.pond, na.rm = TRUE),
              SRSp = mean(MortSRSp.pond, na.rm = TRUE),
              SRSp_min = min(MortSRSp.pond, na.rm = TRUE),
              SRSp_max = max(MortSRSp.pond, na.rm = TRUE)) %>% 
    
      #GRÁFICO
    ggplot(aes(x=semana)) +
    geom_ribbon(aes(ymin = SRSp_min, ymax = SRSp_max), fill = "grey60", alpha=.3) +
    scale_y_continuous(labels=percent_format(accuracy = 0.01), breaks = seq(0,0.002,0.0002), limits = c(0,0.0021), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(1,53,2), limits = c(1,52), expand = c(0.005,0.005)) +
    labs(title = "Mortalidad semanal SRSp (%) en salmón del Atlántico",
         subtitle = str_glue("Comparación años {year.act} y {year.act-1}, en relación a los valores máximos / mínimos registrados durante el período 2015-{year.act-2}"),
         x="\nSemanas",
         y="Mortalidad SRSp\n")+
    geom_line(aes(semana, SRSp), color="white", alpha=.7)+
      #agregamos dato 2020
    geom_line(data = serie_año_acutual_salar, aes(semana, MortSRSp.pond), color="firebrick", lwd=1) +
    geom_point(data = filter(serie_año_acutual_salar, semana == str_glue("{semana.actual}")), aes(semana, MortSRSp.pond), color = "#bb0a21", size=2.5) +
      #agregamos dato 2019
    geom_line(data = serie_año_anterior_salar, aes(semana, MortSRSp.pond), color="steelblue", lwd=.7) +
      #modificamos el aspecto general
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey98"),
          axis.line.x = element_line(size=1, color = "black"),
          axis.ticks.x = element_line(color = "black"),
          text = element_text(color = "black")) +
        #etiqueta y punto semana actual
    geom_label_repel(data = promedio.general, aes(semana, MortSRSp.pond.general, label=str_glue(round(MortSRSp.pond.general*100,3), "%")), size=3, vjust=-1,segment.colour = "black", segment.size = .2, color="black", fill="white",alpha=.7) +
    geom_point(data = filter(serie_año_acutual_salar, semana == str_glue("{semana.actual}")), aes(semana, MortSRSp.pond), color="firebrick", size=3)
)


#guardar en formato .png
ggsave("mort_SRSp_actual_y_anterior.png", graf_area_periodo_salar_v2, dpi=600, units = "cm", width = 27, height = 12.5)





#
##
#### Heatmap macrozonas -----------------------------
##
#


(graf_calor_mz <- serie.SRS %>% 
      
    #Parte 1: Manipulación de datos
   
   filter(ano %in% c((as.integer(year.act)-1):as.integer(year.act)), 
          especie=="Salar", region!="XII Región", 
          !macrozona %in% c("", "Sin macrozona", "Sin Macrozona"), 
          !empresa %in% c("AquaChile", "Cooke Aquaculture")) %>% 
   select(ano_semana, ano, semana, especie, macrozona, num_inicio_sem, srs, sin_causa_aparente) %>%  
   group_by(macrozona, ano, semana) %>% 
   mutate(srs= ifelse(is.na(srs),0,srs),
          Npeces = sum(num_inicio_sem, na.rm = TRUE),
          Nsrs = sum(srs, na.rm = TRUE),
          SRS_calc = Nsrs/Npeces, na.rm = TRUE) %>% 
   mutate(sca= ifelse(is.na(sin_causa_aparente),0,sin_causa_aparente),
          SRSproyecto = srs + sca,
          Nsrsp = sum(SRSproyecto, na.rm = TRUE),
          SRSp_calc = Nsrsp/Npeces, na.rm = TRUE) %>%
   summarize(MortSRSp.pond = mean(SRSp_calc, na.rm = TRUE),
             MortSRS.pond = mean(SRS_calc, na.rm = TRUE)) %>% 
    
      #Parte 2: Gráfico
    
   ggplot(aes(semana, macrozona, fill=MortSRSp.pond)) +
   geom_tile() +
    
      #parte 2.1: tramas de relleno
   scale_fill_gradient2(low = "darkblue", mid = "yellow", high = "firebrick3", midpoint = 0.003, labels=percent_format(accuracy = 0.1)) +
   #scale_fill_distiller(labels=percent_format(accuracy = 0.1), type = "div", palette = "Spectral")+
   #scale_fill_viridis_c(labels=percent_format(accuracy = 0.1), option = "D")+
    
      #parte 2.2: agregar texto en cada cuadrante 
   #geom_text(aes(label = glue::glue("{round(MortSRSp.pond*100,2)}%"), color=ifelse(MortSRSp.pond>0.0006, "black", "white")), size=2.5, angle=90) +
   #scale_color_manual(values = c("black", "white"), guide=FALSE) +
    
      #parte 2.3: modificar eje x
   scale_x_continuous(breaks = seq(1,52,2)) +
    
      #parte 2.4: etiquetas
   labs(title="Mortalidad semanal SRSp (%)  - Macrozona",
        subtitle = str_glue("Salar. Años {year.act-1}-{year.act}"),
        x="Semanas", 
        y="Barrios",
        fill="Mortalidad SRSp (%)  ")+
    
      #parte 2.5: facet
   facet_grid(.~ano, scales = "free_x", space = "free") +
    
      #parte 2.6: modifiar el aspecto general del gráfico
   theme_minimal() +
   theme(strip.background = element_rect(fill = "grey70", color="grey50"),
         strip.text.x = element_text(color = "white", face = "bold"),
         panel.margin.x = unit(0.2, "lines"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line.x = element_line(size=1, color = "black"),
         axis.ticks.x = element_line(color="black"),
         axis.text.x = element_text(angle=90, vjust = .5),
         legend.position="bottom",
         legend.justification = "right",
         legend.key.width = unit(1.5,"cm")) +
    coord_cartesian(expand = FALSE)
)



#Guardar
ggsave("graf_calor_mz.png", graf_calor_mz, dpi=600, units = "cm", width = 33.5, height = 13)





