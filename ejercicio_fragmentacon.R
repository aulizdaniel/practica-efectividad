


#Script para hacer la evaluaci�n de un ANP
#El script proporciona un flujo de trabajo para determinar si un ANP puede 
#servir para reducir la fragemntaci�n o no

#Autor: Dr. Daniel Auliz-Ortiz
#Laboratorio de Geograf�a de la biodiversidad, Instituto de Biolog�a
#contacto: dauliz@cieco.unam.mx


#Paquetes necesarios

#recuerda que puedes instalar el paquete que no tengas usando install.packages()

library(tidyverse)
library(sf)
library(terra)
library(margins)
library(landscapemetrics)
library(exactextractr)



#define tu directorio de trabajo
setwd("D:/iztacala_ejercicio/spatial")

bosque <- rast("resultados/los_tux_bosque_2025.tif")



#Pol�gono de la reserva de la biosfera Los tuxtlas
pol_tux <- read_sf("D:/garbage/res/los_tuxtlas.shp")

#cargamos el pol�gono del buffer alrededor de la reserva
buff_tux <- read_sf("C:/Users/dauli/OneDrive/Documentos/curso/clase_iztacala/spat/buffer_los_tuxtlas.shp")



#agregamos una variable que defina si el sitio cuneta con protecci�n o no a cada parte de los pol�gonos
pol_tux$prot <- "Protected"
buff_tux$prot <- "Unprotected"

#unimos ambos pol�gonos para tener uno, peor que distinga afuera y adentro
pol_tux_full <- bind_rows(pol_tux, buff_tux) %>% 
  select(prot)


pol_tux_full


#siembro semillas para hacer los resultados replicables
set.seed(123)

#siembro 30 puntos dispuestos al azar en el pol�gono que quiero analizar
p_in <- st_sample(pol_tux, 30, type= "random")

#visualizamos los puntos
plot(p_in)

#siembro semillas para hacer los resultados replicables
set.seed(123)

#siembro 30 puntos dispuestos al azar en el pol�gono que quiero analizar
p_out <- st_sample(buff_tux, 30, type= "random")

#visualizamos los puntos
plot(p_out)


#Necesitamos que cada punto tenga informaci�n de si est� afuera o adentro de la reserva
#para eso hacemos la operaci�n de intersecci�n que le confiere los atributos del pol�gono a los puntos
p_in <- st_intersection( pol_tux_full, p_in)

p_out <- st_intersection( pol_tux_full, p_out)

#visualizamos que tenga los atributos
p_in %>% plot

p_out%>% plot


#realizamos buffers alrededor de cada punto con un radio de 3 km
buff_in <- st_buffer(p_in, 3000)

#visualizamos los buffer
buff_in %>% plot

buff_out<- st_buffer(p_out, 3000)

buff_out %>% plot

#para tener certeza de lo que pasa en cada buffer les asingamos un identificador num�rico �nico (id)
#n�meros del 1 al 30 como identificador para los buffer dentro
buff_in$id <- 1:30

#n�meros del 31 al 60 como identificador para los buffer fuera
buff_out$id <- 31:60


#para agilizar el flujo de trabajo vamos a crear funciones que nos dejan hacer m�ltiples pasos en poco tiempo y l�neas de c�digo

#la primer funci�n permite recortar el raster de bosque a las dimensiones de los buffer, se llama cortar

cortar <- function(pol, ra){
  gg <- ra %>% 
    crop(pol) %>% #recorta a la extensi�n
    mask(pol)     #aplica una m�scara
  
  return(gg)      #devuelve el r�ster recortado
}

#la segunda funci�n permite medir el n�mero de parches de bosque en cada buffer, se llama medir_fragment
medir_fragment <- function(pol, ra, part= "none"){
  
  gg <- cortar(pol, ra)  #recorta el r�ster
  
  df_pol <- pol %>%      #convierte el pol�gono en data frame
    as.data.frame()
  
  df <- lsm_c_np(gg) %>%   #mide el n�mero de parches
    mutate(id = df_pol$id,   #agrego identificadores �nicos
           prot = df_pol$prot) %>% 
    filter(class == 1) %>%  #filtro lo que pasa solo en el bosque
    select(id, prot, value) %>%  #ordenos mis datos
    mutate(parte = part) %>% #agrego la parte que estoy midiendo
    rename(np = value) #cambio el nombre a la varaiable a np (n�mero de parches)
  
  return(df)   #devuelvo el dato completo
  
}

#Podr�a aplicar la funci�n manualmente a cada uno de los 60 paisajes de bosque, pero eso levar�a tiempo
#para agilizarlo R puede aplicar una misma funci�n a una lista, o serie de objetos (pol�gonos en este caso
#para ello utilizamos la funci�n map

#aqu� lo que se pide es aplicar cortar a cada rengl�n de buffer, esto hace que recorte el r�ster en cada buffer
paisajes_in<- map(1:nrow(buff_in), ~ cortar(buff_in[.x, ], bosque)) 

paisajes_out<- map(1:nrow(buff_in), ~ cortar(buff_out[.x, ], bosque)) 



#pueden visualizar los paisajes de ls eiguiente forma. Si cambian el n�mero en los corchetes pueden
#cambiar el raster que visualizan, como son 30 dentro y 30 fuera, pueden cambiar hasta el 30 en cada parte

plot(paisajes_in[[1]], col = c("gray", "darkgreen"))

plot(paisajes_out[[7]], col = c("gray", "darkgreen"))


#con las siguientes l�neas vamos a calcular el n�mero de parche spor cada paisaje y lo va a devolver todo
#en un formato que es muy comprensible
fragment_in<- map(1:nrow(buff_in), ~ medir_fragment(buff_in[.x, ], bosque, part ="dentro")) %>% 
  bind_rows()

fragment_out <- map(1:nrow(buff_out), ~ medir_fragment(buff_out[.x, ], bosque, part= "fuera")) %>% 
  bind_rows()


#visualizo los datos
fragment_in

#visualizo los datos
fragment_out

#juntos las mediciones afuera ya dentro en un solo objeto
df_frag <- bind_rows(fragment_in, fragment_out)


df_frag

#visualizo si hay diferencias en los n�meros de parches en ambos sitios
df_frag %>% 
  ggplot(aes(x= parte, y= np, fill=parte))+
  geom_boxplot()

###################################
#vamos acalcular cu�l es el promedio de diferentes variables en cada uno de los buffer o pasiajes


#para ello primero tengo que unir los paisajes en un solo objeto
buff_all <- bind_rows(buff_in, buff_out)

#la ventaja es que ya tienen identificadores �nicos y puedo ver si cada uno es protejido o no
buff_all


#cargo variables
slope_tux <- rast("slope_tuxtlas.tif") #pendiente

road_tux <- rast("road_dis_tuxtlas.tif")  #distancia a carreteras

elev_tux <- rast("elevation_tuxtlas.tif") #altitud

citi_tux <- rast("cities_dis_tuxtlas.tif") #distancia a ciudades


#ahora vamos acrear una funci�n que haga uso de la funci�n exact_extract para calcular promedios de un raster en una zona
#sin embargo, la funci�n que queremos nos pedmite editar muchas cosas, y saber a qu� paisaje se refiere cada medici�n (id)
#adem�s de etiquetar qu� variable estamos midiendo

promedio_raster <- function(pol, ra, ch = "var"){
  
  df_pol <- pol %>% as.data.frame() #convierto el pol�gono a dataframe
  
  prom <- data.frame(exact_extract(ra, pol, "mean"), id= df_pol$id) #creo un df con el valor promedio del raster y el id
  
  colnames(prom) <- c(ch, "id") #le pongo nombre a la variable que estoy midiendo, y el id lo dejo como id

  return(prom) #devuelvo la medici�n
}


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_slope <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], slope_tux, "slope")) %>% 
  bind_rows()

df_slope

#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_road <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], road_tux, "road")) %>% 
  bind_rows()

df_road


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_elev <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], elev_tux, "elev")) %>% 
  bind_rows()

df_elev


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_citi <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], citi_tux, "cities")) %>% 
  bind_rows()

df_citi

#juntamos toda la informaci�n en un solo objeto, n�mero de parches y variables explicativas
df_frag_full <- left_join(df_frag, df_slope, by= "id") %>% 
  left_join(df_road) %>% 
  left_join(df_elev) %>% 
  left_join(df_citi)

df_frag_full



#designo los niveles de la variable
df_frag_full$prot <- factor(df_frag_full$prot, levels = c("Unprotected", "Protected"))

#constanto los niveles
levels(df_frag_full$prot)

#realizo un modelo linear generalizado para ver qu� explica mejor el n�mero de parches de bosque.
# Uso distribuci�n de poisson por tratarse de datos de conteo
modelo_01 <- glm(np ~ prot +slope+road+elev+cities, data = df_frag_full, family = "poisson")

summary(modelo_01)#resultados del modelo


#visualizo los efectos de cada variable
cplot(modelo_01, "prot")
cplot(modelo_01, "slope")
cplot(modelo_01, "elev")
cplot(modelo_01, "cities")

#Finalmente calculamos el efecto marginal de la protecci�n
#Ello es el efecto en mantener paisajes menos fragmentados  por parte de la reserva dejando fijas el resto de variables
margins(modelo_01, variables = "prot") 




