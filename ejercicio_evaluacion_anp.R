
#Script para hacer la evaluaci�n de un ANP
#El script proporciona un flujo de trabajo para determinar si un ANP puede 
#servir para reducir la deforestaci�n o no

#Autor: Dr. Daniel Auliz-Ortiz
#Laboratorio de Geograf�a de la biodiversidad, Instituto de Biolog�a
#contacto: dauliz@cieco.unam.mx


#Paquetes necesarios

#recuerda que puedes instalar el paquete que no tengas usando install.packages()

library(tidyverse)
library(RStoolbox)
library(sf)
library(terra)
library(caret)
library(rsample)
library(margins)
library(landscapemetrics)


#define tu directorio de trabajo
setwd("D:/iztacala_ejercicio/spatial")

#imagen satelital para 2015
tux_2015 <- rast("los_tuxtlas_satel_25k_2015.tif")

#puntos de referencia de las clases de cobertura del suelo
points_2015 <- read_sf("points_land_cover_2015.shp")

#Pol�gono de la reserva de la biosfera Los tuxtlas
pol_tux <- read_sf("D:/garbage/res/los_tuxtlas.shp")


#defino las clases de cobertura como factores
points_2015$class <- factor(points_2015$class)


##############################################################################################################################
##Primero vamos a hacer la clasificaci�n de las im�genes de sat�lite


set.seed(20)#siembro una semilla para poder hacer replicables los resultados
#c�digo para separar el conjunto de datos de manera aleatoria
particion <- initial_split(points_2015, prop = 0.8) #elijo una proporci�n 80% para entrenamiento, y 20% para test
train_2015 <- training(particion)
test_2015 <- testing(particion)

#reviso los balances entre las clases de cobertura
table(train_2015$class)
table(test_2015$class)

#mapeo
ggRGB(tux_2015, r = 4, g = 3, b=2, stretch = "lin", 
      coord_equal = T, geom_raster = T)+
  geom_sf(data = points_2015, aes(col = class))+
  scale_color_manual(values = c("#eeee59", "#59aeee", "#ac82e5", "#8fe72a","#146310",  "#e959ee"))
  

#Clasificaci�n supervisada con m�todo Random Forest

sc_2015 <- superClass(tux_2015, trainData = train_2015, responseCol = "class",
                 model = "rf", tuneLength = 1)

#veo el desempe�o de la clasificaci�n
sc_2015$modelFit


#Valido la clasificaci�n con los datos de test
validation <- superClass(tux_2015, trainData = train_2015, valData = test_2015,
                         responseCol = "class", model = "rf", tuneLength = 1)
validation$validation


#obtengo la capa de clasificaci�n
tux_clas_2015 <- as.factor(sc_2015$map)
levels(tux_clas_2015) <- data.frame(ID = 1:6, class_supervised = levels(points_2015$class))

#mapeo la clasificaci�n
ggR(tux_clas_2015, geom_raster = T, forceCat = T) + 
  scale_fill_manual(values = c("#eeee59", "#59aeee", "#ac82e5", "#8fe72a","#146310",  "#e959ee"))

#�puede mejorar?

#Una alternativa es fusionar clases que se parece y que representan m�s o menos lo mismo para ciertos prop�sitos

#Creo una variable nueva, class_2, dond fusiono agricultura y pastizales
points_2015 <- points_2015 %>% 
  mutate(class_2 = case_match(class,
                              "Agricultura" ~ "Agricultura/pastizal",
                              "Pastizales" ~ "Agricultura/pastizal",
                              .default = class))

points_2015


#repito los pasos anteriores pero con la clase nueva
set.seed(20)
particion <- initial_split(points_2015, prop = 0.8)
train_2015 <- training(particion)
test_2015 <- testing(particion)

table(train_2015$class_2)
table(test_2015$class_2)



sc_2015 <- superClass(tux_2015, trainData = train_2015, responseCol = "class_2",
                      model = "rf", tuneLength = 1)

sc_2015$modelFit



validation <- superClass(tux_2015, trainData = train_2015, valData = test_2015,
                         responseCol = "class_2", model = "rf", tuneLength = 1)
validation$validation


points_2015$class_2 <- factor(points_2015$class_2)


tux_clas_2015 <- as.factor(sc_2015$map)
levels(tux_clas_2015) <- data.frame(ID = 1:5, class_supervised = levels(points_2015$class_2))

map_tux_2015 <- ggR(tux_clas_2015, geom_raster = T, forceCat = T) + 
  scale_fill_manual(
    values = c("#8fe72a", "#59aeee", "#ac82e5", "#146310", "#e959ee"),
    na.value = NA  # No mostrar los NA
  ) +
  geom_sf(data = pol_tux, col= "red", fill= NA, linewidth= 1)+
  labs(title = "Cobertura del suelo a�o 2015", x= "Longit�d", y= "Latit�d",
       fill= "Clase")

map_tux_2015


# Extraer los valores de las bandas 3 y 4 para cada punto de referencia
valores_espectrales <- extract(tux_2015, points_2015)

# Combinar los valores espectrales con las clases
datos_firmas_2015 <- data.frame(
  clase = points_2015$class_2,
  banda_3 = valores_espectrales[, 3],  # Banda 3
  banda_4 = valores_espectrales[, 4]   # Banda 4
)


datos_firmas_2015


# Graficar las firmas espectrales
ggplot(datos_firmas_2015, aes(x = banda_4, y = banda_3, color = clase)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "Firmas espectrales: Banda 4 vs Banda 3",
    x = "Near Infrared (Reflectancia)",
    y = "Red (Reflectancia)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#8fe72a", "#59aeee", "#ac82e5","#146310",  "#e959ee"))

################################
#repito los pasos pero ahora para el a�o 2025

#imagen de sat�lite
tux_2025 <- rast("los_tuxtlas_satel_25k_2025.tif")

points_2025 <- read_sf("points_land_cover_2025.shp")



points_2025 <- points_2025 %>% 
  mutate(class_2 = case_match(class,
                              "Agricultura" ~ "Agricultura/pastizal",
                              "Pastizales" ~ "Agricultura/pastizal",
                              .default = class))


points_2025$class_2 <- factor(points_2025$class_2)



set.seed(20)
particion_2025 <- initial_split(points_2025, prop = 0.8)
train_2025 <- training(particion_2025)
test_2025 <- testing(particion_2025)

table(train_2025$class)
table(test_2025$class)


sc_2025 <- superClass(tux_2025, trainData = train_2025, responseCol = "class_2",
                      model = "rf", tuneLength = 1)

sc_2025$modelFit



validation_2025 <- superClass(tux_2025, trainData = train_2025, valData = test_2025,
                         responseCol = "class_2", model = "rf", tuneLength = 1)
validation_2025$validation




tux_clas_2025 <- as.factor(sc_2025$map)
levels(tux_clas_2025) <- data.frame(ID = 1:5, class_supervised = levels(points_2025$class_2))



map_tux_2025 <- ggR(tux_clas_2025, geom_raster = T, forceCat = T) + 
  scale_fill_manual(
    values = c("#8fe72a", "#59aeee", "#ac82e5", "#146310", "#e959ee"),
    na.value = NA  # No mostrar los NA
  ) +
  geom_sf(data = pol_tux, col= "red", fill= NA, linewidth= 1)+
  labs(title = "Cobertura del suelo a�o 2025", x= "Longit�d", y= "Latit�d",
       fill= "Clase")

map_tux_2015

map_tux_2025

##########################################################################################################
#Ahora el an�lisis se centrar� en el bosque, por lo que hay que reclasificar los raster para obtener
#raster de lo que es bosque y lo que no

#creo una matriz con los valores actuales y los valores nuevos que quiero obtener

mm <- matrix(
  c(
    1, 0,  # Agricultura/pastizal -> 0
    2, 0,  # Agua -> 0
    3, 1,  # Manglar -> 1
    4, 1,  # Selvas h�medas -> 1
    5, 0   # Zonas urbanas -> 0
  ),
  ncol = 2, byrow = TRUE
)

mm


tux_clas_2015_reclas <- classify(tux_clas_2015, mm)
tux_clas_2025_reclas <- classify(tux_clas_2025, mm)


#visualizo el mapa de bosque del 2015
ggR(tux_clas_2015_reclas, geom_raster = T, forceCat = T) + 
  scale_fill_manual(
    values = c("gray75", "#146310"),
    na.value = NA,  # No mostrar los NA
    labels = c( "Otras", "Bosque")) +
  geom_sf(data = pol_tux, col= "red", fill= NA, linewidth= 1)+
  labs(title = "Cobertura de bosque para el a�o 2015", x= "Longit�d", y= "Latit�d",
       fill= "Clase")+
  theme_light()

#visualizo el mapa de bosque del 2025
ggR(tux_clas_2025_reclas, geom_raster = T, forceCat = T) + 
  scale_fill_manual(
    values = c("gray75", "#146310"),
    na.value = NA,  # No mostrar los NA
    labels = c( "Otras", "Bosque")) +
  geom_sf(data = pol_tux, col= "red", fill= NA, linewidth= 1)+
  labs(title = "Cobertura de bosque para el a�o 2025", x= "Longit�d", y= "Latit�d",
       fill= "Clase")+
  theme_light()

######Ahora quiero obtener los lugares donde se h� perdido bosque

#En teor�a, si cada raster solo tiene 1 y 0 como valor, hay cuatro transiciones posibles

#2015  2025
#1 ->   1  permanece bosque
#1 ->   0  p�rdida de bosque
#0 ->   1  ganancia de bosque
#0 ->   0  permanece no bosque


#calculo d�nde hay p�rdida de bosque

perdida_bosque <- tux_clas_2015_reclas == 1 & tux_clas_2025_reclas == 0

# Convertir el resultado a valores num�ricos (TRUE = 1, FALSE = 0)
perdida_bosque <- perdida_bosque * 1


#Visualizamos los patrones de p�rdida de bosque
ggR(perdida_bosque, geom_raster = T, forceCat = T) +
  scale_fill_manual(
    values = c("gray75", "red"),
    na.value = NA,  # No mostrar los NA
    labels = c( "No perdida", "P�rdida")) +
  geom_sf(data = pol_tux, col= "black", fill= NA, linewidth= 1)+
  labs(title = "P�rdida de bosque 2015-2025", x= "Longit�d", y= "Latit�d",
       fill= "Clase")+
  theme_light()

######################################################################################
#Ahora vamos a separar lo que pasa adentro y lo que pasa afuera

#cargamos el pol�gono del buffer alrededor de la reserva
buff_tux <- read_sf("C:/Users/dauli/OneDrive/Documentos/curso/clase_iztacala/spat/buffer_los_tuxtlas.shp")


#Recortamos la capa de p�rdida de bosque dentro de la reserva
loss_in <- perdida_bosque %>% 
  crop(pol_tux) %>% 
  mask(pol_tux)

#Recortamos la capa de p�rdida de bosque fuera de la reserva
loss_out <- perdida_bosque %>% 
  crop(buff_tux) %>% 
  mask(buff_tux)

#visualizamos ambos archivos
plot(loss_in, col = c("gray", "red"))
plot(loss_out, col = c("gray", "red"))



#ahora recortamos la capa de bosque para distinguir adentro y afuera
bosque_in <- tux_clas_2015_reclas %>% 
  crop(pol_tux) %>% 
  mask(pol_tux)


bosque_out <- tux_clas_2015_reclas %>% 
  crop(buff_tux) %>% 
  mask(buff_tux)


###############################################################

#C�lculo de �reas de bosque y de p�rdida de bosque

#Utilizamos al funci�n lsm_c_ca para calcular el �rea (en hectareas) por cada clase seg�n el raster

dat_loss_in <- lsm_c_ca(loss_in) %>% #c�lculo de �reas
  filter(class == 1) %>%  #seleccionar la clase 1 que corresponde p�rdida de bosque
  select(value) %>%  #seleccionar solo el valor
  mutate(parte = "dentro") %>% #agregamos una variable que designe si pasa dentro o fuera
  rename(loss = value) #renombramos el valor como loss para saber de qu� se trata

dat_loss_in

dat_loss_out <- lsm_c_ca(loss_out) %>% 
  filter(class == 1) %>% 
  select(value) %>% 
  mutate(parte = "fuera") %>% 
  rename(loss = value)

dat_loss_out


dat_bosque_in <- lsm_c_ca(bosque_in) %>% #c�lculo de �reas
  filter(class == 1) %>% #seleccionar la clase 1 que corresponde al bosque
  select(value) %>% #seleccionar solo el valor
  mutate(parte = "dentro") %>% #agregamos una variable que designe si pasa dentro o fuera
  rename(bosque = value) #renombramos el valor como loss para saber de qu� se trata

dat_bosque_in

dat_bosque_out <- lsm_c_ca(bosque_out) %>% 
  filter(class == 1) %>% 
  select(value) %>% 
  mutate(parte = "fuera") %>% 
  rename(bosque = value)

dat_bosque_out


#reunimos los datos de las �reas fuera y adentro en un solo data frame para datos de p�rdida y de cobertura de bosque

df_loss <-bind_rows(dat_loss_in, dat_loss_out)

df_bosque <- bind_rows(dat_bosque_in, dat_bosque_out)


df_loss

df_bosque

#ahora unimos los datos de p�rdida y de bosque en un solo objeto
df_loss_b <- left_join(df_loss, df_bosque, by= "parte")

#Calculamos el porcentaje de p�rdida relativo a cuanto bosque hab�a en un comienzo
df_loss_b <- df_loss_b %>% 
  mutate(per_loss = (loss/bosque)*100)


#visualizamos en una gr�fica estos pocentajes
df_loss_b %>% 
  ggplot(aes(x= parte, y= per_loss, fill=parte))+
  geom_col()+
  theme_light()+
  labs(x= "", y= "P�rdida de bosque (%)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12))+
  scale_y_continuous(breaks = seq(0, 40, 5))


###############################################################################################################################

##Un poco de modelado

#agregamos una variable que defina si el sitio cuneta con protecci�n o no a cada parte de los pol�gonos
pol_tux$prot <- "Protected"
buff_tux$prot <- "Unprotected"

#unimos ambos pol�gonos para tener uno, peor que distinga afuera y adentro
pol_tux_full <- bind_rows(pol_tux, buff_tux) %>% 
  select(prot)

#visualizamos el pol�gono nuevo
plot(pol_tux_full["prot"])


#para modelar, tenemos que colectar informaci�n dentro de los pixeles de cada raster
#para hacer esto sembramos puntos de muestreo que nos servir�n para colectar la informaci�n espacial

#siembro semillas para hacer los resultados replicables
set.seed(123)

#siembro 5000 puntos dispuestos de manera regular del pol�gono que quiero analizar
p_regular <- st_sample(pol_tux_full, 1500, type= "regular")

p_regular %>% plot

#visualizo la estructura de los datos
p_regular

#necesito quecada punto tenga un identificador de si est� adentro (protegido) o fuera (no protegido) de la reserva
#para ello se hace una operaci�n que se llama intersecci�n, en donde los atributos de un pol�gono pasan a los puntos
#si estos puntos est�n dentro del pol�gono

p_regular <- st_intersection( pol_tux_full, p_regular)

p_regular %>% plot


#### vamos a extraer informaci�n de algunas variables que pueden ser importantes

#cargo variables
slope_tux <- rast("slope_tuxtlas.tif")

road_tux <- rast("road_dis_tuxtlas.tif")

elev_tux <- rast("elevation_tuxtlas.tif")

citi_tux <- rast("cities_dis_tuxtlas.tif")


#extraigo la informaci�n
ext_loss <- extract(perdida_bosque, p_regular)

ext_slope <- extract(slope_tux, p_regular)

ext_road <- extract(road_tux, p_regular)

ext_elev <- extract(elev_tux, p_regular)

ext_citi <- extract(citi_tux, p_regular)

head(ext_loss)

#Organizo las variables extraidas
df_variables <- data.frame(loss = ext_loss$class_supervised, 
           slope = ext_slope$slope_tuxtlas,
           road = ext_road$road_dis_tuxtlas,
           elev = ext_elev$elevation_tuxtlas,
           citi = ext_citi$cities_dis_tuxtlas) %>%
  as_tibble()

df_variables


#junto los datos extraidos con los puntos de muestreo
df_datos_loss <- bind_cols(p_regular, df_variables) %>% 
  as_tibble() %>% 
  select(-geometry)

df_datos_loss


#grafico los promedios y error est�ndar de p�rdida de bosque
df_datos_loss %>% 
  ggplot(aes(x=prot, y= loss*100, fill=prot))+
  stat_summary(fun.data = mean_se)+
  theme_light()+
  labs(x= "", y="Porcentaje de p�rdida de bosque")


#designo los niveles de la variable
df_datos_loss$prot <- factor(df_datos_loss$prot, levels = c("Unprotected", "Protected"))

levels(df_datos_loss$prot)

#realizo un modelo linear generalizado para predecir la probabilidad de p�rdida de bosque
modelo_01 <- glm(loss ~ prot +slope+road+elev+citi, data = df_datos_loss, family = "binomial")

summary(modelo_01)


cplot(modelo_01, "prot")
cplot(modelo_01, "slope")
cplot(modelo_01, "elev")
cplot(modelo_01, "citi")

#Finalmente calculamos el efecto marginal d ela protecci�n
#Ello es el efecto en prevenir deforestaci�n por parte de la reserva dejando fijas el resto de variables
margins(modelo_01, variables = "prot") 

writeRaster(perdida_bosque, "resultados/los_tux_loss_2015_2025.tif")

writeRaster(tux_clas_2025_reclas, "resultados/los_tux_bosque_2025.tif")



