#Paquetes
library(tidyverse)

#Directorio
data <- readRDS("C:/Users/data.rds")

data<-data%>%
  filter(Caminos_habituales==T)%>%
  filter(Año==2022)%>%
  filter(Caminos=="Portugues"|Caminos=="Costa")

data<-data%>%
  filter(!is.na(distancia))%>%
  mutate(Portugues_distancia=case_when(
    distancia<200 ~1,
    distancia>=200 ~0,
  ))

#Muestra
cluster1<-data%>%
  filter(cluster_inglehart==1)

cluster2<-data%>%
  filter(cluster_inglehart==2)

cluster3<-data%>%
  filter(cluster_inglehart==3)

cluster4<-data%>%
  filter(cluster_inglehart==4)

cluster5<-data%>%
  filter(cluster_inglehart==5)

cluster6<-data%>%
  filter(cluster_inglehart==6)

cluster7<-data%>%
  filter(cluster_inglehart==7)

cluster8<-data%>%
  filter(cluster_inglehart==8)

# Dividir en grupos aleatorios
set.seed(123)  # Establecer una semilla para reproducibilidad

grupo1_cluster1 <- cluster1 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster1 <- cluster1 %>% anti_join(grupo1_cluster1) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster1 <- cluster1 %>% anti_join(grupo1_cluster1) %>% anti_join(grupo2_cluster1)  # 10% de los datos

#agruparmos por % los grupos por cada Cluster
grupo1_cluster2 <- cluster2 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster2 <- cluster2 %>% anti_join(grupo1_cluster2) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster2 <- cluster2 %>% anti_join(grupo1_cluster2) %>% anti_join(grupo2_cluster2)  # 10% de los datos

grupo1_cluster3 <- cluster3 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster3 <- cluster3 %>% anti_join(grupo1_cluster3) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster3 <- cluster3 %>% anti_join(grupo1_cluster3) %>% anti_join(grupo2_cluster3)  # 10% de los datos

grupo1_cluster4 <- cluster4 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster4 <- cluster4 %>% anti_join(grupo1_cluster4) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster4 <- cluster4 %>% anti_join(grupo1_cluster4) %>% anti_join(grupo2_cluster4)  # 10% de los datos

grupo1_cluster5 <- cluster5 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster5 <- cluster5 %>% anti_join(grupo1_cluster5) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster5 <- cluster5 %>% anti_join(grupo1_cluster5) %>% anti_join(grupo2_cluster5)  # 10% de los datos

grupo1_cluster6 <- cluster6 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster6 <- cluster6 %>% anti_join(grupo1_cluster6) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster6 <- cluster6 %>% anti_join(grupo1_cluster6) %>% anti_join(grupo2_cluster6)  # 10% de los datos

grupo1_cluster7 <- cluster7 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster7 <- cluster7 %>% anti_join(grupo1_cluster7) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster7 <- cluster7 %>% anti_join(grupo1_cluster7) %>% anti_join(grupo2_cluster7)  # 10% de los datos

grupo1_cluster8 <- cluster8 %>% sample_frac(0.7)  # 70% de los datos
grupo2_cluster8 <- cluster8 %>% anti_join(grupo1_cluster8) %>% sample_frac(2/3)  # 20% de los datos
grupo3_cluster8 <- cluster8 %>% anti_join(grupo1_cluster8) %>% anti_join(grupo2_cluster8)  # 10% de los datos

#Modelo
#Aplicamos el modelos logit para el 70% de los datos, nuestro set de entrenamiento
model_cluster1<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster1,family = binomial(link = "logit"))
model_cluster2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster2,family = binomial(link = "logit"))
model_cluster3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster3,family = binomial(link = "logit"))
model_cluster4<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster4,family = binomial(link = "logit"))
model_cluster5<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster5,family = binomial(link = "logit"))
model_cluster6<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster6,family = binomial(link = "logit"))
model_cluster7<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster7,family = binomial(link = "logit"))
model_cluster8<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_cluster8,family = binomial(link = "logit"))

#Imprimir los valores reales y la media
mean(model_cluster1$data$Portugues_distancia)
mean(model_cluster2$data$Portugues_distancia)
mean(model_cluster3$data$Portugues_distancia)
mean(model_cluster4$data$Portugues_distancia)
mean(model_cluster5$data$Portugues_distancia)
mean(model_cluster6$data$Portugues_distancia)
mean(model_cluster7$data$Portugues_distancia)
mean(model_cluster8$data$Portugues_distancia)

#imprimir los valores del fit de la función y la media
mean(model_cluster1$fitted.values)
mean(model_cluster2$fitted.values)
mean(model_cluster3$fitted.values)
mean(model_cluster4$fitted.values)
mean(model_cluster5$fitted.values)
mean(model_cluster6$fitted.values)
mean(model_cluster7$fitted.values)
mean(model_cluster8$fitted.values)

# Crear una nueva ventana de gráficos
dev.new()
# Crear un gráfico vacío
plot(1, 1, type = "n", xlim = range(c(fitted.values(model_cluster1), grupo1_cluster1$Portugues_distancia)), ylim = c(0, 1), xlab = "Valores ajustados", ylab = "Valores reales")
# Agregar los valores reales y ajustados para el primer modelo
points(fitted.values(model_cluster8), grupo1_cluster8$Portugues_distancia, col = "brown", pch = 16)
lines(sort(fitted.values(model_cluster8)), sort(grupo1_cluster8$Portugues_distancia), col = "brown", lty = 1)

################# VALIDATION SET ###########################

#vamos a mirar cuales son los datos del grupo dos para comparar con nuestra prediccion
model_01_cluster1<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster1,family = binomial(link = "logit"))
model_01_cluster2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster2,family = binomial(link = "logit"))
model_01_cluster3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster3,family = binomial(link = "logit"))
model_01_cluster4<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster4,family = binomial(link = "logit"))
model_01_cluster5<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster5,family = binomial(link = "logit"))
model_01_cluster6<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster6,family = binomial(link = "logit"))
model_01_cluster7<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster7,family = binomial(link = "logit"))
model_01_cluster8<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_cluster8,family = binomial(link = "logit"))

#imprimos los valores de 0 o 1 que nos dan en el 20%
model_01_cluster1$data$Portugues_distancia
mean(model_01_cluster1$data$Portugues_distancia) 
mean(model_01_cluster2$data$Portugues_distancia)
mean(model_01_cluster3$data$Portugues_distancia)
mean(model_01_cluster4$data$Portugues_distancia)
mean(model_01_cluster5$data$Portugues_distancia)
mean(model_01_cluster6$data$Portugues_distancia)
mean(model_01_cluster7$data$Portugues_distancia)
mean(model_01_cluster8$data$Portugues_distancia)
# Aplicar el modelo a grupo2
grupo1_pred <- predict(model_cluster1, newdata = grupo2_cluster1, type = "response")
grupo2_pred <- predict(model_cluster2, newdata = grupo2_cluster2, type = "response")
grupo3_pred <- predict(model_cluster3, newdata = grupo2_cluster3, type = "response")
grupo4_pred <- predict(model_cluster4, newdata = grupo2_cluster4, type = "response")
grupo5_pred <- predict(model_cluster5, newdata = grupo2_cluster5, type = "response")
grupo6_pred <- predict(model_cluster6, newdata = grupo2_cluster6, type = "response")
grupo7_pred <- predict(model_cluster7, newdata = grupo2_cluster7, type = "response")
grupo8_pred <- predict(model_cluster8, newdata = grupo2_cluster8, type = "response")
# Imprimir los resultados
mean(grupo1_pred) 
mean(grupo2_pred)
mean(grupo3_pred)
mean(grupo4_pred)
mean(grupo5_pred)
mean(grupo6_pred)
mean(grupo7_pred)
mean(grupo8_pred)
#hallar la diferencia entre lo empirico y lo predicho
q_cluster1 <- mean(model_01_cluster1$data$Portugues_distancia) - mean(grupo1_pred)
print(q_cluster1) 
q_cluster2 <- mean(model_01_cluster2$data$Portugues_distancia) - mean(grupo2_pred)
print(q_cluster2)
q_cluster3 <- mean(model_01_cluster3$data$Portugues_distancia) - mean(grupo3_pred)
print(q_cluster3)
q_cluster4 <- mean(model_01_cluster4$data$Portugues_distancia) - mean(grupo4_pred)
print(q_cluster4)
q_cluster5 <- mean(model_01_cluster5$data$Portugues_distancia) - mean(grupo5_pred)
print(q_cluster5)
q_cluster6 <- mean(model_01_cluster6$data$Portugues_distancia) - mean(grupo6_pred)
print(q_cluster6)
q_cluster7 <- mean(model_01_cluster7$data$Portugues_distancia) - mean(grupo7_pred)
print(q_cluster7)
q_cluster8 <- mean(model_01_cluster8$data$Portugues_distancia) - mean(grupo8_pred)
print(q_cluster8)

#############################  CONJUNTO DE COMPROBACION  ####################################

#TraBajar con el 10% de cada cluster que nos queda. 1º vamos a hallar la probabilidad 
model_02_cluster1<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster1,family = binomial(link = "logit"))
model_02_cluster2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster2,family = binomial(link = "logit"))
model_02_cluster3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster3,family = binomial(link = "logit"))
model_02_cluster4<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster4,family = binomial(link = "logit"))
model_02_cluster5<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster5,family = binomial(link = "logit"))
model_02_cluster6<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster6,family = binomial(link = "logit"))
model_02_cluster7<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster7,family = binomial(link = "logit"))
model_02_cluster8<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_cluster8,family = binomial(link = "logit"))

#probabilidades empíricas de ese 10%
mean(model_02_cluster1$data$Portugues_distancia)
mean(model_02_cluster2$data$Portugues_distancia)
mean(model_02_cluster3$data$Portugues_distancia)
mean(model_02_cluster4$data$Portugues_distancia)
mean(model_02_cluster5$data$Portugues_distancia)
mean(model_02_cluster6$data$Portugues_distancia)
mean(model_02_cluster7$data$Portugues_distancia)
mean(model_02_cluster8$data$Portugues_distancia)

#ahora lo que vamos a hacer va a ser calcular nuestra f(l), nuestra función logit pero corregirla con los q(L)
grupo1_pred_grupo3 <- predict(model_cluster1, newdata = grupo3_cluster1, type = "response")
grupo2_pred_grupo3 <- predict(model_cluster2, newdata = grupo3_cluster2, type = "response")
grupo3_pred_grupo3 <- predict(model_cluster3, newdata = grupo3_cluster3, type = "response")
grupo4_pred_grupo3 <- predict(model_cluster4, newdata = grupo3_cluster4, type = "response")
grupo5_pred_grupo3 <- predict(model_cluster5, newdata = grupo3_cluster5, type = "response")
grupo6_pred_grupo3 <- predict(model_cluster6, newdata = grupo3_cluster6, type = "response")
grupo7_pred_grupo3 <- predict(model_cluster7, newdata = grupo3_cluster7, type = "response")
grupo8_pred_grupo3 <- predict(model_cluster8, newdata = grupo3_cluster8, type = "response")

mean(grupo1_pred_grupo3)
mean(grupo2_pred_grupo3)
mean(grupo3_pred_grupo3)
mean(grupo4_pred_grupo3)
mean(grupo5_pred_grupo3)
mean(grupo6_pred_grupo3)
mean(grupo7_pred_grupo3)
mean(grupo8_pred_grupo3)

#guardamos la suma de las variables:
print(factor_utilidad_atraccion_01 <- mean(grupo1_pred_grupo3) + q_cluster1)
print(factor_utilidad_atraccion_02 <- mean(grupo2_pred_grupo3) + q_cluster2)
print(factor_utilidad_atraccion_03 <- mean(grupo3_pred_grupo3) + q_cluster3)
print(factor_utilidad_atraccion_04 <- mean(grupo4_pred_grupo3) + q_cluster4)
print(factor_utilidad_atraccion_05 <- mean(grupo5_pred_grupo3) + q_cluster5)
print(factor_utilidad_atraccion_06 <- mean(grupo6_pred_grupo3) + q_cluster6)
print(factor_utilidad_atraccion_07 <- mean(grupo7_pred_grupo3) + q_cluster7)
print(factor_utilidad_atraccion_08 <- mean(grupo8_pred_grupo3) + q_cluster8)