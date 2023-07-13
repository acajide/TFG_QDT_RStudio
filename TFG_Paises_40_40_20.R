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
#En este código, utilizamos el operador %>% para encadenar las operaciones. 
#La función count() cuenta las ocurrencias de cada valor único en la columna "Pais" y el parámetro sort = TRUE 
#indica que queremos ordenar los resultados de mayor a menor.
#El resultado se almacenará en la variable conteo_paises, 
#que contendrá una tabla con dos columnas: "Pais" (los diferentes nombres de países) y "n" (el número de veces que se repite cada país).
conteo_paises <- data %>%
  count(Pais, sort = TRUE)

#Muestra
pais01<-data%>%
  filter(Pais=='España')
pais02<-data%>%
  filter(Pais=='Portugal')
pais03<-data%>%
  filter(Pais=='Alemania')
pais04<-data%>%
  filter(Pais=='Italia')
pais05<-data%>%
  filter(Pais=='Estados Unidos')
pais06<-data%>%
  filter(Pais=='Brasil')
pais07<-data%>%
  filter(Pais=='República Checa')
pais08<-data%>%
  filter(Pais=='Holanda')
pais09<-data%>%
  filter(Pais=='Reino Unido')
pais10<-data%>%
  filter(Pais=='Polonia')

# Dividir en grupos aleatorios
set.seed(123)  # Establecer una semilla para reproducibilidad
#ESPAÑA
grupo1_pais01 <- pais01 %>% sample_frac(0.4)  # 40% de los datos
grupo2_pais01 <- pais01 %>% anti_join(grupo1_pais01) %>% sample_frac(4/6)  # 40% de los datos
grupo3_pais01 <- pais01 %>% anti_join(grupo1_pais01) %>% anti_join(grupo2_pais01)  # 20% de los datos
#PORTUGAL
grupo1_pais02 <- pais02 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais02 <- pais02 %>% anti_join(grupo1_pais02) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais02 <- pais02 %>% anti_join(grupo1_pais02) %>% anti_join(grupo2_pais02)  # 10% de los datos
#ALEMANIA
grupo1_pais03 <- pais03 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais03 <- pais03 %>% anti_join(grupo1_pais03) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais03 <- pais03 %>% anti_join(grupo1_pais03) %>% anti_join(grupo2_pais03)  # 10% de los datos
#ITALIA
grupo1_pais04 <- pais04 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais04 <- pais04 %>% anti_join(grupo1_pais04) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais04 <- pais04 %>% anti_join(grupo1_pais04) %>% anti_join(grupo2_pais04)  # 10% de los datos
#ESTADOS UNIDOS
grupo1_pais05 <- pais05 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais05 <- pais05 %>% anti_join(grupo1_pais05) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais05 <- pais05 %>% anti_join(grupo1_pais05) %>% anti_join(grupo2_pais05)  # 10% de los datos
#BRASIL
grupo1_pais06 <- pais06 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais06 <- pais06 %>% anti_join(grupo1_pais06) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais06 <- pais06 %>% anti_join(grupo1_pais06) %>% anti_join(grupo2_pais06)  # 10% de los datos
#REPLÚBLICA CHECA
grupo1_pais07 <- pais07 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais07 <- pais07 %>% anti_join(grupo1_pais07) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais07 <- pais07 %>% anti_join(grupo1_pais07) %>% anti_join(grupo2_pais07)  # 10% de los datos
#HOLANDA
grupo1_pais08 <- pais08 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais08 <- pais08 %>% anti_join(grupo1_pais08) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais08 <- pais08 %>% anti_join(grupo1_pais08) %>% anti_join(grupo2_pais08)  # 10% de los datos
#REINO UNIDO
grupo1_pais09 <- pais09 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais09 <- pais09 %>% anti_join(grupo1_pais09) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais09 <- pais09 %>% anti_join(grupo1_pais09) %>% anti_join(grupo2_pais09)  # 10% de los datos
#POLONIA
grupo1_pais10 <- pais10 %>% sample_frac(0.4)  # 70% de los datos
grupo2_pais10 <- pais10 %>% anti_join(grupo1_pais10) %>% sample_frac(4/6)  # 20% de los datos
grupo3_pais10 <- pais10 %>% anti_join(grupo1_pais10) %>% anti_join(grupo2_pais10)  # 10% de los datos

#Modelo CONJUNTO DE ENTRENAMIENTO
#Aplicamos el modelos logit para el 70% de los datos, nuestro set de entrenamiento
model_pais01<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais01,family = binomial(link = "logit"))
model_pais02<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais02,family = binomial(link = "logit"))
model_pais03<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais03,family = binomial(link = "logit"))
model_pais04<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais04,family = binomial(link = "logit"))
model_pais05<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais05,family = binomial(link = "logit"))
model_pais06<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais06,family = binomial(link = "logit"))
model_pais07<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais07,family = binomial(link = "logit"))
model_pais08<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais08,family = binomial(link = "logit"))
model_pais09<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais09,family = binomial(link = "logit"))
model_pais10<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo1_pais10,family = binomial(link = "logit"))

#CONJUNTO DE VALIDACIÓN
# Ahora con nuestro modelo entrenado lo que hacemos es hallar una f(L), factor de utilidad, que prediga las probabilidades empíricas
grupo1_pred <- predict(model_pais01, newdata = grupo2_pais01, type = "response")
grupo2_pred <- predict(model_pais02, newdata = grupo2_pais02, type = "response")
grupo3_pred <- predict(model_pais03, newdata = grupo2_pais03, type = "response")
grupo4_pred <- predict(model_pais04, newdata = grupo2_pais04, type = "response")
grupo5_pred <- predict(model_pais05, newdata = grupo2_pais05, type = "response")
grupo6_pred <- predict(model_pais06, newdata = grupo2_pais06, type = "response")
grupo7_pred <- predict(model_pais07, newdata = grupo2_pais07, type = "response")
grupo8_pred <- predict(model_pais08, newdata = grupo2_pais08, type = "response")
grupo9_pred <- predict(model_pais09, newdata = grupo2_pais09, type = "response")
grupo10_pred <- predict(model_pais10, newdata = grupo2_pais10, type = "response")

#Vamos a ver que resultado obtenemos de ese 20% en probabilidades empíricas de escoger un camino corto
model_pais01_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais01,family = binomial(link = "logit"))
model_pais02_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais02,family = binomial(link = "logit"))
model_pais03_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais03,family = binomial(link = "logit"))
model_pais04_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais04,family = binomial(link = "logit"))
model_pais05_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais05,family = binomial(link = "logit"))
model_pais06_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais06,family = binomial(link = "logit"))
model_pais07_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais07,family = binomial(link = "logit"))
model_pais08_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais08,family = binomial(link = "logit"))
model_pais09_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais09,family = binomial(link = "logit"))
model_pais10_grupo2<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo2_pais10,family = binomial(link = "logit"))

#hallar la diferencia entre lo empirico y lo predicho
q_cluster1 <- mean(model_pais01_grupo2$data$Portugues_distancia) - mean(grupo1_pred)
print(q_cluster1) 
q_cluster2 <- mean(model_pais02_grupo2$data$Portugues_distancia) -  mean(grupo2_pred)
print(q_cluster2) 
q_cluster3 <- mean(model_pais03_grupo2$data$Portugues_distancia) - mean(grupo3_pred)
print(q_cluster3) 
q_cluster4 <- mean(model_pais04_grupo2$data$Portugues_distancia) - mean(grupo4_pred)
print(q_cluster4) 
q_cluster5 <- mean(model_pais05_grupo2$data$Portugues_distancia) - mean(grupo5_pred)
print(q_cluster5) 
q_cluster6 <- mean(model_pais06_grupo2$data$Portugues_distancia) - mean(grupo6_pred)
print(q_cluster6) 
q_cluster7 <- mean(model_pais07_grupo2$data$Portugues_distancia) - mean(grupo7_pred)
print(q_cluster7) 
q_cluster8 <- mean(model_pais08_grupo2$data$Portugues_distancia) - mean(grupo8_pred)
print(q_cluster8) 
q_cluster9 <- mean(model_pais09_grupo2$data$Portugues_distancia) - mean(grupo9_pred)
print(q_cluster9) 
q_cluster10 <- mean(model_pais10_grupo2$data$Portugues_distancia) - mean(grupo10_pred)
print(q_cluster10) 

##################### REGRESION LINEAL #######################
library(readxl)
library(ggplot2)
# Cargar el archivo de Excel
data_excel <- read_excel("C:/Users/alejandro.cajide/Documents/Papeleo/USC/TFG/Inglehart_Schwartz_Hofstede.xlsx", sheet = 4)
# Almacenar los valores conocidos y los predictores
y <- data_excel$y
x1 <- data_excel$pdi
x2 <- data_excel$idv
x3 <- data_excel$mas
x4 <- data_excel$uai
x5 <- data_excel$ltowvs
x6 <- data_excel$ivr
# Ajustar el modelo de regresión lineal múltiple
model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = data_excel)
# Imprimir los coeficientes estimados
print(coef(model))
summary(model)
#valor de los pesos significativos de los coficientes
# Obtener los valores ajustados del modelo
y_pred <- predict(model)
# Crear un data frame con los valores observados y ajustados
df <- data.frame(y = y, y_pred = y_pred)
# Crear un gráfico de dispersión con línea de referencia
ggplot(df, aes(x = y, y = y_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Valores observados vs. Valores ajustados") +
  xlab("Valores observados") +
  ylab("Valores ajustadps")
#hallar R^2
lm(df)
summary(lm(df))
##################################################################################################################################
#BUCLE

grupo3_paises <- list(grupo3_pais01, grupo3_pais02, grupo3_pais03, grupo3_pais04, grupo3_pais05,
                      grupo3_pais06, grupo3_pais07, grupo3_pais08, grupo3_pais09, grupo3_pais10)

model_pais1_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais01,family = binomial(link = "logit"))
model_pais2_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais02,family = binomial(link = "logit"))
model_pais3_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais03,family = binomial(link = "logit"))
model_pais4_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais04,family = binomial(link = "logit"))
model_pais5_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais05,family = binomial(link = "logit"))
model_pais6_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais06,family = binomial(link = "logit"))
model_pais7_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais07,family = binomial(link = "logit"))
model_pais8_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais08,family = binomial(link = "logit"))
model_pais9_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais09,family = binomial(link = "logit"))
model_pais10_grupo3<-glm(Portugues_distancia~Sexo+edad_Cat+Agrupaciones+Medio_binaria,data=grupo3_pais10,family = binomial(link = "logit"))

model_paises_grupo3 <- list(model_pais1_grupo3, model_pais2_grupo3, model_pais3_grupo3, model_pais4_grupo3, model_pais5_grupo3,
                            model_pais6_grupo3, model_pais7_grupo3, model_pais8_grupo3, model_pais9_grupo3, model_pais10_grupo3)

grupo1_pred_2 <- predict(model_pais01, newdata = grupo3_pais01, type = "response") # valores entre 0 y 1 que predice, f(L)
grupo2_pred_2 <- predict(model_pais02, newdata = grupo3_pais02, type = "response")
grupo3_pred_2 <- predict(model_pais03, newdata = grupo3_pais03, type = "response")
grupo4_pred_2 <- predict(model_pais04, newdata = grupo3_pais04, type = "response")
grupo5_pred_2 <- predict(model_pais05, newdata = grupo3_pais05, type = "response")
grupo6_pred_2 <- predict(model_pais06, newdata = grupo3_pais06, type = "response")
grupo7_pred_2 <- predict(model_pais07, newdata = grupo3_pais07, type = "response")
grupo8_pred_2 <- predict(model_pais08, newdata = grupo3_pais08, type = "response")
grupo9_pred_2 <- predict(model_pais09, newdata = grupo3_pais09, type = "response")
grupo10_pred_2 <- predict(model_pais10, newdata = grupo3_pais10, type = "response")

pred_2_paises <- list(grupo1_pred_2, grupo2_pred_2, grupo3_pred_2, grupo4_pred_2, grupo5_pred_2,
                      grupo6_pred_2, grupo7_pred_2, grupo8_pred_2, grupo9_pred_2, grupo10_pred_2)

for (n in 1:10) {
  # Obtener el nombre de la variable
  grupo_nombre <- paste0("grupo", n, "_pred_2")
  model_nombre <- paste0("model_pais", n, "_grupo3")
  
  # Predicción modelo simple
  p_Lj_pred_completo <- get(grupo_nombre) - y_pred[n]
  # Predicción modelo simple
  p_Lj_pred_simple <- get(grupo_nombre)
  
  # Nueva lista para almacenar los valores
  nueva_lista_completo <- NULL
  nueva_lista_simple <- NULL
  
  # Bucle para recorrer la variable p_Lj_pred_completo
  for (valor in p_Lj_pred_completo) {
    if (valor > 0.5) {
      nueva_lista_completo <- c(nueva_lista_completo, 1)
    } else {
      nueva_lista_completo <- c(nueva_lista_completo, 0)
    }
  }
  
  for (valor in p_Lj_pred_simple) {
    if (valor > 0.5) {
      nueva_lista_simple <- c(nueva_lista_simple, 1)
    } else {
      nueva_lista_simple <- c(nueva_lista_simple, 0)
    }
  }
  
  # Contador inicializado en cero
  contador_completo <- 0
  contador_simple <- 0
  
  # Comparar las listas y actualizar el contador
  for (i in 1:length(nueva_lista_completo)) {
    if (nueva_lista_completo[[i]] == get(model_nombre)$data$Portugues_distancia[[i]]) {
      contador_completo <- contador_completo + 1
    }
  }
  
  # Comparar las listas y actualizar el contador
  for (i in 1:length(nueva_lista_simple)) {
    if (nueva_lista_simple[[i]] == get(model_nombre)$data$Portugues_distancia[[i]]) {
      contador_simple <- contador_simple + 1
    }
  }
  
  # Imprimir el contador
  print(paste0("Contador completo (", grupo_nombre, "): "))
  print(contador_completo)
  aciertos_predicciones_completo <- contador_completo / length(nueva_lista_completo)
  print(aciertos_predicciones_completo)
  
  # Imprimir el contador
  print(paste0("Contador simple (", grupo_nombre, "): "))
  print(contador_simple)
  aciertos_predicciones_simple <- contador_simple / length(nueva_lista_simple)
  print(aciertos_predicciones_simple)
}