#Instalamos librerias necesarias

install.packages("readxl")
install.packages("car")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("reshape2")
install.packages("gplots")
install.packages("RColorBrewer")
install.packages("MASS")
install.packages("scales")
install.packages("FactoMineR")
install.packages("gplots")
install.packages("dbscan")
install.packages("cluster")
install.packages("fpc")
install.packages("lmtest")
install.packages("sandwich")
install.packages("tseries")
install.packages("psych")

#Cargamos librerías necesarias

library(readxl)
library(car)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(gplots)
library(RColorBrewer)
library(MASS)
library(scales)
library(FactoMineR)
library(gplots)
library(dbscan)
library(cluster)
library(fpc)
library(lmtest)
library(sandwich)
library(tseries)
library(psych)

#Obtenemos el directorio actual de trabajo

getwd()

#Establecemos el directorio de trabajo si no es el correcto

setwd("C:\\Users\\PYM\\Documents\\María\\Master\\TFM")


#Leemos el dataset y lo guardamos en Datos
datos <- read_excel("FICHERO_SC.xlsx")
View(datos)

#consideramos la base de datos como un data frame
as.data.frame(datos)
head(datos)

#Vemos el número de filas y columnas que tiene nuestro dataset (36733 filas y 211 columnas)
dim(datos)

#Para poder llamar a las columnas sin tener que nombrar el dataframe del que provienen
attach(datos)

#Hacemos un summary de las variables del dataset
summary(datos)

#Veamos las variables con valores faltantes.
# Calcular el porcentaje de valores perdidos por variable
porcentaje_perdidos <- colMeans(is.na(datos)) * 100
porcentaje_perdidos

# Crear el gráfico de barras. El gráfico tiene demasiadas variables para ser manejable.
#barplot(porcentaje_perdidos, horiz = TRUE, col = "skyblue", main = "Porcentaje de Valores Perdidos por Variable",
#        xlab = "Porcentaje de Valores Perdidos", ylab = "Variables",
#        xlim = c(0, 100), las = 1)


#Observamos variables que tienen todos los valores faltantes o todos menos uno. 
#Eliminamos dichas variables.

# Identificar las columnas que tienen más del 90% de valores NA
columnas_na <- sapply(datos, function(x) sum(is.na(x))/length(x)) > 0.9

# Eliminar las columnas que tienen más del 90% de valores NA
datos <- datos[, !columnas_na]

dim(datos)
#Nos quedamos, de momento, con 44 variables.

#La variable Territorio contiene información que es redundante, que ya consta en otras variables, a excepción 
# de la denominación del municipio.
#municipio_d=str_sub(Territorio,12,str_locate(Territorio,"sección")[1]-2)
#municipio_d

datos$municipio_d=str_sub(datos$Territorio,12,regexpr("sección", datos$Territorio)-2)

dim(datos)

#Ahora ya podemos eliminar la variable Territorio
datos<- subset(datos, select = -Territorio)
dim(datos)

#También está duplicada la variable CMUN (MUNICIPIO_COD contiene esa informacion)
datos<- subset(datos, select = -CMUN)
dim(datos)
# Tenemos 43 variables

summary(datos)

#ESTADISTICA DESCRIPTIVA UNIVARIANTE
names(datos)
#comprobamos la información de estas dos variables.
summary (datos$Poblacion)
summary (datos$PobTotal)
#Observamos que PobTotal tiene menos datos faltantes. Eliminamos la variable Población.
#Ahora ya podemos eliminar la variable Territorio
datos<- subset(datos, select = -Poblacion)
dim(datos)
# Tenemos 42 variables

#Si aplicamos summary al dataframe, nos porporciona el mínimo, primer, segundo y tercer cuartil y el máximo.
summary(datos)
#Comprobamos algunas variables con gran porcentaje de valores missing, las eliminamos también
# Identificar las columnas que tienen más del 80% de valores NA
columnas_na <- sapply(datos, function(x) sum(is.na(x))/length(x)) > 0.8

# Eliminar las columnas que tienen más del 80% de valores NA
datos <- datos[, !columnas_na]

dim(datos)
#Nos quedamos con 39 variables

summary(datos)
#Observamos 378 valores faltantes en las variables de elecciones (Votosacandidaturas, Blancos, Nulos, CensoINE). Comprobamos con los datos de origen 
#que son secciones que no existían en el momento de las elecciones. Eliminaremos dichos registros.
#Guardamos en datos las filas que no tienen datos faltantes en la característica Blancos, Nulos y VotosaCandidaturas

datos <- datos[!is.na(datos$Votosacandidaturas),]
datos <- datos[!is.na(datos$Blancos),]
datos <- datos[!is.na(datos$Nulos),]
datos <- datos[!is.na(datos$CensoINE),]
dim(datos)
#Tenemos 36355 observaciones y 39 variables.


#Detectamos secciones con valores en PobTotal muy extremos.
#Comprobamos que aquellas con valor muy grande corresponden con secciones censales
#con alto porcentaje de extranjeros. Son atípicos pero no las eliminamos, pues aportan información.
#También existen secciones censales con población extremadamente pequeña y que tienen la mayoría de las 
#variables no informadas. Estas instancias sí que se pueden eliminar, pues no aportan información.


#Observamos que para las secciones censales con PobTotal menor que 100, no están informadas la mayor parte de variables
#Las eliminamos pues no aportan conocimiento.
datos <- subset(datos, datos$PobTotal >= 100)
dim(datos)

#Tenemos 34908 registros y 39 variables.

#Comprobamos el resto de variables numéricas con valores missing que no se han analizado hasta ahora.
#El porcentaje de NaN en las variables es menor del 6%. Imputaremos el valor mediano en estos valores faltantes.
#Si el valor es faltante, le imputo la mediana. Para calcular la mediana, no utilizo los faltantes (na.rm=TRUE)
#Si no es faltante, lo dejo como está.

datos$RentaNetaMediaPorPersona <- ifelse(is.na(datos$RentaNetaMediaPorPersona), median(datos$RentaNetaMediaPorPersona, na.rm = TRUE), datos$RentaNetaMediaPorPersona)
datos$RentaNetaMediaPorHogar <- ifelse(is.na(datos$RentaNetaMediaPorHogar), median(datos$RentaNetaMediaPorHogar, na.rm = TRUE), datos$RentaNetaMediaPorHogar)
datos$MediadelarentaPorUC <- ifelse(is.na(datos$MediadelarentaPorUC), median(datos$MediadelarentaPorUC, na.rm = TRUE), datos$MediadelarentaPorUC)
datos$MedianadelarentaPorUC <- ifelse(is.na(datos$MedianadelarentaPorUC), median(datos$MedianadelarentaPorUC, na.rm = TRUE), datos$MedianadelarentaPorUC)
datos$RentabrutaMediaPorPersona <- ifelse(is.na(datos$RentabrutaMediaPorPersona), median(datos$RentabrutaMediaPorPersona, na.rm = TRUE), datos$RentabrutaMediaPorPersona)
datos$RentabrutaMediaPorHogar <- ifelse(is.na(datos$RentabrutaMediaPorHogar), median(datos$RentabrutaMediaPorHogar, na.rm = TRUE), datos$RentabrutaMediaPorHogar)
datos$FuentedeIngresoSalario <- ifelse(is.na(datos$FuentedeIngresoSalario), median(datos$FuentedeIngresoSalario, na.rm = TRUE), datos$FuentedeIngresoSalario)
datos$FuentedeingresoOtrosIngresos <- ifelse(is.na(datos$FuentedeingresoOtrosIngresos), median(datos$FuentedeingresoOtrosIngresos, na.rm = TRUE), datos$FuentedeingresoOtrosIngresos)

#comprobamos que no hay datos faltantes
summary(datos$RentaNetaMediaPorPersona)
summary(datos$RentaNetaMediaPorHogar)
summary(datos$MediadelarentaPorUC )
summary(datos$MedianadelarentaPorUC)
summary(datos$RentabrutaMediaPorPersona)
summary(datos$RentabrutaMediaPorHogar)
summary(datos$FuentedeIngresoSalario)
summary(datos$FuentedeingresoOtrosIngresos)

dim(datos)
summary (datos)

#Para tener una visión de conjunto del data set.
dim(datos)
#solo tienen sentido las variables numericas
summary (datos)
describe(datos)


#Gráficas para detectar outliers

#Gráfica de violín
max_pob_total <- max(datos$PobTotal)
ggplot(datos, aes(x = "", y = PobTotal)) + 
  geom_violin(fill = "lightblue") +
  labs(y = "Población total") +
  scale_y_continuous(labels = function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")) +
  coord_cartesian(ylim = c(0, max_pob_total))

boxplot(datos$PorcentajeMujeres, main = "PorcentajeMujeres")
boxplot(EdadMedia, main = "EdadMedia")
boxplot(PorcentajeMenores16, main = "PorcentajeMenores16")
boxplot(Porcentaje16a64, main = "Porcentaje16a64")
boxplot(PorcentajeMayores64, main = "PorcentajeMayores64")
boxplot(PorcentajeNacionalidadExtranjera, main = "PorcentajeNacionalidadExtranjera")
boxplot(PorcentajeNacidosExtranjero, main = "PorcentajeNacidosExtranjero")
boxplot(PorcentajeCursandoEstudiosSuperiores, main = "PorcentajeCursandoEstudiosSuperiores")
boxplot(PorcentajeCursandoUniversidad , main = "PorcentajeCursandoUniversidad")
boxplot(PorcentajePersEstudiosSuperiores , main = "PorcentajePersEstudiosSuperiores")
boxplot(ParadosSobreActivos , main = "ParadosSobreActivos")
boxplot(PorcentajeOcupados , main = "PorcentajeOcupados")
boxplot(PorcentajeActivos , main = "PorcentajeActivos")
boxplot(PorcentajePensionInvalidez , main = "PorcentajePensionInvalidez")
boxplot(PorcentajePensionistaJubilacion , main = "PorcentajePensionistaJubilacion")
boxplot(PorcentajeOtrosInactivos , main = "PorcentajeOtrosInactivos")
boxplot(PorcentajeEstudiantes , main = "PorcentajeEstudiantes")

# Histograma de la variable RentaNetaMediaporPersona
hist(datos$RentaNetaMediaPorPersona, breaks = 20, col = "lightblue", 
     xlab = "Renta", ylab = "Frecuencia", main = "Renta neta media por persona")

boxplot(RentaNetaMediaPorPersona , col = "lightblue",
        main = "RentaNetaMediaPorPersona")
#Observamos valores anormalmente altos de los valores de renta. Comprobamos que son válidos pues corresponden a secciones censales on elevados
#niveles de renta.

boxplot(RentaNetaMediaPorHogar ,  col = "lightblue" , main = "Renta Neta Media Por Hogar")
boxplot(MediadelarentaPorUC ,  col = "lightblue" , main = "Media de la renta por Unidad de Consumo")

#Observamos en los boxplot de las variables, que hay valores atípicos. Puede ser debido a la propia definición de sección censal, 
#muy diferente en una aldea de 100 habitantes que en una capital muy poblada.

#VARIABLES CALCULADAS
#En primer lugar vamos a calcular una columna abstencion, que es lo que nos interesa estudiar.

datos$abstencion=datos$CensoINE-datos$Blancos-datos$Votosacandidaturas-datos$Nulos
summary(datos$abstencion)

datos$Porcentajeabstencion=100*(datos$abstencion/datos$CensoINE)
summary(datos$Porcentajeabstencion)

#Observamos que hay 3 secciones censales con 0 votos en blanco, 0 votos a candidaturas y 0 nulos. Nos da 100% abstencion ¡OJO!!!!
#Eliminamos las filas pues creemos que se trata de un error.
dim(datos)
#Tenemos 34908 observaciones y 41 variables

# Eliminar filas que cumplen tres condiciones en un dataframe
#Nos quedamos con el subconjunto que no cumpla simultáneamente que las 3 variables son 0.
datos <- subset(datos, datos$Blancos != 0 | datos$Nulos != 0 | datos$Votosacandidaturas != 0)
dim(datos)
#Tenemos 34905 observaciones y 41 variables


#GRÄFICAS
hist(datos$abstencion, xlab="Abstención", ylab="Frecuencia", col="light blue", border="blue", main= "Abstención")

boxplot(datos$abstencion, main = "Abstención")

#MEDIDAS DE DISPERSIÓN
#la cuasivarianza
var(datos$abstencion)

#la cuasidesviacion tipica
sd(datos$abstencion)

#Crearemos un indicador de capital de provincia, por si puede ser de interes para caracterizar la abstencion.
datos$capital <- ifelse(datos$MUNICIPIO_COD %in% c("01059", "02003", "03014", "04013", "05019", "06015", "07040", "08019", "09059", "10037", "11012", "12040", "13034", "14021", "15030", "16078", "17079", "18087", "19130", "20069", "21041", "22125", "23050", "24089", "25120", "26089", "27028", "28079", "29067", "30030", "31201", "32054", "33044", "34120", "35016", "36038", "37274", "38038", "39075", "40194", "41091", "42173", "43148", "44216", "45168", "46250", "47186", "48020", "49275", "50297", "51001", "52001"), 1, 0)
dim(datos)

names(datos)
#Eliminamos las variables Blancos, Nulos, Votosacandidaturas y CensoINE pues forman parte del cálculo de la abstencion
#Eliminamos abstencion, nos interesa porcentaje abstencion
#Si quiero quitar las columnas de la 2 a la 5 y la 40
datos <- datos [, -c(2:5,40)]
names(datos)
dim(datos)

#DESCRIPTIVA variables categóricas
#Comunidades autónomas
#Obtenemos tablas de frecuencias absolutas con el comando table
table(datos$ccaa)
#Podemos obtener las frecuencias relativas
table(datos$ccaa) / margin.table(table(datos$ccaa))
#GRAFICAS
#diagrama de barras
barplot(table(datos$ccaa), col="blue", main="Comunidades autónomas", ylim=c(0,7000))

#Provincias
table(datos$CPRO)
table(datos$CPRO) / margin.table(table(datos$CPRO))
barplot(table(datos$CPRO), col="steelblue", main="Provincias", ylim=c(0,5000))

#Indicador capital de provincia
table(datos$capital)
table(datos$capital) / margin.table(table(datos$capital))


#REDUCCIÓN DE LA DIMENSIONALIDAD. COMPONENTES PRINCIPALES.
#Eliminamos las variables categóricas.

datos_num <- datos %>% select_if(is.numeric)
names(datos_num)
describe(datos_num)

#Eliminamos también el porcentaje de abstención
datos_num_p <- datos_num [, -c(28)]
dim(datos_num_p)

#Para tener una visión conjunta de las relaciones entre las variables, calculamos la matriz de correlaciones.
correlaciones <- cor(datos_num_p)
correlaciones

#Como hay tantas variables, no es fácil manejar la matriz de correlaciones. 

#Prueba
heatmap(correlaciones, col = hcl.colors(50))
#sin dendrograma
heatmap(correlaciones, col = hcl.colors(50), Rowv = NA, Colv = NA, main="Mapa de calor de correlaciones")
?heatmap()


#Es importante escalar los datos para que no afecte la unidad de medida
datos_num_p_esc <- scale(datos_num_p)
#Calculamos las correlaciones
correlaciones_esc <- cor(datos_num_p_esc)
correlaciones_esc

#Para visualizar las correlaciones, graficamos un mapa de calor
heatmap(correlaciones_esc)

#No hacemos gráfico de dispersión porque son demasiadas variables.

#Obtenemos las componentes principales
#Método 1.
pca <- PCA(datos_num_p_esc, graph = FALSE)
pca <- PCA(datos_num_p_esc, graph = TRUE)
#El gráfico es difícil de interpretar, aunque ayuda a conocer los datos
pca <- PCA(datos_num_p_esc)

#Método 2
acp1 <- prcomp(datos_num_p_esc)
summary(acp1)
acp1
#En el siguiente gráfico se observa cómo disminuye la proporción de varianza que explican 
#las componentes 2,3, etc. respecto de la primera
#Según esta representación (método del codo, nos quedaríamos con las 3 primeras componentes principales)
plot(acp1 , type="lines", main="Método del codo (acp1)")

#Observando los valores de la varianza, para obtener un 80% explicada, tomaríamos 5 componentes
#con 5 cp 82,40 % de la varianza
acp1$rotation

#Otro método para las componentes principales
acp2 <- princomp(datos_num_p_esc)
summary(acp2)
acp2$loadings
plot(acp2, type = "lines", main="Método del codo (acp2)")
#Los pesos de cada variable en la primera componente principal
round(acp2$loadings[, 1], 3)
#Representación gráfica de los pesos de las componentes principales
plot(acp2$loadings[, 1:2], type = "n", xlim = c(-1, 1))
for (i in 1:nrow(acp2$loadings)) {
  arrows(0, 0, acp2$loadings[i, 1], acp2$loadings[i, 2])
}
text(acp2$loadings[, 1:2], dimnames(acp2$loadings)[[1]])
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
#Resulta muy complicado de interpretar. Era mejor el método anterior.

#Veamos el método DBSCAN para la selección de características
#con eps la distancia máxima entre 2 ptos para que se consideren del mismo grupo
#MinPts el número mínimo de puntos para formar un grupo
dim(datos_num_p_esc)
#Variamos los valores de los parámetros eps y MinPts. Empezamos por MinPts superior al numero
#de características, y vamos aumentando. A su vez, variamos eps. 
db <- dbscan(datos_num_p_esc , eps = 0.5, MinPts = 30)
#visualizamos los resultados
plot(datos_num_p_esc, col = db$cluster, main="Representación DBSCAN")
# el paquete fpc es el que contiene la función dbscan

#Probamos diferentes valores de los parámetros
eps_values <- seq(0.5, 2, by = 0.5) # valores de eps que se van a probar
MinPts_values <- seq(30, 400, by = 20) # valores de MinPts que se van a probar

resultados <- matrix(ncol = length(MinPts_values), nrow = length(eps_values)) # matriz para guardar los resultados

for (i in seq_along(eps_values)) {
  for (j in seq_along(MinPts_values)) {
    db <- dbscan(datos_num_esc, eps = eps_values[i], MinPts = MinPts_values[j]) # ejecuta el algoritmo dbscan con los valores de eps y minPts correspondientes
    resultados[i,j] <- length(unique(db$cluster)) # guarda el número de clusters encontrados en la matriz de resultados
  }
}

resultados # muestra la matriz de resultados
#representación gráfica de resultados
image(minPts_values, eps_values, resultados, col = rev(heat.colors(10)))

#definimos una distancia
dist_mat <- dist(datos_num_p_esc)
sil <- silhouette(db$cluster, dist_mat)
plot(sil)
#No ha servido para identificar patrones

#Otro intento de Análisis de Componentes Principales (eliminando variables)
#Observando los pesos de las variables, porcentaje de mujeres y porcentaje de hombres, aportan 
#información complementaria
#En cuanto a la edad media, es un indicador compendio de población en diferentes grupos de edad.

#Tomaremos dataframe eliminando porcentaje de hombres y las variables de población en diferentes grupos de edad
#y repetiremos el ACP.

names(datos_num_p)
datos_num_p_bis <- datos_num_p [, -c(3,5:7)]

names(datos_num_p_bis)
#Repetimos el proceso para tener componentes principales de este nuevo dataframe
datos_num_p_bis_esc <- scale(datos_num_p_bis)
acp3 <- princomp(datos_num_p_bis_esc)
summary(acp3)
#Necesito 4 CP para 80% de la varianza (79,48%)
acp3$loadings
plot(acp3, type = "lines") #metodo codo (3 CP)
#Los pesos de cada variable en la primera componente principal
round(acp3$loadings[, 1], 3)
#Representación gráfica de los pesos de las componentes principales
plot(acp3$loadings[, 1:2], type = "n", xlim = c(-1, 1))
for (i in 1:nrow(acp3$loadings)) {
  arrows(0, 0, acp3$loadings[i, 1], acp3$loadings[i, 2])
}
text(acp3$loadings[, 1:2], dimnames(acp3$loadings)[[1]])
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
#Igualmente difícil de interpretar


#Repetimos el proceso eliminando más variables que pueden contener información parecida
#Por otra parte, la información de PorcentajeMayores64 y PorcentajePensionistasJubilación es muy similar
#Ocurre los mismo por nacidos en el extanjero o de nacionalidad extranjera 
#Igualmente con la renta neta y renta bruta
#Eliminamos PorcentajeEstudiandouniversidad pues es similar a cursando estudios superiores
names(datos_num_p_bis)
datos_num_3 <- datos_num_p_bis [, -c(5,7,13,20,21)]
names(datos_num_3)
#escalamos
datos_num_3_esc <- scale(datos_num_3)
acp4 <- princomp(datos_num_3_esc)
summary(acp4)
#Necesito 5 CP para 80% de la varianza (83.25%)
acp4$loadings
plot(acp4, type = "lines", main="Método del codo para acp4") #metodo codo (3 CP)
#Los pesos de cada variable en la primera componente principal
round(acp4$loadings[, 1], 3)
#Representación gráfica de los pesos de las componentes principales
plot(acp4$loadings[, 1:2], type = "n", xlim = c(-1, 1))
for (i in 1:nrow(acp4$loadings)) {
  arrows(0, 0, acp4$loadings[i, 1], acp4$loadings[i, 2])
}
text(acp4$loadings[, 1:2], dimnames(acp4$loadings)[[1]])
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
#De nuevo es difícil de interpretar



#REGRESIÓN SIMPLE

#Definimos una matriz con las variables explicativas (eliminamos la variable porcentaje de abstencion, que será la variable dependiente en el modelo)
names(datos_num)
datos_sin <- datos_num[,-c(28)] #variables explicativas, sin porcentajeabstencion
#La convertimos en matriz
matriz <- as.matrix (datos_sin)

#Realizamos la regresión simple para cada variable
resultados <- list()
for (i in 1:28) {
   modelo <- lm(datos$Porcentajeabstencion ~ matriz[,i])
   resultados[[i]] <- summary(modelo)
}
resultados

#Para visualizar más fácilmente la significatividad de las variables, obtenemos un vector de p-valores
#Calculemos el p-valor de todas las posibles regresiones simples de porcentajeabstencion respecto a las variables explicativas contenidas en matriz
#Inicializamos un vector de p-valores
pvalores <-rep(NA,28)
for (i in 1:28) {
  pvalores[i] <-summary(lm(datos$Porcentajeabstencion ~ matriz[,i]))$coefficients[,4][2]
}
pvalores


#Observamos que todas las variables en la regresion simple tienen p-valor menor que 0,05.
#Indica que en la regresión simple, una a una, sin la presencia del resto de variables,
#estas variables son significativas para explicar el porcentaje de abstención




#REGRESIÓN MULTIPLE

#Implementaremos el método Stepwise para saber si todas las variables son necesarias en la regresión

library(MASS)
fullModel = lm(datos$Porcentajeabstencion ~ ., data = datos_sin) # modelo con todas las variables
summary(fullModel)
#Observamos que no nos da resultados para 2 variables que se obtienen directamente del conocimiento de
#otras presentes en el modelo.
#Solo 4 variables no resultan significativas en el modelo global 
#PorcentajePensionistaJubilacion         
#PorcentajeOtrosInactivos  
#RentaNetaMediaPorPersona
#RentaNetaMediaPorHogar
nullModel = lm(datos$Porcentajeabstencion ~ 1, data = datos_sin) # modelo con solo interceptacion
summary(stepAIC(nullModel, # empezamos con un modelo sin variables
                direction = 'forward', # hacemos la selección hacia adelante
                scope = list(upper = fullModel, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel), # el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion

#Salen todas las variables significativas.

#Haremos el modelo Stepwise backward, para comprobar si sale diferente.

summary(stepAIC(fullModel, # empezamos con un modelo con todas las variables
                direction = 'backward', # hacemos la selección hacia atrás
                scope = list(upper = fullModel, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel), # el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion
#El R2 ajustado es de 0.5994, el pvalor del test, significativo

#Si lo hacemos de otra manera
modback <- stepAIC(fullModel, trace=TRUE, direction="backward")
AIC(modback)
modback$anova
#Hacia atrás, elimina 4 variables: PorcentajeMayores64 PorcentajeHombres
#PorcentajeOtrosInactivos RentaNetaMediaPorHogar

modfor <- stepAIC(nullModel, trace=FALSE, direction="forward", scope=fullModel)
modfor$anova
AIC(modfor)
#Hacia adelante no añade ninguna variable

#El modelo doble o mixto
opcion <- list(lower = nullModel, upper = fullModel)
modboth <- stepAIC(nullModel, trace=FALSE, direction="both", scope=opcion)
modboth$anova
AIC(modboth)
#Vamos a calcular el AIC para decidir con cuál de los dos modelos (Back o forward)
#nos quedamos.

#Si hacemos Regresión mútliple con todas las variables, no todas ellas salen significativas
#en presencia de las otras
lm.fit <-lm(datos$Porcentajeabstencion~ ., data = datos_sin)
summary(lm.fit)


#SELECCION DE CARACTERISTICAS FINAL
#Del conjunto de datos, tras el análisis de correlaciones, vamos 
#a seleccionar características
names(datos_num)

#Tomamos todas las filas y quitamos columnas, eliminando también el porcentaje de abstención
datos_num_f <- datos_num [, -c(3,5:7,9,11:12,15,17,19:22,24:26,28)]
names(datos_num_f)

#Para tener una visión conjunta de las relaciones entre las variables, calculamos la matriz de correlaciones.
correlaciones_f <- cor(datos_num_f)
correlaciones_f

#Prueba
heatmap(correlaciones_f)

#Es importante escalar los datos para que no afecte la unidad de medida
datos_num_f_esc <- scale(datos_num_f)
#Calculamos las correlaciones
correlaciones_f_esc <- cor(datos_num_f_esc)
correlaciones_f_esc

#Para visualizar las correlaciones, graficamos un mapa de calor
heatmap(correlaciones_f_esc, col = hcl.colors(50), Rowv = NA, Colv = NA, main="Mapa de calor de correlaciones")

#Veamos si ahora se pueden hacer gráficos que permitan intuitivamente analizar los datos
pairs(datos_num_f)

#Análisis de componentes principales
acp1_f <- prcomp(datos_num_f_esc)
summary(acp1_f)
acp1_f

#En el siguiente gráfico se observa cómo disminuye la proporción de varianza que explican 
#las componentes 2,3, etc. respecto de la primera
#Según esta representación (método del codo, nos quedaríamos con las 5 primeras componentes principales)

#Observando los valores de la varianza, para obtener un 80% explicada, tomaríamos 5 componentes (0,79544)
#con 6 cp 84,42 % de la varianza


#REGRESIÓN SIMPLE
#Puesto que se trata de un subconjunto de variables del original y todas ellas eran significativas. No hace falta repetir.

#Definimos una matriz con las variables explicativas (eliminamos la variable porcentaje de abstencion, que será la variable dependiente en el modelo)
names(datos_num_f)
datos_sin_f <- datos_num_f[,-c(12)] #variables explicativas, sin porcentajeabstencion
#La convertimos en matriz
matriz_f <- as.matrix (datos_sin_f)

#Realizamos la regresión simple para cada variable
resultados_f <- list()
for (i in 1:12) {
  modelo_f<- lm(datos$Porcentajeabstencion ~ matriz_f[,i])
  resultados_f[[i]] <- summary(modelo_f)
}
resultados_f

#REGRESIÓN MULTIPLE

#Implementaremos el método Stepwise para saber si todas las variables son necesarias en la regresión

library(MASS)
fullModel_f = lm(datos$Porcentajeabstencion ~ ., data = datos_sin_f) # modelo con todas las variables
summary(fullModel_f)
#Solo 1 variable no resulta significativas en el modelo global 
#FuentedeingresosOtrosIngresos
nullModel_f = lm(datos$Porcentajeabstencion ~ 1, data = datos_sin_f) # modelo con solo interceptacion
summary(stepAIC(nullModel_f, # empezamos con un modelo sin variables
                direction = 'forward', # hacemos la selección hacia adelante
                scope = list(upper = fullModel_f, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel_f), # el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion

#Salen todas las variables significativas. Con r cuadrado de 0,5541

#Haremos el modelo Stepwise backward, para comprobar si sale diferente.

summary(stepAIC(fullModel_f, # empezamos con un modelo con todas las variables
                direction = 'backward', # hacemos la selección hacia atrás
                scope = list(upper = fullModel_f, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel_f), # the minimum to consider is a model with no variables  el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion
#El R2 ajustado es de 0.5541, el pvalor del test, significativo

#Si lo hacemos de otra manera
modback_f <- stepAIC(fullModel_f, trace=TRUE, direction="backward")
AIC(modback_f)
modback_f$anova
#REVISAR Hacia atrás, elimina 1 variable: FuentedeingresosOtrosIngresos

modfor_f <- stepAIC(nullModel_f, trace=FALSE, direction="forward", scope=fullModel_f)
modfor_f$anova
AIC(modfor_f)
#Hacia adelante no añade ninguna variable

#El modelo doble o mixto
opcion_f <- list(lower = nullModel_f, upper = fullModel_f)
modboth_f <- stepAIC(nullModel_f, trace=FALSE, direction="both", scope=opcion_f)
modboth_f$anova
AIC(modboth_f)
#Vamos a calcular el AIC para decidir con cuál de los dos modelos (Back o forward)
#nos quedamos. El que dé menor valor.


#Comprobación de las hipotesis del modelo de regresion múltiple

# Evaluar la normalidad de los residuos

# Gráfico de histograma
hist(resid(modback_f), breaks = 20, main = "Histograma de los residuos", col = "lightblue")

# Gráfico Q-Q
qqnorm(resid(modback_f))
qqline(resid(modback_f))

# Prueba de normalidad de Shapiro-Wilk. No proporciona resultado pues el tamaño es muy grande.
shapiro.test(resid(modback_f))

#Lilliefors sirve par muestras pequeñas. Aquí no tiene utilidad.

# Prueba de normalidad de Kolmogorov-Smirnov
ks.test(resid(modback_f), "pnorm")

#Jarque Bera normalidad
jarque.bera.test(resid(modback_f))

# Evaluar la linealidad de la relación entre las variables explicativas y la variable de respuesta
# Gráficos de dispersión
#plot(datos$var1, datos$abstencion, main = "Abstención vs Var1")
#plot(datos$var2, datos$abstencion, main = "Abstención vs Var2")

#Grafico para evaluar linealidad. Gráfico de componentes más residuos
crPlots(modback_f)


# Prueba de Ramsey RESET
library(lmtest)
#resettest(modback_f, power = 2)
resettest(modback_f)

# Evaluar la homocedasticidad de los residuos

#Obtenemos los residuos estandarizados
residuos_estandarizados <- rstandard(modback_f)
#los valores ajustados
valores_ajustados <- fitted.values(modback_f)
#Creamos un gráfico de dispersión
plot(valores_ajustados, residuos_estandarizados, 
     xlab = "Valores Ajustados", ylab = "Residuos Estandarizados",
     main = "Gráfico de dispersión de Residuos vs Valores Ajustados")


# Prueba de Breusch-Pagan
library(lmtest)
bptest(modback_f)

# Prueba de White
library(sandwich)
vcovHC(modback_f)
coeftest(modback_f, vcov = vcovHC(modback_f))

#Goldfeld-Quandt
gqtest(modback_f)

#multicolinealidad
library(car)
vif(modback_f)
vif_valores <- vif(modback_f)
barplot(vif_valores , main = "Valores VIF", horiz = TRUE , col="steelblue")
abline(v=10, lwd = 3, lty = 2)

vif_valores <- vif(modback_f)
nombres_variables <- names(vif_valores)
barplot(vif_valores, main = "Valores VIF", horiz = TRUE, col = "steelblue",
        names.arg = nombres_variables)
abline(v = 10, lwd = 3, lty = 2)

#Observamos que da un valor mayor que 10 para PorcentajeOcupados
#Implica multicolinealidad

#Eliminamos esta variable del Modelo y comparamos AIC de ambos modelos, para elegir con cuál nos quedamos.
#Definimos una matriz con las variables explicativas eliminando PorcentajeOcupados
names(datos_sin_f)
datos_sin_fp <- datos_sin_f[,-c(7)] #variables explicativas, sin PorcentajeOcupados
names(datos_sin_fp)

#La convertimos en matriz
matriz_fp <- as.matrix (datos_sin_fp)

#REGRESIÓN MULTIPLE

#Implementaremos el método Stepwise para saber si todas las variables son necesarias en la regresión

library(MASS)
fullModel_fp = lm(datos$Porcentajeabstencion ~ ., data = datos_sin_fp) # modelo con todas las variables
summary(fullModel_fp)
#Solo 1 variable no resulta significativas en el modelo global 
#FuentedeingresosOtrosIngresos
nullModel_fp = lm(datos$Porcentajeabstencion ~ 1, data = datos_sin_fp) # modelo con solo interceptacion
summary(stepAIC(nullModel_fp, # empezamos con un modelo sin variables
                direction = 'forward', # hacemos la selección hacia adelante
                scope = list(upper = fullModel_fp, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel_fp), # el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion

#A diferencia del caso anterior, no salen todas las variables significativas. 
#FuentedeingresosOtrosIngresos no es significativa, y baja R2 ajustado de 0,5541 a 0,5531. pvalor del test significativo

#Haremos el modelo Stepwise backward, para comprobar si sale diferente.

summary(stepAIC(fullModel_fp, # empezamos con un modelo con todas las variables
                direction = 'backward', # hacemos la selección hacia atrás
                scope = list(upper = fullModel_fp, # el máximo a considerar es un modelo con todas las variables
                             lower = nullModel_fp), # the minimum to consider is a model with no variables  el mínimo modelo no tiene variables
                trace = FALSE)) # no muestra el paso a paso de la seleccion
#El R2 ajustado también ha bajado de 0.5541 a 0,5531, el pvalor del test, significativo

#Si lo hacemos de otra manera
modback_fp <- stepAIC(fullModel_fp, trace=TRUE, direction="backward")
AIC(modback_fp)
modback_fp$anova
#Hacia atrás, Ahora no elimina ninguna variable (antes eliminaba FuentedeingresosOtrosIngresos)

modfor_fp <- stepAIC(nullModel_fp, trace=FALSE, direction="forward", scope=fullModel_fp)
modfor_fp$anova
AIC(modfor_fp)
#Hacia adelante no añade ninguna variable (igual que antes)

#El modelo doble o mixto
opcion_fp <- list(lower = nullModel_fp, upper = fullModel_fp)
modboth_fp <- stepAIC(nullModel_fp, trace=FALSE, direction="both", scope=opcion_fp)
modboth_fp$anova
AIC(modboth_fp)
#Se queda con las 11 variables incluidas sin eliminar ninguna.

#Vamos a comparar el AIC para decidir con cuál de los dos modelos (Back o forward)
#nos quedamos el que dé menor valor. Da el mismo valor el backward y el both. Nos quedamos con backward.(215086,1)
#Comparamos ahora con el AIC del modelo que incluía PorcentajeOcupados. (215005,5)

