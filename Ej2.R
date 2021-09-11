# Cambios realizados en RStudio Cloud 
################ Cambios hechos desde RSTUDIO CLOUD 

# Ejemplo 2. RegresiÃ³n Lineal MÃºltiple

# Predecir el precio de cena (platillo). 
# Datos de encuestas de clientes de 168 restaurantes Italianos
# en el Ã¡rea deseada estÃ¡n disponibles.

# Y: Price (Precio): el precio (en USD) de la cena
# X1: Food: EvaluaciÃ³n del cliente de la comida (Max de 30)
# X2: Decor: ValuaciÃ³n del cliente de la decoraciÃ³n (Max de 30)
# X3: Service: Valuación del cliente del servicio (Max de 30)
# X4: East: variable dummy: 1 (0) si el restaurante estÃ¡ al este (oeste) de la quinta avenida

#y ~ b0 x0 + b1 x1 + b2 x2 + b3 x3 
# Primero debemos establecer nuestro directorio de trabajo y el archivo
# de datos (nyc.csv) que importaremos a R deberÃ¡ de estar en este directorio

getwd()
nyc <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-05/Ejemplo-02/nyc.csv", header = TRUE)
summary(nyc)
dim(nyc)

# Observamos algunas filas y la dimensiÃ³n del data frame

head(nyc, 2); tail(nyc, 2); dim(nyc)


attach(nyc)

# Llevamos a cabo el ajuste de un modelo
# Y = beta0 + beta1*Food + beta2*Decor + beta3*Service + beta4*East + e

m1 <- lm(Price ~ Food + Decor + Service + East)

# Obtenemos un resumen

summary(m1)

# Ajustamos nuevamente un modelo pero ahora sin considerar la variable Service
# ya que en el resultado anterior se observÃ³ que su coeficiente de regresiÃ³n
# no fue estadÃ­sticamente significativo

# Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)

m2 <- lm(Price ~ Food + Decor + East)

# Obtenemos un resumen del modelo ajustado

summary(m2)

# Una forma alternativa de obtener m2 es usar el comando update

m2 <- update(m1, ~.-Service)
summary(m2)

######
install.packages("ggplot")
library(ggplot)
# AnÃ¡lisis de covarianza

# Para investigar si el efecto de los predictores depende de la variable dummy 
# East consideraremos el siguiente modelo el cual es una extensiÃ³n a mÃ¡s de una 
# variable predictora del modelo de rectas de regresiÃ³n no relacionadas 
# Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
#           + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e (Completo)

mfull <- lm(Price ~ Food + Decor + Service + East + 
              Food:East + Decor:East + Service:East)

# Note como ninguno de los coeficientes de regresiÃ³n para los
# tÃ©rminos de interacciÃ³n son estadÃ­sticamente significativos

summary(mfull)

# Ahora compararemos el modelo completo guardado en mfull contra el modelo
# reducido guardado en m2. Es decir, llevaremos a cabo una prueba de hipÃ³tesis
# general de

# H0: beta3 = beta5 = beta6 = beta7 = 0
# es decir Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)
#
#   contra
#
# H1: H0 no es verdad
# es decir, 
# Y = beta0 + beta1*Food + beta2*Decor +  beta3*Service + beta4*East 
#           + beta5*Food*East + beta6*Decor*East + beta7*Service*East + e (Completo)

# La prueba de si el efecto de los predictores depende de la variable dummy
# East puede lograrse usando la siguiente prueba-F parcial.

anova(m2,mfull)

# Dado que el p-value es aproximadamente 0.36, fallamos en rechazar la hipÃ³tesis
# nula y adopatamos el modelo reducido
# Y = beta0 + beta1*Food + beta2*Decor + beta4*East + e (Reducido)

######

# DiagnÃ³sticos

# En regresiÃ³n mÃºltiple, las grÃ¡ficas de residuales o de residuales
# estandarizados proporcionan informaciÃ³n directa sobre la forma
# en la cual el modelo estÃ¡ mal especificado cuando se cumplen
# las siguientes dos condiciones:

# E(Y | X = x) = g(beta0 + beta1*x1 + ... + betap*xp) y
# E(Xi | Xj) aprox alpha0 + alpha1*Xj

# Cuando estas condiciones se cumplen, la grÃ¡fica de Y contra
# los valores ajustados, proporciona informaciÃ³n directa acerca de g.
# En regresiÃ³n lineal mÃºltiple g es la funciÃ³n identidad. En
# este caso la grÃ¡fica de Y contra los valores ajustados
# debe producir puntos dispersos alrededor de una recta.
# Si las condiciones no se cumplen, entonces un patrÃ³n en la
# grÃ¡fica de los residuales indica que un modelo incorrecto
# ha sido ajustado, pero el patrÃ³n mismo no proporciona 
# informaciÃ³n directa sobre como el modelo estÃ¡ mal especÃ­ficado.

# Ahora tratemos de verificar si el modelo ajustado es un modelo vÃ¡lido.

# AcontinuaciÃ³n mostramos una matriz de grÃ¡ficos de dispersiÃ³n de los
# tres predictores continuos. Los predictores parecen estar linealmente
# relacionados al menos aproximadamente

pairs(~ Food + Decor + Service, data = nyc, gap = 0.4, cex.labels = 1.5)

# AcontinuaciÃ³n veremos grÃ¡ficas de residuales estandarizados contra cada
# predictor. La naturaleza aleatoria de estas grÃ¡ficas es un indicativo de
# que el modelo ajustado es un modelo vÃ¡lido para los datos.

m1 <- lm(Price ~ Food + Decor + Service + East)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow = c(2, 2))
plot(Food, StanRes1, ylab = "Residuales Estandarizados")
plot(Decor, StanRes1, ylab = "Residuales Estandarizados")
plot(Service, StanRes1, ylab = "Residuales Estandarizados")
plot(East, StanRes1, ylab = "Residuales Estandarizados")
dev.off()

# Finalmente mostramos una grÃ¡fica de Y, el precio contra los valores
# ajustados 

plot(m1$fitted.values, Price, xlab = "Valores ajustados", ylab = "Price")
abline(lsfit(m1$fitted.values, Price))

# Inspirado en:

# [S.J. Sheather, A Modern Approach to Regression with R, DOI: 10.1007/978-0-387-09608-7_2, Â© Springer Science + Business Media LLC 2009](https://gattonweb.uky.edu/sheather/book/index.php)

# Add