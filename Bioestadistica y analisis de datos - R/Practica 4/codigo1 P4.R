#La tabla siguiente muestra la edad (años) y la presión sanguínea (mmHg) de cada una de doce mujeres.
edad = c(56, 42, 72, 36, 63, 47, 55, 49, 38, 42, 68, 60)
PAM = c(147, 125, 160, 118, 149, 128, 150, 145, 115, 140, 152, 155)
n=12
plot(edad,PAM,main = "Diagrama de dispersión")

#apartado a
#Utilice las fórmulas apropiadas para calcular la recta de regresión de Y sobre (en función de) X, 
#el coeficiente de correlación lineal, la varianza residual, e interprete los resultados.
x=edad
y=PAM
data = data.frame(x,y)

#y depende de x (y es la variable dependiente de x, que es la variable independiente)
#en la regresión lineal simple se busca encontrar la ecuación de la recta que más se ajusta a la 
#nube de puntos: yi = α + βxi + ϵi

#β=  ∑(xi − x¯)(yi − y¯)/∑(xi − x¯)^2
#α = y¯ − βx¯

media.x = mean(x)
media.y = mean(y)
beta= sum((data$x - media.x)*(data$y - media.y))/sum((data$x - media.x)^2)
alpha = media.y - beta*media.x

#recta de regresion
yi = alpha + beta*data$x 

#el coeficiente de correlación lineal se calcula siguiendo la fórmula: 
#r =  ∑(xi − x¯)(yi − y¯)/√∑(xi − x¯)^2√∑(yi − y¯)^2

#coeficiente de correlación lineal
r= sum((data$x - media.x)*(data$y - media.y))/(sqrt(sum((data$x - media.x)^2))*sqrt(sum((data$y - media.y)^2)))

#antes de calcular la varianza, debemos calcular la suma de cuadrados del error
sres = sum((data$y - beta*data$x - alpha)^2)
#La varianza residual (σ^2) del modelo se estima como S^2res/(n−2)
var = sres/(n-2)

#hemos querido comprobar que el valor obtenido como coeficiente de correlación lienal es el correcto
cor.test(edad, PAM)


#apartado b
#Ahora calcule el coeficiente de regresión y el intercepto, y sus correspondientes intervalos de 
#confianza utilizando lm()y confint(). 
#Represente gráficamente el diagrama de dispersión y la recta de regresión estimada.

#se utiliza lm ajustar rectas de regresión.
lm(data$y~data$x)
lineal.model = lm(data$y~data$x)

#Los valores de y que se esperaría para los valores de x dados de acuerdo con la línea recta que
#mejor se ajusta; en este caso : 80.77773+1.138005*edad. 
fitted(lineal.model) #La función fitted() devuelve valores ajustados

#Los residuos que devuelve resid() es la diferencia entre el valor de y predicho por el modelo 
#(el que se obtenía con fitted()) y el observado.
resid(lineal.model)

#La función extractora confint() nos da los intervalos de confianza para los coeficientes. 
confint(lineal.model)

plot(data$x,data$y, xplot="edad (años)", yplot="presión sanguínea (mmHg)", main="Diagrama de dispersión")
lines(x,fitted(lineal.model))


#Apartado c
#Utilice la siguiente línea de código para superponer en el diagrama de dispersión anterior una 
#línea suavizada que se adapte a los puntos.
#lines(lowess(presion~edad), col="blue", lwd=3)
#¿Cual de las 2 líneas obtenidas le parece que se adapta mejor a la línea de puntos?
  
lines(lowess(PAM~edad), col="blue", lwd=3)

#Ejercicio propuesto 2
# En este caso yo el fichero está en el directorio de trabajo.
setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P4")
getwd() # Se puede cambiar con setwd()
dir() # Se debería ver el fichero que se quiere leer.
data <- read.csv("Mortality_NHANES8894_NonSmokers-1.csv")
names(data)
# El codebook con la explicación de lo que es cada variable se encuentra en
# el fichero Readme.txt que se ha facilitado.
# Ya hemos visto algunos motivos por los que hay que eliminar valores perdidos
# cuando se trabaja con modelos de regresión.
# Otro motivo es para que cuando se realizan modelos progresivos de ajuste,
# es conveniente que éstos se lleven acabo consistentemente en el subset de los mismos individuos.
dim(data) #dimensiones
data <- data[complete.cases(data),]
dim(data) #dimensiones restando las celdas vacías
attach(data)

#apartado a
#Escriba el modelo que le permite calcular el cambio promedio esperado en la función renal por un 
#incremento de una unidad en el índice de masa corporal para un valor fijo de diabetes (E[gfr.epi|bmxbmi, diab]).
#Puede utilizar elnombre de las variables que aparece reflejado en el Codebook.

E[gfr.epi|bmxbmi, diab]~bmxbmi + as.factor(diab)

#apartado b
#Ajuste el modelo de interés usando lm() y utilice la función extractora summary() para ver los 
# coeficientes. Interprete los elementos clave de la salida

lm(gfr.epi ~ bmxbmi + as.factor(diab), data = data)
rlm=lm(gfr.epi ~ bmxbmi + as.factor(diab), data = data)
summary(rlm)

par(mfrow=c(2,2))
plot(rlm)





