setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/Practica1")

#frecuencias absolutas
intervalos<-c(20,30,37.5,42.5,50,60)
frecuencia <- c(10, 20, 15, 20, 20, 15)
intervalos.frecuencia <- rep(intervalos,frecuencia)
brk=c(15,25,35,40,45,55,66)

#frecuencias acumuladas
frecuencias.acumuladas <- c(10, 30, 45, 65, 85, 100)
intervalos.frecuencia.abs <- rep(intervalos,frecuencias.acumuladas)

#histograma de frecuencias absolutas
hist(intervalos.frecuencia,breaks=brk, freq=TRUE, xlab = "Intervalos",ylab = "",main = "Histograma de frecuencias absolutas", col="darkolivegreen1")
par(new=TRUE)
plot(intervalos,frecuencia, plot(frecuencia~intervalos,type="l",xlim=c(15,66), ylim=c(0,20)),cex=2,pch=20,bg="black",col="black")

#histograma de densidad
histogr<- hist(intervalos.frecuencia, breaks=brk, xlab = "Intervalos",ylab = "",main = "Histograma de densidad de frecuencias absolutas", col="darkolivegreen2")
par(new=TRUE)
plot(intervalos, histogr$density, plot(histogr$density~intervalos,xlim=c(15,66), ylim=c(0,0.04),type="l"),cex=2,pch=20,bg="black",col="black")

#histograma de frecuencias acumuladas
hist(intervalos.frecuencia.abs, breaks=brk, freq=TRUE, xlab = "Intervalos",ylab = "",main = "Histograma de frecuencias acumuladas", col="darkolivegreen3")
par(new=TRUE)
plot(intervalos,frecuencias.acumuladas, plot(frecuencias.acumuladas~intervalos,type="l",xlim=c(15,66), ylim=c(0,100)),cex=2,pch=20,bg="black",col="black")

#histograma de frecuencias acumuladas por densidades
histograma<- hist(intervalos.frecuencia.abs, breaks=brk, xlab = "Intervalos",ylab = "",main = "Histograma de densidad de frecuencias acumuladas", col="darkolivegreen4")
par(new=TRUE)
plot(intervalos, histograma$density, plot(histograma$density~intervalos,xlim=c(15,66), ylim=c(0,0.03899),type="l"),cex=2,pch=20,bg="black",col="black")



quantile(intervalos.frecuencia.abs, 0.3)
quantile(intervalos.frecuencia.abs, 0.7)