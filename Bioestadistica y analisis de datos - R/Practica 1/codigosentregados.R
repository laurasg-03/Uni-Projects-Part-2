setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/Practica1")

#código 1

#Primero generaremos una distribución normal de 50 elementos.
# rnorm() crea 50 valores que siguen una distribución normal (0, 1)
# <- los almacena en un vector que se llama x.
x<-rnorm(50)

#media almacena la media de x (vector de los 50 valores que siguen una 
#distribución normal (0,1))
media=mean(x)

#mediana almacena la mediana de x
mediana=median(x)

#Se pueden obtener los cuantiles empíricos utilizando la función quantile()
quantile(x)

#en pvec se guarda la secuencia del 0 al 1, con saltos de 0.1
pvec<-seq(0,1,0.1)

#calcular el los cuantiles en pocentajes de 10, de x
quantile(x,pvec)

################################################################################
#código 2

#se descarga la librería 
install.packages("ISwR")
library(ISwR)

#mostrar los datos contenidos en la base de datos de juul
data(juul)

#help de juul
?juul

#se le da la instrucción de que muestre unn gráfico de puntos que relaciona
#igf1 con edad, a partir de la tabla de datos de juul
plot(igf1~age, data=juul)

attach(juul)
#va a dar error puesto que hay valores perdidos (NA)
mean(igf1)

#debemos indicar que no tenga en cuenta valores perdidos
mean(igf1,na.rm=T)

#calcular la desviacion estandar
sd(igf1,na.rm=T)

#determina el numero de valores perdidos (numero de NA)
sum(!is.na(igf1))

#resumen del dataset juul
summary(juul)

################################################################################
#código 3

detach(juul) #usado para "desenganchar" un paquete que estaba enganchado a una librería
# facte¡or estructura de datos para manejar variables categóricas
juul$sex<-factor(juul$sex,labels=c("M","F")) #en la columna sex se van a renombrar las filas como M y F
juul$menarche<-factor(juul$menarche,labels=c("No","Yes")) #en la columna menarche se van a renombrar las filas como No y Yes
juul$tanner<-factor(juul$tanner,labels=c("I","II","III","IV","V"))
attach(juul) #se va a poder acceder a las columnas de juul simplemente dando su nombre
summary(juul)

#También podríamos haber utilizado la función transform(), que permite escribirlo todo en la misma líena de código
juul<-transform(juul,
                sex=factor(sex,labels=c("M","F")),
                menarche=factor(menarche,labels=c("No","Yes")),
                tanner=factor(tanner,labels=c("I","II","III","IV","V")) )
summary(juul)

################################################################################
#código 4

hist(x)#Histogramas. Por defecto R, intenta hacer puntos de corte "adecuados"

#Ejemplo accidentes versus edad (0-4,5-9,10-15,16,17,18-19,20-24,25-59,60-79)
#se guardan valores de la edad media, el número de accidentes, y la repetición de cada edad por accidente, en vectores diferentes
mid.age<-c(2.5,7.5,13,16.5,17.5,19,22.5,44.5,70.5)
acc.count<-c(28,46,58,20,31,64,149,316,103)
age.acc<-rep(mid.age,acc.count)
brk<-c(0,5,10,16,17,18,20,25,60,80) #va a ser las divisiones del eje x al crear el histograma

#Nótese que automátiamente se obtiene de esta manera el histograma correcto donde el área
#de una columna es proporcional a la frecuencia relativa de manera que el área total del histograma es 1.
hist(age.acc,breaks=brk) #pueden apreciarse las diviones (breaks)

#Distribución empírica acumulada
n<-length(x)
plot(sort(x),(1:n)/n, type="s",ylim=c(0,1))

#Qqplot
qqnorm(x)

#Boxplots IgM (Concentraciones de IgM en suero de 298 niños de 6 meses-6 años de edad
data(IgM)
?IgM
#par(mfrow=c(2,1)) dibuja una matriz de gráficos 2x1: un gráfico debajo de otro
par(mfrow=c(1,2)) #con mfrow los gráficos se organizarán por filas
boxplot(IgM) #boxplot sirve para crear un gráfico de cajas y bigotes
boxplot(log(IgM)) #se representa el mismográfico tras haberle aplicado logatirmos

par(mfrow=c(2,1)) #se pretende graficar la misma información, pero mostrando un gráfico encima del otro
boxplot(IgM)
boxplot(log(IgM))

################################################################################
#código 5

#Concentraciones de folatos en células sanguíneas en relación a
#tres tipos de ventilación durante la anestesia
data(red.cell.folate) #se cargan los datos de las concentraciones de folatos en células sanguíneas, guardadas en la librería
attach(red.cell.folate)
?red.cell.folate
summary(red.cell.folate)
tapply(folate,ventilation,mean)

#Para tener más de un estadístico resumen por grupo
m<-tapply(folate,ventilation,mean)
s<-tapply(folate,ventilation,sd)
n<-tapply(folate,ventilation,length)
cbind(mean=m,std.dev=s,n=n)

#parael dataset juul
data(juul)

tapply(igf1,tanner,mean)
tapply(igf1,tanner,mean,na.rm=T)

#Cargamos la base de datos energy
data(energy)
attach(energy)
summary(energy)
?energy

# Histogramas para cada grupo de mujeres
expend.lean<-expend[stature=="lean"]
expend.obese<-expend[stature=="obese"]
par(mfrow=c(2,1))
hist(expend.lean,breaks=10,xlim=c(5,13),ylim=c(0,4),col="white")
hist(expend.obese,breaks=10,xlim=c(5,13),ylim=c(0,4),col="black")

#Boxplots para cada grupo
par(mfrow=c(1,1))
boxplot(expend~stature)
boxplot(expend.lean,expend.obese)
#Con muestras tan pequeñas, los boxplots pueden resultar engañosos
#Se puede realizar gráficos de los datos originales, punto a punto
opar<-par(mfrow=c(2,2),mex=0.8,mar=c(3,3,2,1)+0.1)
stripchart(expend~stature)
stripchart(expend~stature,method="jitter")
stripchart(expend~stature,method="stack")
stripchart(expend~stature,method="stack",jitter=0.03)

################################################################################
#código 6

#Una tabla debe estar en un objecto tipo matriz
#Ejemplo: consumo de cafeína en mujeres según estado civil
caff.marital<-matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)
caff.marital
colnames(caff.marital)<-c("0","1-150","151-300",">300")
rownames(caff.marital)<-c("Married","Prev.married","Single")
caff.marital

detach(juul)
juul$sex<-factor(juul$sex,labels=c("M","F"))
juul$menarche<-factor(juul$menarche,labels=c("No","Yes"))
juul$tanner<-factor(juul$tanner,labels=c("I","II","III","IV","V"))
attach(juul)
summary(juul)
#También podríamos haber utilizado la función transform()
juul<-transform(juul,
                sex=factor(sex,labels=c("M","F")),
                menarche=factor(menarche,labels=c("No","Yes")),
                tanner=factor(tanner,labels=c("I","II","III","IV","V")) )
#También podemos crearla a partir de variables categóricas de un dataset
table(sex)
table(sex,menarche)
table(menarche,tanner)

#Podemos transponer las tablas
t(caff.marital)
#Para calcular las frecuencias marginales, por fila o columna
tanner.sex<-table(tanner,sex)
margin.table(tanner.sex,1)
margin.table(tanner.sex,2)
prop.table(tanner.sex,1)
prop.table(tanner.sex,1)*100
tanner.sex/sum(tanner.sex)

#También se pueden representar gráficamente tablas como por ejemplo
#con el diagrama de barras
total.caff<-margin.table(caff.marital,2)
total.caff
barplot(total.caff,col="white")
#Diagramas de barras para una tabla de contingencia
par(mfrow=c(2,2))
barplot(caff.marital,col="white")
barplot(t(caff.marital),col="white")
barplot(t(caff.marital),col="white",beside=T)
barplot(prop.table(t(caff.marital),2),col="white",beside=T)
par(mfrow=c(1,1))
#Otro diagrama de barras para una tabla de contingencia
barplot(prop.table(t(caff.marital),2),beside=T, legend.text=colnames(caff.marital),
        col=c("white","grey80","grey50","black"))
#Diagrama de sectores para una tabla de contingencia
opar<-par(mfrow=c(2,2),mex=0.8,mar=c(1,1,2,1))
slices<-c("white","grey80","grey50","black")
pie(caff.marital["Married",],main="Married",col=slices)
pie(caff.marital["Prev.married",],main="Previouslymarried",col=slices)
pie(caff.marital["Single",],main="Single",col=slices)
par(opar)
