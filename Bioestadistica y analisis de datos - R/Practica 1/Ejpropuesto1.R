setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/Practica1")
datos <- read.csv("Datos.csv", sep=";") #se abre el fichero csv.
nombres <- c("id", "entorno.residencia", "sexo", "edad", "estado.civil", "nivel.educacion", "tiempo.educacion", 
             "sistolica", "diastolica", "altura", "peso") #creamos los nombres de nuestras columnas
options(max.print=999999) #sin este comando, se leían sólo las primeras 60 filas del csv.
datos <- read.table(file = "Datos.csv", header = FALSE, sep = ";",skip=1, col.names = nombres) #se guarda en datos la matriz con la información contenida en el csv.

#media, se calcula con mean
mean(datos$edad,na.rm=T) #na.rm=T sirve para despreciar los missing values
mean(datos$peso,na.rm=T) #datos$_____ sirve para acceder a una columna concreta
mean(datos$sistolica,na.rm=T)
mean(datos$diastolica,na.rm=T)

#desviacion estandar, se calcula con sd
sd(datos$edad,na.rm=T)
sd(datos$peso,na.rm=T)
sd(datos$sistolica,na.rm=T)
sd(datos$diastolica,na.rm=T)

#mediana , se calcula con median
median(datos$edad,na.rm=T)
median(datos$peso,na.rm=T)
median(datos$sistolica,na.rm=T)
median(datos$diastolica,na.rm=T)

#cuantiles , se calcula con quantile
quantile(datos$edad,na.rm=T, probs=c(0.25, 0.75))
quantile(datos$peso,na.rm=T, probs=c(0.25, 0.75))
quantile(datos$sistolica,na.rm=T, probs=c(0.25, 0.75))
quantile(datos$diastolica,na.rm=T, probs=c(0.25, 0.75))

#missing, se calcula sumando los datos NA
sum(is.na(datos$edad))
sum(is.na(datos$peso))
sum(is.na(datos$sistolica))
sum(is.na(datos$diastolica))

#frecuencias
sex.freq<-table(datos$sexo) #table nos crea una tabla cuyas filas son el número de veces que aparece cada valor diferente
estado.freq<-table(datos$estado.civil)

uno<-sex.freq[1] #queremos acceder a las dos columnas por separado que gurda la tabla de frecuencias de sex
dos<-sex.freq[2]

#porcentajes
porc.uno<- (uno/(uno+dos))*100 #el porcentaje de 1 es la frecuencia de 1 entre el total
porc.dos<- (dos/(uno+dos))*100
sex.freq.total<-uno+dos #número total de muestras
sex.porc.total<-porc.uno+porc.dos #porcentaje total (debería devolver "100")

nunc.casad<-estado.freq[1]  #queremos acceder a las dos columnas por separado que gurda la tabla de frecuencias de estado.civil
actual.casad<-estado.freq[2]
pareja<-estado.freq[3]
divorciado<-estado.freq[4]
viud<-estado.freq[5]

porcent.casad<- (nunc.casad/(nunc.casad+alctual.casad+pareja+divorciado+viud))*100 #el porcentaje de casados es la frecuencia de 1 entre el total
porcent.actual<- (actual.casad/(nunc.casad+alctual.casad+pareja+divorciado+viud))*100
porcent.parej<- (pareja/(nunc.casad+alctual.casad+pareja+divorciado+viud))*100
porcent.divorc<- (divorciado/(nunc.casad+alctual.casad+pareja+divorciado+viud))*100
porcent.viud<- (viud/(nunc.casad+alctual.casad+pareja+divorciado+viud))*100

estado.civil.freq.total<-nunc.casad+alctual.casad+pareja+divorciado+viud #número total de muestras
estado.civil.porcentaj.total<-porcent.casad+porcent.actual+porcent.parej+porcent.divorc+porcent.viud #porcentaje total (debería devolver "100")


#segunda parte del ejercicio propuesto 1

int_pesos <- seq(min(datos$peso,na.rm=TRUE),max(datos$peso,na.rm=TRUE),10) #secuenciar los intervalos de pesos desde el mínimo hasta el máximo, de 10 en 10

#estado civil
nunca.casado <- datos[datos$estado.civil=="1",]$peso #se guarda en el vector nunca.casado los valores del peso de aquellas personas casadas ("1")
actualmente.casado <- datos[datos$estado.civil=="2",]$peso
viviendo.pareja <- datos[datos$estado.civil=="3",]$peso
separado.divorciado <- datos[datos$estado.civil=="4",]$peso
viudo <- datos[datos$estado.civil=="5",]$peso

#sexo
sex1 <- datos[datos$sex=="1",]$peso
sex2 <- datos[datos$sex=="2",]$peso

#estudios
no.escuela <- datos[datos$nivel.educacion=="0",]$peso
inf.prim <- datos[datos$nivel.educacion=="1",]$peso
est.prim <- datos[datos$nivel.educacion=="2",]$peso
sec <- datos[datos$nivel.educacion=="3",]$peso
bach <- datos[datos$nivel.educacion=="4",]$peso
univ <- datos[datos$nivel.educacion=="5",]$peso
mast <- datos[datos$nivel.educacion=="6",]$peso

#entorno de residencia
urbano <- datos[datos$entorno.residencia=="1",]$peso
rural <- datos[datos$entorno.residencia=="2",]$peso

#estado civil
par(mfrow=c(3,2))
hist(pesos1, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Nunca casado", col="darkslategray1")
hist(pesos2, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Actualmente casado", col="darkslategray2")
hist(pesos3, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Viviendo en pareja", col="darkslategray3")
hist(pesos4, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Separado/a, Divorciado", col="darkslategray4")
hist(pesos5, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Viudo", col="darkslategray")

#sexo
par(mfrow=c(2,1))
hist(sex1, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Hombres", col="firebrick")
hist(sex2, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Mujeres", col="firebrick1")

#estudios
par(mfrow=c(3,3))
hist(no.escuela, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Nunca ha ido a la escuela", col="palevioletred1")
hist(inf.prim, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Inferior a estudios primarios", col="palevioletred2")
hist(est.prim, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Estudios primarios", col="palevioletred3")
hist(sec, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Secundaria", col="palevioletred")
hist(bach, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Bachillerato (o equivalente) completado", col="pink1")
hist(univ, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Estudios universitarios", col="pink2")
hist(mast, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Máster o Doctorado", col="pink3")

#entorno de residencia
par(mfrow=c(1,2))
hist(urbano, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Entorno de residencia urbano", col="springgreen1")
hist(rural, breaks=int_pesos,xlab="Peso, Kg",ylab="Frecuencia",main="Entorno de residencia rural", col="springgreen3")