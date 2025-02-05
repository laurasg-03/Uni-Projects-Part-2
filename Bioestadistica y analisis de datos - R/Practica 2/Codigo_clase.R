setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-2")


#Código 1: Funciones en R para simular distribuciones de probabilidad: la distribución binomial
# ¿Cual es la probabilidad que una binomial(10,0.2) tome exactamente el valor 2?
dbinom(2,size=10,prob=0.2)
# ¿Cual es la probabilidad de que una binomial(10,0.2) tome un valor inferior a 2?
pbinom(2,size=10,prob=0.2)
# ¿Qué valor de una binomial(10,0.2) presenta una probabilidad acumulada de 0.9 ?
qbinom(0.9,size=10,prob=0.2)
# Generación de 20 valores aleatorios de una distribución binomial(10,0.2)
rbinom(20,size=10,prob=0.2)
# Distribución de probabilidad y función de distribución acumulada de una binomial(10,0.2).
# Observe en la tabla inferior como al ser una distribución discreta, la distribución acumulada consiste en la suma de las probabilidades puntuales para cada X < x
x<-seq(0,10,by=1)
data.frame(x,p=dbinom(x,size=10,prob=0.2),F=pbinom(x,size=10,prob=0.2))
# Simulación de un experimento en el que una moneda está trucada con probabilidad de obtener "cara"=0.20 (en vez de 0.50 que sería la probabilidad de obtener "cara" si no estuviera trucada) según una binomial con 20 lanzamientos
x<-rbinom(20,size=1,prob=0.2)
x<-factor(x)
levels(x)<-c("TAIL","HEAD")
table(x)
# Obsérvese que si repetimos el experimento se obtiene diferente número de caras, ya que, como buena simulación, esta incluye el componente de variabilidad aleatoria

################################################################################

#Código 2: Demostración empírica de algunas propiedades de la distribución normal

#La función de densidad es probablemente el tipo de función en R menos usado en la práctica para la distribución normal. Pero si por ejemplo se desea  dibujar la conocido curva de campana de la distribución normal, entonces se puede hacer así:
x <- seq(-5,5,0.1)
plot(x, dnorm(x),type="l", col=2, ylim=c(0,1), ylab="dnorm(x, mean, sd)")
# La función seq() se usa para generar valores equidistantes, aquí de −5 a 5 en pasos de 0,1; es decir, (−5.0, −4.9, −4.8, ... , 4.9, 5.0).
# El uso de type="l" como argumento para trazar hace que la función dibuje líneas entre los puntos en lugar de trazar los puntos mismos.
# Por defecto, si no se especifican parámetros, dnorm() asume una normal tipificada media 0 y desviación típica 1. Vamos a observar que ocurre cuando se cambian los parámetros:
lines(x, dnorm(x, mean=0, sd=0.50), col =3)
lines(x, dnorm(x, mean=0, sd=2), col =4)
lines(x, dnorm(x, mean=-2, sd=1), col =6)
legend("topright",
       c("mu=0, sigma=1","mu=0, sigma=0.5","mu=0, sigma=2","mu=-2, sigma=1"),
       lty=1, lwd=1, col=c(2,3,4,6))
# Ahora comprobaremos empíricamente como una distribución discreta como la binomial se puede aproximar a la distribución normal con un tamaño muestral suficientemente grande
x <- seq(0,20, 1)
par(mfrow=c(2,2))
plot(x, dbinom(x, 0.10, size=10),type="h", col=2,
     ylim=c(0,0.40), ylab="P(X=x)", xlab="x (pi=0.10, n=10)")
plot(x, dbinom(x, 0.10, size=25),type="h", col=2,
     ylim=c(0,0.40), ylab="P(X=x)", xlab="x (pi=0.10, n=25)")
plot(x, dbinom(x, 0.10, size=50),type="h", col=2,
     ylim=c(0,0.40), ylab="P(X=x)", xlab="x (pi=0.10, n=50)")
plot(x, dbinom(x, 0.10, size=100),type="h", col=2,
     ylim=c(0,0.40), ylab="P(X=x)", xlab="x (pi=0.10, n=100)")
# En el panel con N=100, se tiene además la función de densidad de una distribución normal con media N*pi = 100 * 0.10=10, y varianza N*pi(1-pi)=100*0.1*0.90=9
lines(x, dnorm(x, 10, sqrt(9)), col =1)

################################################################################

xbar <- 83; sigma <- 12; n <- 5
sem <- sigma/sqrt(n)
sem

xbar + sem * qnorm(0.025)
xbar + sem * qnorm(0.975)


################################################################################

muestra <- c(176,168,173,167,181,167); mu=176.6; media = mean(muestra)
media

s = sqrt(var(muestra))
s

t = (media - mu)/(s/sqrt(length(muestra)))
t

pt(t, df=5, lower.tail=TRUE)

2*pt(t, df=5, lower.tail=TRUE)

