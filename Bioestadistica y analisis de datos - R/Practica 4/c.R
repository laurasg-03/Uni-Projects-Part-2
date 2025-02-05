# APARTADO C: Selección de variable
setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P4")
data <-read.csv("Mortality_NHANES8894_NonSmokers")
dim(data)
data <- data[complete.cases(data),]
dim(data)
attach(data)
fit <- glm(prev.cvd~as.factor(smoking), family=binomial("logit"))
summary(fit)

# Un experto cardiólogo nos indica que además del tabaco, hay otros factores de 
# riesgo cardiovascular establecidos: el sexo, la edad, el colesterol alto, la diabetes,
# la enfermedad renal, el sedentarismo, y la hipertensión arterial.
# Muchos de estos factores están relacionados con estilos de vida y por tanto
# posiblemente correlacionados con el hábito tabáquico.
# Para descartar que la asociación cruda entre tabaco y historia de enfermedad
# cardiovascular no se explica por la confusión introducida por los otros, 
# factores de riesgo, procedemos ajustar un modelo de regresión logística que 
# incluye todos esos factores:
fit <- glm(prev.cvd~as.factor(smoking)+as.factor(riagendr)+ridageyr+diab+ckd+sedent+hbp+highchol, family=binomial("logit"))
summary(fit)


#Las tablas de desviación corresponden a las tablas ANOVA para regresión múltiple
#análisis y se generan así con la función anova():
anova(fit, test="Chisq")

# Note que la columna "Deviance" da diferencias entre modelos cuando las variables
# se agregan al modelo secuencialmente.
# Dichas desviaciones se distribuyen aproximadamente según una Ji cuadrado con los
# grados de libertad indicados.
#Es necesario agregar el argumento test="chisq" para obtener las pruebas de Ji cuadrado.
#Dado que la variable de "high cholesterol" es la última en introducirse secuencialmente
# en el modelos, es posible que no sea significativa porque no queda mucho margen
# en la deviance para poder explicar, o hemos metido con anterioridad variables
# relaciondas con high chol, que podrían, "robar" su efecto.
# Sin embargo, si las variables se reorganizan de modo que high cholesterol viene 
# la primera, obtenemos una prueba basada en la desviación para ver si, esa variable
# explica una parte de la desviación en ausencia de las otras variables:
fit <- glm(prev.cvd~highchol+as.factor(smoking)+as.factor(riagendr)+ridageyr+bmxbmi+diab+ckd+sedent+hbp, family=binomial("logit"))
summary(fit)
anova(fit, test="Chisq")


# De esta salida se puede desprender que high chol es eliminable, mientras que las otras no.
# También se podría re-configurar el orden de las otras variables explicativas para
# ver su relevancia en ausencia de las otras.
# Un método alternativo es usar drop1() para intentar eliminar un término a la vez:
drop1(fit, test="Chisq")


#Aquí LRT es la prueba de razón de verosimilitud, otro test para medir el cambio
# en la desviación. La información en las tablas de desviación es fundamentalmente
# la misma que la dada por las pruebas z en la tabla de coeficientes individuales de,
# regresión porque en la práctica la diferencia entre ambos tests suele ser pequeña
# debido a que en muestras grandes Ji cuadrado es igual a la distribución normal al
# cuadrado en pruebas con un solo grado de libertad.
# Sin embargo, para probar los factores con más de dos categorías, no hay más remedio 
# que usar la tabla de desviación ya que las pruebas z solo se relacionan con 
# coeficientes individuales y no con grupos de coeficientes, por ejemplo en el 
# contexto de variables, indicadores ("dummies") relacionadas con una variable 
# categórica de más de una categoría.


dat <- as.data.frame(cbind(c(3.4, 5.2, 7.0, 8.5, 10.5, 13.0, 18.0, 21.0, 28.0),
                           c(33, 32, 38, 37, 40, 37, 31, 37, 30), c(0, 5, 11, 14, 18, 21, 23, 30, 27)) )
names(dat) <- c("dosis", "n", "s")
attach(dat)
n=dat$n
s=dat$s
mice.tbl <- cbind(s,(n-s))
glm(mice.tbl~dosis,family=binomial("logit"))
prop.mice <- s/n
pchisq(q=102.909, df=(8-7), lower.tail = FALSE)
qchisq(p=0.05, df=7, lower.tail = FALSE)


Surv(peryr.exm.8yr, heart.8yr==1)[1:10]
