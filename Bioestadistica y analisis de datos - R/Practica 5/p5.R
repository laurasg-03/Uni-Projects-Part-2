#A continuación veremos ejemplos sobre el uso de la regresión logística utilizando datos tabulares, 
#en concreto los correspondientes a un experimento (Hemmingsen y Krogh, 1926) en el que a los ratones
#se les inyectó una dosis de insulina. 

#Primero cargaremos los datos:
dat <- as.data.frame(cbind(c(3.4, 5.2, 7.0, 8.5, 10.5, 13.0, 18.0, 21.0, 28.0),c(33, 32, 38, 37, 40, 37, 31, 37, 30), c(0, 5, 11, 14, 18, 21, 23, 30, 27)) )
names(dat) <- c("dosis", "n", "s")
#attach(dat)
n=dat$n
s=dat$s

#R puede ajustar análisis de regresión logística para datos tabulares de dos maneras diferentes. 
#En la primera opción, hay que especificar la respuesta como una matriz, donde una columna es el 
#número de “enfermos” y el otro es el número de “sanos” (o “éxito” y “fracaso”, según el contexto). 
#Es decir:
mice.tbl <- cbind(s,(n-s))
#La función cbind (”c” para ”columna”) se utiliza para vincular variables, por columnas, para formar 
#una matriz. 
#Hay que tener cuidado en no cometer el error de usar el conteo total para la columna 2 en lugar del
#número de fallos.

#Luego, se puede especificar el modelo de regresión logística como:
glm(mice.tbl~dosis,family=binomial("logit"))

#La otra forma de especificar un modelo de regresión logística para datos tabulares es dar la 
#proporción de enfermos en cada celda. 
#La salida correspondiente sería similar a la que se obtendría de objetos lm().
prop.mice <- s/n
glm(prop.mice~dosis, family=binomial("logit"),weights=n)

summary(glm(mice.tbl~dosis,family=binomial("logit")))

exp(cbind( coef(glm(mice.tbl~dosis,family=binomial("logit"))),
           confint(glm(mice.tbl~dosis,family=binomial("logit")) )))


fit <- glm(mice.tbl~dosis,family=binomial("logit"))
pred.frame <- data.frame(dose=c(3.4, 5.2, 7.0, 8.5, 10.5, 13.0, 18.0, 21.0, 28.0))
predict(fit, newdata=pred.frame)

# EJERCICIO 1: REGRESIÓN LOGÍSTICA
# APARTADO A: Interpretación de la salida del modelo logístico
dat <- as.data.frame(cbind(c(3.4, 5.2, 7.0, 8.5, 10.5, 13.0, 18.0, 21.0, 28.0),
                           c(33, 32, 38, 37, 40, 37, 31, 37, 30), 
                           c(0, 5, 11, 14, 18, 21, 23, 30, 27)) )
names(dat) <- c("dosis", "n", "s")
attach(dat)
dat

mice.tbl <- cbind(s,(n-s))
glm(mice.tbl~dosis,family=binomial("logit"))
prop.mice <- s/n
glm(prop.mice~dosis, family=binomial("logit"),weights=n)
qchisq(p=0.05, df=7, lower.tail = FALSE)
pchisq(q=102.909, df=(8-7), lower.tail = FALSE)
exp(cbind( coef(glm(mice.tbl~dosis,family=binomial("logit"))),
           confint(glm(mice.tbl~dosis,family=binomial("logit")))) 
    
    
# APARTADO B: Predicción y bondad de ajuste del modelo logístico
fit <- glm(mice.tbl~dosis,family=binomial("logit"))
pred.frame <- data.frame(dose=c(3.4, 5.2, 7.0, 8.5, 10.5, 13.0, 18.0, 21.0, 28.0))
predict(fit, newdata=pred.frame)
predict(fit, newdata=pred.frame, type="response") 


# CÓDIGO 1: 
# Regresión logística para datos tabulares
# Para datos tabulares es obvio tratar de comparar las proporciones observados y las
#predichas por el modelo logístico, para los valores, correspondientes a los observados.
plot(dosis, s/n, type="b", pch=16, ylim=c(0,1))
title("Sample proportions of sypmtoms vs. dose")
lines(x=dosis, y=predict(fit, newdata=pred.frame, type="response"), type="l", lty="dashed")
legend ("bottomright", legend= c("empirical probabilities", "Model 1: dose original scale"), lty=c(0, 2), cex=0.7, pch=c(16,NA))

# Sin embargo observamos que el modelo con la dosis en la escala original no se 
# ajusta del todo bien. ¿Que pasaría si log-transformamos la dosis?
fit2 <- glm(mice.tbl~log(dosis),family=binomial("logit"))
summary(fit2)
plot(dosis, s/n, type="b", pch=16, ylim=c(0,1))
title("Sample proportions of sypmtoms vs. dose")
lines(x=dosis, y=predict(fit, newdata=pred.frame, type="response"), type="l", lty="dashed")
lines(x=dosis, y=predict(fit2, newdata=log(pred.frame), type="response"), type="l", pch=3)
legend ("bottomright", legend= c("empirical probabilities", "Model 1: dose original scale", "Model 2: dose log scale"),
        lty=c(0, 2, 1), cex=0.7, pch=c(16,NA, NA))

# El problema con las probabilidades es que no se ven bien las frecuencias absolutas.
# Puede ser mejor mirar recuentos observados y esperados en lugar, de las probabilidades.
#Esto se calcularía así:
predict(fit, newdata=pred.frame, type="response")*n
predict(fit2, newdata=log(pred.frame), type="response")*n

#y para obtener una buena impresión para la comparación, puede usar
data.frame(fit1=predict(fit, newdata=pred.frame, type="response")*n,fit2=predict(fit2, newdata=log(pred.frame), type="response")*n,
           s,n)
# El modelo 2 parece ajustarse mejor a los datos.
qchisq(p=0.05, df=7, lower.tail = FALSE)



