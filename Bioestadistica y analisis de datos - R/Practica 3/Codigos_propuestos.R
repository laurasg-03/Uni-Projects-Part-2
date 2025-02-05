#Ejercicio 1
#Utilice los mismos números del ejemplo anterior para simular razones de odds en 
#lugar de riesgos relativos. Muestre su código. Compare los estimadores e intervalos 
#de confianza obtenidos empíricamente con el bootstrap con los obtenidos a partir de 
#la aproximación de la distribución log-normal.

library(epitools) #para poder utilizaar el método "oddsratio"
asa.tab<- matrix(c(11034-189,11037-104, 189,104),2,2) #misma matriz utilizada para el código 6
epitab(asa.tab, method="oddsratio") #estimadores e intervalos de confianza obtenidos empíricamente 
                                    #con el bootstrap


set.seed(151) #se fija una semilla de aleatorizacion para que los resultados sean reproducibles

#rbinom(5000)--> repetir 5000 veces un experimento donde contamos el número de infartos de miocardio en 2 poblaciones
tx <- rbinom(5000, 11037, 104/11037) #104 infartos de miocardio entre 11.037 personas en el grupo de tratamiento 
plac <- rbinom(5000, 11034, 189/11034) #189 casos de infato de miocardio de entre 11.034 personas en el grupo placebo.

#odds ratio: número de individuos que tienen una característica entre el número de quienes no la tienen.
o.tx<-tx/(11037-tx) #nº de personas con IM, entre el número de personas que no lo sufrieron (en el grupo de tratamiento)  
o.plac<-plac/(11034-plac) #nº de personas con IM, entre el número de personas que no lo sufrieron (en el grupo de placebo) 

or.sim <- o.tx/o.plac #se calcula el ratio de odds para cada simulación

mean(or.sim) #se obtiene una media de todos los ratios de odds obtenidos
quantile(or.sim, c(0.025, 0.975)) #los cuantiles teóricamente declaran el rango en el que se mueven los valores
sd(or.sim) #se calcula la desviación típica
hist(or.sim,main = "Histograma del Odds Ratio", xlab = "Simulaciones", ylab = "Frecuencia", col = "slategray1") #se crea un histograma 
hist(log(or.sim),main = "Histograma del logaritmo del Odds Ratio", xlab = "Simulaciones", ylab = "Frecuencia", col = "slategray2") #en este caso, al aplicar logaritmos nos aproximamos más a una distribución normal
