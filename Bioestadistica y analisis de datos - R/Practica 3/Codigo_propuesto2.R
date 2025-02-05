#Ejercicio 2
#Apartado a
setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-3")
region <- read.csv("MuestraTotalBases.csv", sep=",") #se abre el fichero csv.

#Apartado b
options(max.print=999999)
region
head(region)

#Apartado c
tabla=table(region)
addmargins(tabla)
prop.table(tabla)
addmargins(prop.table(tabla))

#Apartado d
set.seed(42) # fijamos la semilla de aleatorización para que sea reproducible
n=150
indices.muestra=sample(1:10000, size=n, replace=TRUE)
muestra.test.indep= region[indices.muestra, ]

tabla.indep=table(muestra.test.indep$tipo, muestra.test.indep$max.frec)
test.indep=chisq.test(tabla.indep)
test.indep

E <- test.indep$expected
O <- test.indep$observed
test.indep$expected
test.indep$observed
(O-E)^2/E

#Apartado e
set.seed(42) # fijamos la semilla de aleatorización para que sea reproducible
n2=50
tipo.A=region[region$tipo=="A",][sample(1:nrow(region[region$tipo=="A",]), size=n2),]
tipo.B=region[region$tipo=="B",][sample(1:nrow(region[region$tipo=="B",]), size=n2),]
tipo.C=region[region$tipo=="C",][sample(1:nrow(region[region$tipo=="C",]), size=n2),]

muestra.test.indep2=rbind(tipo.A,tipo.B,tipo.C)

tabla.indep2=table(muestra.test.indep2$tipo, muestra.test.indep2$max.frec)
test.indep2=chisq.test(tabla.indep2)
test.indep2



