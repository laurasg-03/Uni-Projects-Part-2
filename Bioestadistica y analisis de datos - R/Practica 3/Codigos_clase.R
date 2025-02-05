setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-3")
prop.test(39,215,.15)
binom.test(39,215,.15)

asma.ingresos <- c(14,6,3)
asma.total <- c(16,12,11)
prop.test(asma.ingresos,asma.total)
asma.ingresos <- matrix(c(14,6,3,2,6, 8),3)
fisher.test(asma.ingresos)

chisq.test(asma.ingresos)
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)
matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital
chisq.test(caff.marital)

E <- chisq.test(caff.marital)$expected
O <- chisq.test(caff.marital)$observed
(O-E)^2/E

chisq.test(caff.marital)$expected
chisq.test(caff.marital)$observed

install.packages("epitools")
library(epitools)
RRtable<-matrix(c(1017,2260,165,992),nrow = 2, ncol = 2)
RRtable

# La siguiente línea le pide a R que calcule el RR y el intervalo de confianza del 95%
riskratio.wald(RRtable)
# El índice de riesgo y el intervalo de confianza del 95 % se enumeran en el resultado bajo $medida.
# Los estudios de casos y controles utilizan una razón de odds como medida de asociación,
# pero este procedimiento es muy similar al análisis anterior para RR.
ORtable<-matrix(c(1017,2260,165,992),nrow = 2, ncol = 2)
ORtable
oddsratio.wald(ORtable)


setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-3")
getwd()
data <-read.csv("Mortality_NHANES8894_NonSmokers.csv")
names(data)
dim(data)
data <- data[complete.cases(data),]
dim(data)
attach(data)
table(diab,heart.8yr)
riskratio.wald(table(diab,heart.8yr))
oddsratio.wald(table(diab,heart.8yr))


# Primero calcularemos el RR y sus intervalos de confianza correspondientes con una aproximación log-normal
library(epitools)
asa.tab<- matrix(c(11034-189,11037-104, 189,104),2,2)
epitab(asa.tab, method="riskratio")
# Simulación de razones de riesgo (RR) usando rbinom() para repetir 5,000 veces un experimento donde contamos el
# número de infartos de miocardio (IM) en dos poblaciones
# el parámetro probabilidad en la población viene definida por los resultados de la estudio de Henneken
# Lo primero es fijar una semilla de aleatorizacion para que los resultados sean reproducibles
set.seed(151)
tx <- rbinom(5000, 11037, 104/11037)
plac <- rbinom(5000, 11034, 189/11034)
# Para cada réplica, divida el número de resultados por el número de personas en cada población
# para obtener 5.000 estimaciones de riesgo para cada grupo (tratamiento y placebo
r.tx<-tx/11037
r.plac<-plac/11034
r.tx
r.plac
# Se calcula el riesgo relativo para cada simulación
rr.sim <- r.tx/r.plac
rr.sim

# Se describe la distribución de riesgos relativos resultante y se compara con los resultados de la aproximación log-normal obtenida con epitab()
mean(rr.sim)
quantile(rr.sim, c(0.025, 0.975))
sd(rr.sim)
hist(rr.sim)
hist(log(rr.sim))





