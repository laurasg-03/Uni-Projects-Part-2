setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-2")

# Ejercicio 3
#Una muestra aleatoria de valores de colesterol total (mg/dL) en menores de 21 años (X), 
#cuya distribución de probabilidad en la población se supone Normal, obtiene los siguientes
#resultados: 165, 162, 165, 166, 164, 165, 170, 169, y 168. Elaborar un intervalo de confianza
#al 99% para la media de la población e interprételo. 

muestra=c(165, 162, 165, 166, 164, 165, 170, 169, 168)
N=9
media_muestral=mean(muestra) #se calcula la media de la muestra
sigma= sd(muestra) #se valcula la varianza
z_a2=qt(0.01/2, df=N-1) #se calcula el cuantil del 99% (es decir, 0.01/2). df=grados de libertad
error= z_a2*(sigma/sqrt(N)) #se calcula el error
error

int_inf=media_muestral - (-error) #intervalo inferior
int_inf
int_sup=media_muestral + (-error) #intervalo superior
int_sup
