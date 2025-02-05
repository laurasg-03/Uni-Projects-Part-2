setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-2")

#4. La diferencia entre el peso a los 4 meses de vida y el peso al nacer en 16 recién nacidos de una muestra, sigue una distribución
#N(µ, σ), y se supone que debe ser al menos de 2.5 kg. En un estudio de 16 niños a los que se le ha evaluado el peso al
#nacer y a los 4 meses, las diferencias en el peso han dado una media muestral de 2.340 Kg y una varianza muestral igual a
#0.36. ¿Hay evidencia para pensar que estos niños no se han desarrollado lo suficiente?. Interprete el p valor en el contexto
#de este contraste de hipótesis. Nota: para muestras pequeñas, o si la varianza poblacional es desconocida, el estadístico
#necesario para el contraste de hipótesis se distribuye según una ”t de Student” con n − 1 grados de libertad.

supuesta_media=2.5
nmuestral=16
media_muestral=2.340
desv_muestral= sqrt(0.36)

t = (media_muestral - supuesta_media)/(desv_muestral/sqrt(16)) #t=y'− µ/(s/√n)
t
p=pt(t, df=15, lower.tail=TRUE) #probabilidad de significación
p

#• Si p < 0.05, las diferencias son significativas a un nivel de confianza del 95%. 
#• Si p < 0.01, las diferencias serían entonces significativas a un nivel de confianza del 99%. 
#• Si p < 0.001, las diferencias serían significativas a un nivel de confianza del 99.9%.

#En la literatura científica, se suele utilizar tradicionalmente 0.05 como nivel
#de significación (construyendo intervalos de confianza al 95%).