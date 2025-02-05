setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-2")

# Ejercicio 1
# Realizar la representación gráfica de la función de distribución de X, y calcular las probabilidades P(1 < X ≤ 3.7), y P(1.5 ≤ X ≤ 3.5).
x= c(1,2,3,4,5)
Px= c(2/8,1/8,2/8,2/8,1/8)

plot(x, Px, type="h", xlab="X", ylab="P(X=x)", col=4, ylim=c(0,0.28), lwd = 5) #type = "h" para dibujar líneas verticales
grid(nx = NA, ny = NULL,lty = 1, col = "gray", lwd = 1) #cuadricular el gráfico

par(mfrow=c(1,2)) #obtener dos gráficos en la misma figura
plot(x, Px, type="h", xlab="X", ylab="P(1 < X ≤ 3.7)", col=4, ylim=c(0,0.28), lwd = 5)
abline(v = c(1,3.7), col=2) #añadir en x=1 y x=3.7 líneas rojas verticales para analizar el contenido entre medias
plot(x, Px, type="h", xlab="X", ylab="P(1.5 ≤ X ≤ 3.5)", col=4, ylim=c(0,0.28), lwd = 5)
abline(v = c(1.5,3.5), col=6)

Pintervalo=Px[2]+Px[3] #0.125+0.25=0.375, que corresponde a P(1 < X ≤ 3.7), y P(1.5 ≤ X ≤ 3.5)
Pintervalo