setwd("C:/Users/Laura/Desktop/BIOESTADÍSTICA/P-2")

# Ejercicio 2.
#Cinco estudiantes preparan un examen de cierta asignatura en la que la probabilidad de que un alumno apruebe es de 2/5.
#determinar la probabilidad de que: a) todos aprueben, b) que aprueben al menos dos, c) que sólo aprueben dos, y d) que
#al menos apruebe uno.

#a P(x=5)
dbinom(5, size=5,prob=(2/5)) #calcular la probabilidad de que aprueben 5 de 5

#b P(x>=2)
1-pbinom(2, size=5,prob=(2/5)) + dbinom(2, size=5,prob=(2/5)) #calcular la probabilidad de que aprueben 1 - (menores a 2 de 5) + probabilidad de 2 (x>=2)
1-(dbinom(0, size=5,prob=(2/5)) + dbinom(1, size=5,prob=(2/5))) #para comprobar, se calcula la probabilidad complementaria a q aprueben 0 + que apruebe 1

#c P(x=2)
dbinom(2, size=5,prob=(2/5)) #calcular de manera binomial que sólo aprueban 2

#d P(x>=1)
1-pbinom(1, size=5,prob=(2/5)) + dbinom(1, size=5,prob=(2/5)) #calcular la probabilidad de que aprueben 1 - (menores a 1 de 5) + probabilidad de 1 (x>=1)

1-dbinom(0, size=5,prob=(2/5)) #para comprobar, se calcula la probabilidad complementaria a q aprueben 0