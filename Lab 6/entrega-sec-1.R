####################################
########## LABORATORIO 06 ##########
##########   02-10-2017   ##########
####################################

##################
### Pregunta 1 ###
##################
v1 = rpois(50, lambda=3)
v1 = table(v1)/50
v2 = rpois(500, lambda=3)
v2 = table(v2)/500
v3 = rpois(5000, lambda=3)
v3 = table(v3)/5000
png(filename="graficoP1-1.png")
plot(v1, type="h")
points(0:10,dpois(0:10, 3))
dev.off()
png(filename="graficoP1-2.png")
plot(v2, type="h")
points(0:10,dpois(0:10, 3))
dev.off()
png(filename="graficoP1-3.png")
plot(v3, type="h")
points(0:10,dpois(0:10, 3))
dev.off()
#Mientras mas grande la muestra mas se acerca al modelo teorico

##################
### Pregunta 2 ###
##################
png(filename="graficoP2-1.png")
plot(0:10, dbinom(x=0:10, size=10, p=0.5), type="p", col="gold")
dev.off()
png(filename="graficoP2-2.png")
plot(0:10, dbinom(x=0:10, size=10, p=0.8), type="p", col="deeppink")
dev.off()
png(filename="graficoP2-3.png")
plot(0:10, dbinom(x=0:10, size=10, p=0.2), type="p", col="steelblue")
dev.off()
#Se desplaza la media en cada uno, por lo que los valores van a tender a otro punto
##################
### Pregunta 3 ###
##################
png(filename="graficoP3.png")
step = 0.1
plot(seq(-5,5,step), dnorm(seq(-5,5,step), 0,1), type="l", col="yellow")
lines(seq(-5,5,step), dnorm(seq(-5,5,step), 2,1), col="aquamarine")
#El grafico se tralada horizontalmente, por lo que los datos tienden a ser mayores
lines(seq(-5,5,step), dnorm(seq(-5,5,step), 0,2), col="pink")
#El grafico se achata, o sea mayor dispersion de datos
abline(v=-1.4, col="green")
abline(v=1.4, col="green")
dev.off()
#El area de la curva con menor sigma es mayor entre las lineas
