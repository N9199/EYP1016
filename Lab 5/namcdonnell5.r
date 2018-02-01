# Pregunta 1
f1 = function(n){
    v = sample(1:6,n,replace=T, prob=c(0.1,0.1,0.1,0.1,0.3,0.3))
    arg = c(0,0,0,0,0,0)
    for (i in v) {
        arg[i] = arg[i]+1
    }
    for (i in 1:6) {
        arg[i] = arg[i]/n
    }
    png(filename = paste0("grafico",n,".png"))
    plot(1:6, arg,xlab="", ylab="Frecuencia relativa",main=paste0("Resultado de ",n," lanzamientos"),type="h")
    dev.off()
}
f1(50)
f1(100)
f1(1000)
f1(10000)

#Pregunta 2
v = sample(1:4,replace=T,1000, prob=c(0.1,0.2,0.3,0.4))
arg = c()
for (i in 1:1000) {
    arg = c(arg, mean(v[1:i]))
}
png(filename = "Pregunta 2 Prom.png")
plot(1:1000, arg, main="Promedio de la muesta aleatoria", xlab="Tamaño de la muestra", ylab="X", type="l")
abline(h = 3, col="blue")
dev.off()

arg = c()
for (i in 1:1000) {
    arg = c(arg, var(v[1:i]))
}
png(filename = "Pregunta 2 Var.png")
plot(1:1000, arg, main="Varianza de la muesta aleatoria", xlab="Tamaño de la muestra", ylab="X", type="l")
abline(h=1, col="blue")
dev.off()

#Pregunta 3
v = c()
for (i in 1:1000) {
    a = sample(0:1,1,prob=c(0.1,0.9))
    b = sample(0:1,1,prob=c(0.7,0.3))
    v = c(v, 40+a*10+b*15)
}
arg = c()
for (i in 1:1000) {
    arg = c(arg, mean(v[1:i]))
}
png(filename = "Pregunta 3.png")
plot(1:1000, arg, main="Promedio de la muesta aleatoria", xlab="Tamaño de la muestra", ylab="X", type="l")
abline(h =53.5, col="blue")
dev.off()