####################################
########## LABORATORIO 08 ##########
##########   23-10-2017   ##########
####################################

##################
### Pregunta 1 ###
##################
a = read.table("/Users/nicholas/Desktop/EYP1016/Lab 8/xcauchy.txt", head=T)
logv = function(gamma){
    sum = 0
    for (item in v) {
       sum = sum + log(gamma/(pi*(item*item+gamma*gamma)))
    }
    sum
}
b = seq(0, 7, 0.1)
c = seq(0, 7, 0.1)
v = a$x
for (i in 1:length(b)) {
   c[i]=logv(b[i])
}
png(filename="Pregunta 1.png")
plot(b,c,type="l")
dev.off()

#c) Vemos en el grafico que el maximo ocurre alrededor del 2
c = optimize(logv, maximum = T, interval = c(0,7))
#names(c)
#d) Con algun otro metodo numerico se podria encontrar el EMV, ya que en a) vimos que no sepuede sacar analiticamente.
x = read.table("/Users/nicholas/Desktop/EYP1016/Lab 8/xcauchy2.txt", head =T)
asdf = c()
for (i in 1:500) {
    v=x[,i]
    asdf[i] = optimize(logv, maximum = T, interval = c(0,7))$maximum
}
Sesgo = mean(asdf)-2
print(Sesgo)
ECM = var(asdf)+Sesgo^2
print(ECM)
#Vemos un sesgo y un ECM muy pequeño, por lo que el valor estimado es bien cercano al teorico (2)