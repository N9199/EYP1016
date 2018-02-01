####################################
########## LABORATORIO 07 ##########
##########   16-10-2017   ##########
####################################
library(mvtnorm)
##################
### Pregunta 1 ###
##################
a = seq(-3,3,0.1)
png(filename="Pregunta 1-0.png")
d=expand.grid(a,a)
s = matrix(0,ncol=length(a),nrow=length(a))
temp = matrix(c(1.0,0,0,1.0), ncol=2)
for (i in 1:length(a)) {
    for (j in 1:length(a)) {
       s[j,i]=dmvnorm(d[j+61*(i-1),], mean=c(0,0), sigma=temp)
    }
}
contour(a,a,s)
dev.off()
png(filename="Pregunta 1-1.png")
grid(a,a)
temp = matrix(c(1.0,0.5,0.5,1.0), ncol=2)
s = matrix(0,ncol=length(a),nrow=length(a))
for (i in 1:length(a)) {
    for (j in 1:length(a)) {
       s[j,i]=dmvnorm(d[j+61*(i-1),], mean=c(0,0), sigma=temp)
    }
}
contour(a,a,s)
dev.off()
#El comportamiento en cada eje es similar en los limites de los graficos es similar, pero en uno hay independencia total y en el otro hay dependencia.
png(filename="Pregunta 1-2.png")
grid(a,a)
temp = matrix(c(1.0,-0.9,-0.9,1.0), ncol=2)
s = matrix(0,ncol=length(a),nrow=length(a))
for (i in 1:length(a)) {
    for (j in 1:length(a)) {
       s[j,i]=dmvnorm(d[j+length(a)*(i-1),], mean=c(0,0), sigma=temp)
    }
}
contour(a,a,s)
dev.off()
#El comportamiento en cada eje es similar, pero inverso, ya que hay dependencia inversa (negativa) entre cada eje, la cual es mayor que en el segundo grafico
png(filename="Pregunta 1-3.png")
grid(a,a)
temp = matrix(c(1.0,0,0,9.0), ncol=2)
s = matrix(0,ncol=length(a),nrow=length(a))
for (i in 1:length(a)) {
    for (j in 1:length(a)) {
       s[j,i]=dmvnorm(d[j+61*(i-1),], mean=c(0,0), sigma=temp)
    }
}
contour(a,a,s)
dev.off()
#La gran diferencia entre cada distrubucion es la varianza de cada variable, una tiene una varianza mucho mas grande que la otra.
##################
### Pregunta 2 ###
##################
po = rpois(1000, 7)
bine = rnbinom(1000, 3, 0.2)
expo = rexp(1000, 5)
gam = rgamma(1000, 2, 3)

##Poisson
a = c()
for (i in 1:1000){
	a[i] = mean(po[1:i])
}
x = 1:1000
y = a
png(filename="Pregunta 2 Pois.png")
plot(x,y, type="l", main="Promedio de la M.A. Poisson", xlab="Tama�o de la Muestra", ylab="X")
EsPois = 7
abline(h = EsPois, col="yellow")
dev.off()

##Binomial Negativa
a = c()
for (i in 1:1000){
	a[i] = mean(bine[1:i])
}
x = 1:1000
y = a
png(filename="Pregunta 2 Binom.png")
plot(x,y, type="l", main="Promedio de la M.A. Binomial Negativa", xlab="Tamaño de la Muestra", ylab="X")
EsNeg = 3*(1-0.2)/0.2
abline(h = EsNeg, col="blue")
dev.off()

##Exponencial
a = c()
for (i in 1:1000){
	a[i] = mean(expo[1:i])
}
x = 1:1000
y = a
png(filename="Pregunta 2 Exp.png")
plot(x,y, type="l", main="Promedio de la M.A. Exponencial", xlab="Tama�o de la Muestra", ylab="X")
EsEx = 1/5
abline(h = EsEx, col="red")
dev.off()

##Gamma
a = c()
for (i in 1:1000){
	a[i] = mean(gam[1:i])
}
x = 1:1000
y = a
png(filename="Pregunta 2 Gamma.png")
plot(x,y, type="l", main="Promedio de la M.A. Gamma", xlab="Tama�o de la Muestra", ylab="X")
EsGam = 2/3
abline(h = EsGam, col="purple")
dev.off()

##Los gráficos convergen a un valor, el cual es su esperanza.
vmpo=c()
vvpo=c()
vmbine=c()
vvbine=c()
vvexpo=c()
vmexpo=c()
vvgam=c()
vmgam=c()

for (i in 1:1000){
	po = rpois(1000, 7)
	bine = rnbinom(1000, 3, 0.2)
	expo = rexp(1000, 5)
	gam = rgamma(1000, 2, 3)
	mpo = mean(po)
	mbine = mean(bine)
	mexpo = mean(expo)
	mgam = mean(gam)
	vpo = var(po)
	vbine = var(bine)
	vexpo = var(expo)
	vgam = var(gam)
	
	vmpo[i] = mpo
	vvpo[i] = vpo
	vmbine[i] = mbine
	vvbine[i] = vbine
	vmexpo[i] = mexpo
	vvexpo[i] = vexpo
	vmgam[i] = mgam
	vvgam[i] = vgam
}

##Esperanza de S^2 y varianza X 
###teóricos:
#Poisson:
#var(prom)=var/n=7/1000=0.007
#E(Var)=n/(n-1)Var=1000/999*7=7.007...

#Binom Negativa
#var(prom)=var/n=3(1-0.2)/0.2^2/1000=0.06
#E(Var)=n/(n-1)Var=1000/999*3(1-0.2)/0.2^2=60.06...

#Exponencial
#var(prom)=var/n=1/5^2*1/1000=4*10^-5
#E(Var)=n/(n-1)=1000/999*1/5^2=0.04...

#Gamma
#var(prom)=var/n=2/3^2*1/1000=0.000222222....
#E(Var)=n/(n-1)=1000/999*2/3^2=0.222444666888....

#Notamos que se parece mucho a lo esperado, y que para un n mas grande se deberia parecer aun mas.

###empíricos:
#Poisson:
vardxpo = var(vmpo)
medspo = mean(vvpo)
print(vardxpo)
print(medspo)
#Binomial Negativa:
vardxbi = var(vmbine)
medsbi = mean(vvbine)
print(vardxbi)
print(medsbi)
#Exponencial:
vardxex = var(vmexpo)
medsex = mean(vvexpo)
print(vardxex)
print(medsex)
#Gamma:
vardxga = var(vmgam)
medsga = mean(vvgam)
print(vardxga)
print(medsga)
