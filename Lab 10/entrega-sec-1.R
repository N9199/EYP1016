####################################
########## LABORATORIO 10 ##########
##########   13-11-2017   ##########
####################################

##################
### Pregunta 1 ###
##################
calc = function(sigma=1, epsilon, c, v = F, p = F){
    if(v){
        sigma = sqrt(sigma)
    }
    if(p){
        k = ceiling((qnorm(c)*sigma/epsilon)^2)
    }else{
        k = ceiling(0.25*(qnorm(c)/epsilon)^2)
    }
    k
}
##################
### Pregunta 2 ###
##################
png(filename="Grafico1.png")
x = seq(0.01,0.05,0.001)
y = c()
for (i in 1:length(x)) {
   y[i]=calc(epsilon=x[i], c=0.95, p=T)
}
plot(x,y, type='l')
dev.off()
##################
### Pregunta 3 ###
##################
print(calc(epsilon=0.05, c=0.95, p=T))
#Usando la encuesta https://www.adimark.cl/es/estudios/documentos/microestudio%20sexualidad%202017.pdf calcule la muestra minima (1083) y me di cuenta de que usaron una muestra mucho mas grande de lo necesario, dado el tamano muestral (1709), confianza (95%) y error (+-2,5%). Por lo que puede ser que la muestra no necesariamente distribuye normal
