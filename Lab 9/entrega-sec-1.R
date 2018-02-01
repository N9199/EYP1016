####################################
########## LABORATORIO 09 ##########
##########   30-10-2017   ##########
####################################

##################
### Pregunta 1 ###
##################

f = function(z){
    n = 100
    norm = rnorm(n,3,1)
    c = sqrt(var(norm))
    a = c*z/5
    mu = mean(norm)
    IC = c()
    res = (3>=mu-c*z/10 && 3<=mu+c*z/10)
    ans = c(res, a)
    ans
}
vec1 = matrix(data = rep(0,2000), nrow = 2, ncol = 1000)
for( i in 1:1000){
    vec1[,i] = f(1.96)
}
sum = 0
for( i in 1:1000){
    sum = sum + vec1[1,i]
}
sum = sum/1000.0
sum
# Se relacionan ya que el nivel de confianza es la probabilidad de que la media este dentro del intervalo.

vec2 = matrix(data = rep(0,2000), nrow = 2, ncol = 1000)
for( i in 1:1000){
    vec2[,i] = f(2.58)
}
sum = 0
for( i in 1:1000){
    sum = sum + vec2[1,i]
}
sum = sum/1000.0
sum

vec3 = matrix(data = rep(0,2000), nrow = 2, ncol = 1000)
for( i in 1:1000){
    vec3[,i] = f(1.64)
}
sum = 0
for( i in 1:1000){
    sum = sum + vec3[1,i]
}
sum = sum/1000.0
sum

#e
mean(vec1[2,])
mean(vec2[2,])
mean(vec3[2,])
#Mientras mas grande el intervalo mas alto es la confianza del intervalo
#Mientas mas alta la confianza mas ancho el intervalo
#Si, por la formula de intervalo de confianza.