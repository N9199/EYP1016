## Pregunta 1
area = function(a=0,b=0,r=0,t=""){
    if(a<0 || b<0 || r<0){
        print("Error, argumentos negativos")
        return ()
    }
    if(t=="rectangulo"){
        print(paste0("El area del ",t," es ",a*b))
    }else if(t=="circulo"){
        print(paste0("El area del ",t," es ",pi*r^2))
    }else if(t=="triangulo"){
        print(paste0("El area del ",t," es ",a*b/2))
    }
}

## Pregunta 2
loteria = function(nums=c()){
    if(length(nums)!=5){
        print("Argumento invalido")
        return
    }
    for(i in 1:length(nums)){
        if(nums[i]>20 || nums[i]<1){
            print("Argumento invalido")
            return
        }
    }
    lot = sample(1:20,5)
    sort(lot)
    sort(nums)
    for(i in 1:5){
        if(lot[i]!=nums[i]){
            print(paste0("Usted es el usuario numero 1.0000.0000, sus numeros son ", nums[1],' ',nums[2],' ',nums[3],' ',nums[4],' ',nums[5], " y usted perdio"))
            return ()
        }
    }
    print(paste0("Usted es el usuario numero 1.0000.0000, sus numeros son ", nums, " y usted gano"))
}

## Pregunta 3

summary = function(table){
    temp = c()
    for (i in 1:dim(table)[2]){
        if (is.numeric(table[,i])){
            temp = c( c(names(data)[i], mean(table[,i]), min(table[,i]), max(table[,i])))
        }
    }
    return (list(pdata = temp))
}