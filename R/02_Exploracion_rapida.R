# en el dataset hay varias variables con valores faltantes, veamos cuales y
# cuantas son

faltantes <- function(x) {
  any(is.na(x))
}

View(df1)

faltan <- apply(df1, MARGIN = 2, FUN = faltantes)
View(faltan)

#vemos ya en que variables nos faltan valores,

#cuantas son??
sum(faltan)
length(df1)-1

(length(df1)-1) - sum(faltan)



# Por lo general es necesario (pero no siempre) reemplazar los valores faltantes
# para asi poder ocupar todas las muestras recolectadas, existen diferentes
# metodos dependiendo de la variable y del tipo de dato, asi como del numero 
# faltante de estos, una buena aproximacion es identificar aquellas variables en
# las que falten menos del 30% - 40% de los datos 

faltantes1 <- function(x) {
  sum(is.na(x))
}
faltan1 <- apply(df1, MARGIN = 2, FUN = faltantes1)
View(faltan1)
sort(faltan1, decreasing = T)
# vemos las variables en las que faltan mas datos

# veamos los porcentajes de valores faltantes
faltantes2 <- function(x) {
  (sum(is.na(x)) / length(x)) * 100
}
faltan2 <- apply(df1, MARGIN = 2, FUN = faltantes2)
faltan2
View(faltan2)

#vemos cuales son las variables que mas falta de datos tienen
sort(faltan2, decreasing = T)

#veamos aquellas en donde el porcentaje sea mayor
porcentaje <- 40
faltan2[faltan2 > porcentaje]
# pues afortunadamente vemos que no son tantas las variables que tendriamos que
# descartar
# en las demas es necesario sutituir. Esta parte la hare talvez por algun 
# valor representativo del conjunto, o posiblemente mediante algun modelo 
# de regresion (necesito mas informacion de que representan esas variables)



# por lo mientras es posible obtener algunas estadisticas descriptivas 
# del conjunto de datos

summary(df1$MIN)
summary(df1$CLASE)

resum <- function(x) {
  summary(x)
}
dfprueba <- df1
dfprueba <- as.data.frame(dfprueba)

# obtengamos un resumen de las variables numericas excluyendo los NA

indices <- sapply(X = df1, FUN = is.numeric)
indices
resumenes1 <- apply(X = dfprueba[,indices], MARGIN = 2, FUN = resum)
resumenes1

#usando el paquete plyr (no es necesario al menos ahora)
#lprueba <- alply(.data = dfprueba, .margins = 2, .fun = resum)
#lprueba

#increible que esta sea la forma mas facil
resumenes <- sapply(X = df1, FUN = resum)
resumenes$Y_LAT
resumenes

# veo algo raro en la variable latitud y lngitud, ver que onda

#ahora valores unicos
unicos <- sapply(X = df1, FUN = unique)
unicos


resum1 <- function(x) {
  data.frame(var = var(x, na.rm = T),
             sd = sd(x, na.rm = T),
             rango = range(x, na.rm = T))
}
dispersion <- sapply(X = df1, FUN = resum1, simplify = F)
dispersion
str(dispersion)

### aqui puedo identificar las variables con mayor dispersion, seleccionarlas 
# automaticamentey en esas aplicar algun tipo de transformacion y eliminar
# outliers

any(is.na(df1))
any(is.na(df2))
any(is.na(df3))
any(is.na(df4))

#en los tres ultimos datasets no hay valores faltantes ni problemas













