


# GLM modelo --------------------------------------------------------------

view(xxx)


gmmolde <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = xxx)
summary(gmmolde)
1-gmmolde$deviance/gmmolde$null.deviance

####
#construccion de todos las combinaciones posibles de variables
backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace inversa
fullmodels1 <- dredge(gmmolde)
View(fullmodels1)
sw(fullmodels1)

#funcion de enlace logaritmica
fullmodels2 <- dredge(gma2)
View(fullmodels2)
sw(fullmodels2)


options(backup_options)




# retirar variables -------------------------------------------------------

# metodo analitico --------------------------------------------------------

# retirar variables mediante el grado de correlacion que existe entre ellas:

#asignar variables para prueba
mcormain <- list_cor$Main
mcormain <- as.matrix(mcormain)
str(mcormain)


#encontrar variables
index_quitar <- findCorrelation(x = mcormain, cutoff = 0.2, names = FALSE)
index_quitar
index <- which(index_quitar==2)
index
index_quitar <- index_quitar[-index]

dfsub2 <- list_ready$Main
dfsub2 <- dfsub2[,-index_quitar]
str(dfsub2)


# models dredge -----------------------------------------------------------

dfsub2 <- dfsub2[,c(2,5:10)]
str(dfsub2)

gmmolde1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfsub2)
summary(gmmolde1)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde1, gg = TRUE, scale = "response")

## modelo 1

library(broom)

a <- tidy((gmmolde1))
b <- glance(gmmolde1)

write.csv(a, here("Tereso/GLM 1/output data/glm1.csv"))
write.csv(b, here("Tereso/GLM 1/output data/glm1_performance.csv"))


backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace inversa
fullmodels1 <- dredge(gmmolde1, trace = 2) 
View(fullmodels1)
sw(fullmodels1)

#funcion de enlace logaritmica
fullmodels2 <- dredge(gma2)
View(fullmodels2)
sw(fullmodels2)


options(backup_options)

# es indispensable reducir el modelo, con 31 variables la cantidad de tiempo 
# en resolverse es abismal

# FAIL




# modelo seleccionado por criterio de akaike
gmmolde2 <- glm(formula = CONTEO ~ ALTITUD_MEAN + DIST_ELECTRICOS_MEAN +
                  NDMI_HUMEDAD_MEAN+ NDWI_AGUA_MEAN, family = poisson(link = "log"), data = dfsub2)
summary(gmmolde2)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde2, gg = TRUE, 
       scale="response")


ccc <- tidy((gmmolde2))
ddd <- glance(gmmolde2)

write.csv(ccc, here("Tereso/GLM 2/output data/glm2.csv"))
write.csv(ddd, here("Tereso/GLM 2/output data/glm2_performance.csv"))







xx <- list_ready$Main
write.csv(x = xx, file = here("Data/Ready_data/obs.csv"))
