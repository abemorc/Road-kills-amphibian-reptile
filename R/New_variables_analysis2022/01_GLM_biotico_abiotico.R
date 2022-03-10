

# glm variables bioticas y abioticas


# se divide en tres subgrupos (clase): 
# - todas especies
# - reptiles
# - anfibios



# Todas las especies ------------------------------------------------------

View(list_main)

# variables ambientales + bioticas

fvars_bio <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(SUBTRANSECTO, TRANSECTO, CONTEO, 
                 DEG, CHIL, RH_C, ALTITUD, HI, DP, BULB, BARO, SPD, TC, TS,
                 TEMP_ASF, TEMP_SUELO, ALTITUD, TEMPORADA, ESCURRIMIENTOS,
                 NDVI_VEGETACION_MEAN, NDWI_AGUA_MEAN, NDMI_HUMEDAD_MEAN,
                 DIST_RIOS_MEAN, DIST_PENDIENTE_MEAN, LONG_TOTAL, PESO
                 ) %>% 
          rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
        )
  
  return(list_dfs)
}

# variables abioticas

fvars_abio <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(SUBTRANSECTO, TRANSECTO, CONTEO,
                 DIAWEEK, CAÑADA, TURNO, DIST_URBANIZACION_MEAN, 
                 DIST_ELECTRICOS_MEAN, DIST_AGRICOLA_MEAN
                 )
        )
  
  return(list_dfs)
}


list_allbio <- fvars_bio(list_main)
View(list_allbio)

list_allabio <- fvars_abio(list_main)
View(list_allabio)

# ---------------------------------------------------
# glm bioticas

dfbio_all <- list_allbio[[1]]
str(dfbio_all)

gmbio_all <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfbio_all)

summary(gmbio_all)
alias(gmbio_all)

gmbioall_res <- resid(gmbio_all)
gmbioall_res

# las variables seleccionadas bioticas presentan multicolinealidad, por lo que
# es necesario reducir el numero de variables mediante algun metodo.

# intentar eliminando informacion redundante de transecto

# dfbio_all <- dfbio_all[-c(1, 2)]  #no funciono

##########################3
# pruebas findcorrelation

nums <- sapply(dfbio_all, is.numeric)
data.numeric <- dfbio_all[ , nums]
dfbio_all
data.numeric
data.without_na <- na.omit(data.numeric)
cor_matrix <- cor(data.without_na)
View(cor_matrix)

varsc <- findCorrelation(cor_matrix, 0.9, verbose = T, names = F)
dfbio_all <- dfbio_all[-varsc]





#############################







# ----------------------------------------
# glm abioticas


dfabio_all <- list_allabio[[1]]
str(dfabio_all)



gmabio_all <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfabio_all)

summary(gmabio_all)
alias(gmabio_all)

gmabioallres <- resid(gmabio_all)
qqnorm(gmabioallres)
hist(gmabioallres)
plot(density(gmabioallres))

AIC(gmabio_all)
BIC(gmabio_all)


# parece que todo esta correcto, podemos buscar un mejor modelo

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
abiomodels <- dredge(gmabio_all, trace = 2)
sw(abiomodels)
abioall_sw <- sw(abiomodels)
abioall_sw <- tidy(abioall_sw)
names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")
View(abioall_sw)


# encontramos ya cuales son las variables abioticas que influyen mas en el 
# atropellamiento para todas las especies.
# falta para reptiles y anfibios por separado










# Anfibios ----------------------------------------------------------------




# Reptiles ----------------------------------------------------------------








