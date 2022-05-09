

fvars_bio3 <- function(list_dfs) {
  
  list_dfs <- 
    map2(.x = list_dfs,
         .y = names(list_dfs),
         .f = ~
           if (.y=="Amphibia") {
             .x %>% select(CONTEO, 
                           DEG, CHIL, RH_C, HI, DP, BULB, BARO, SPD, TS,
                           TEMP_ASF, TEMP_SUELO, ALTITUD, TEMPORADA,
                           NDVI_VEGETACION_MEAN, NDWI_AGUA_MEAN, NDMI_HUMEDAD_MEAN,
                           DISTANCIA_ARROYOS_MEAN, DIST_PENDIENTE_MEAN,
                           DOSEL_MEDIA,NDWI_GAO) %>% 
               rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
           } else {
             .x %>% select(CONTEO, 
                           DEG, CHIL, RH_C, HI, DP, BULB, BARO, SPD, TS,
                           TEMP_ASF, TEMP_SUELO, ALTITUD, TEMPORADA,
                           NDVI_VEGETACION_MEAN, NDWI_AGUA_MEAN, NDMI_HUMEDAD_MEAN,
                           DISTANCIA_ARROYOS_MEAN, DIST_PENDIENTE_MEAN,
                           DOSEL_MEDIA, NDWI_GAO) %>% 
               rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
           }
    )
  return(list_dfs)
}

View(list_all_data)
list_bio3 <- fvars_bio3(list_all_data)
View(list_bio3)



fvars_abio3 <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(CONTEO,
                 DIAWEEK, DISTANCIA_CAÑADAS_MEAN, TURNO, 
                 DIST_URBANIZACION_MEAN, DIST_ELECTRICOS_MEAN,
                 DIST_AGRICOLA_MEAN, PENDIENTE_D, PENDIENTE_I)
        )
  
  return(list_dfs)
}

list_abio3 <- fvars_abio3(list_all_data)

View(list_bio1)
View(list_abio1)

# nuevos dataset sin las variables subtransecto, longitud y peso
View(list_bio3)
View(list_abio3)



# GLMs variables bioticas para todas las especies -------------------------

# reduccion de variables
#############################

df_bio_all3 <- list_bio3[["All"]]
df_bio_all3

bio_all_cor3 <- df_bio_all3 %>% 
  select(where(is.numeric), -c(CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(bio_all_cor3)
varsc3 <- findCorrelation(bio_all_cor3, 0.9, verbose = T, names = T)
varsc3

df_bio_all_red3 <- df_bio_all3 %>% select(-any_of(varsc3))
df_bio_all_red3
str(df_bio_all_red3)

# # randomforest
# rf <- randomForest(CONTEO ~ ., df_bio_all_red, importance=TRUE)
# importance(rf)
# print(importance(rf))
# imp <- importance(rf)
# imp
# best <- rownames(imp)[order(imp[, "%IncMSE"], decreasing=TRUE)[1:30]]
# best <- na.omit(best)
# best
# 
# df_bio_all_red1 <- df_bio_all %>% select(any_of(best))
# df_bio_all_red1
# str(df_bio_all_red1)



# ##################
# # randomforest
# dfall <- list_all_data$All %>% select(-c(N_INVESTIGATOR, CARROS_TUX, CARROS_OAX,
#                                          AÑO, DIAYEAR, TRANSECTO, VIVO, MUERTO,
#                                          DIRECCION, UBICACION_T,
#                                          CONDICION, DIST_RIOS_MEAN))
# str(dfall)
# rf <- randomForest(CONTEO ~ ., dfall, importance=TRUE)
# importance(rf)
# print(importance(rf))
# imp <- importance(rf)
# imp
# best <- rownames(imp)[order(imp[, "%IncMSE"], decreasing=TRUE)[1:29]]
# best <- na.omit(best)
# best
# 
# df_bio_all_red1 <- df_bio_all %>% select(any_of(best))
# df_bio_all_red1
# str(df_bio_all_red1)
# #################33

# GLM

gm_bio_all3 <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_all_red3)
summary(gm_bio_all)
summary(gm_bio_all3)
alias(gm_bio_all3)
AIC(gm_bio_all3)
BIC(gm_bio_all3)
gmbioallres3 <- resid(gm_bio_all3)
qqnorm(gmbioallres3)
hist(gmbioallres3)
plot(density(gmbioallres3))


backup_options <- options()
options(na.action = "na.fail")
modelbioall3 <- dredge(gm_bio_all3, trace = 2)
View(modelbioall3)
sw(modelbioall3)
swbioall3 <- sw(modelbioall3)
swbioall3 <- tidy(swbioall3)
names(swbioall3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)

View(swbioall)
View(swbioall3)


gm_bio_all_b3 <- eval(getCall(modelbioall3, 1))
summary(gm_bio_all_b)
summary(gm_bio_all_b3)

# graficos
visreg(gm_bio_all_b3, xvar = "BARO", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Presión Barométrica",
       ylab = "Número de atropellamientos",
       line=list(col="steelblue"),
       fill=list(col="skyblue")
       )

visreg(gm_bio_all_b3, xvar = "DOSEL_MEDIA", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Dosel arbóreo ",
       ylab = "Número de atropellamientos",
       line=list(col="steelblue"),
       fill=list(col="skyblue")
       )




# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de todas las especies y clases para las 
# variables bioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc subtransecto)
# y el nuevo sin esas variables
# 
View(swbioall)
View(swbioall3)







