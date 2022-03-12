

fvars_bio2 <- function(list_dfs) {
  
  list_dfs <- 
    map2(.x = list_dfs,
         .y = names(list_dfs),
         .f = ~
           if (.y=="Amphibia") {
             .x %>% select(SUBTRANSECTO, CONTEO, 
                           DEG, CHIL, RH_C, HI, DP, BULB, BARO, SPD, TC, TS,
                           TEMP_ASF, TEMP_SUELO, ALTITUD, TEMPORADA,
                           NDVI_VEGETACION_MEAN, NDWI_AGUA_MEAN, NDMI_HUMEDAD_MEAN,
                           DISTANCIA_ARROYOS_MEAN, DIST_PENDIENTE_MEAN, PESO,
                           DOSEL_MEDIA,NDWI_GAO) %>% 
               rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
           } else {
             .x %>% select(SUBTRANSECTO, CONTEO, 
                           DEG, CHIL, RH_C, HI, DP, BULB, BARO, SPD, TC, TS,
                           TEMP_ASF, TEMP_SUELO, ALTITUD, TEMPORADA,
                           NDVI_VEGETACION_MEAN, NDWI_AGUA_MEAN, NDMI_HUMEDAD_MEAN,
                           DISTANCIA_ARROYOS_MEAN, DIST_PENDIENTE_MEAN, LONG_TOTAL, PESO,
                           DOSEL_MEDIA, NDWI_GAO) %>% 
               rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
           }
    )
  return(list_dfs)
}

list_bio1 <- fvars_bio2(list_all_data)
View(list_bio1)



fvars_abio2 <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(SUBTRANSECTO, CONTEO,
                 DIAWEEK, DISTANCIA_CAÑADAS_MEAN, TURNO, 
                 DIST_URBANIZACION_MEAN, DIST_ELECTRICOS_MEAN,
                 DIST_AGRICOLA_MEAN, PENDIENTE_D, PENDIENTE_I)
        )
  
  return(list_dfs)
}

list_abio1 <- fvars_abio2(list_all_data)

View(list_bio1)
View(list_abio1)



# GLMs variables bioticas para todas las especies -------------------------

# reduccion de variables
#############################
df_bio_all <- list_bio1[["All"]]
df_bio_all

bio_all_cor <- df_bio_all %>% 
  select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(bio_all_cor)
varsc <- findCorrelation(bio_all_cor, 0.9, verbose = T, names = T)
varsc

df_bio_all_red <- df_bio_all %>% select(-any_of(varsc))
df_bio_all_red
str(df_bio_all_red)

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

gm_bio_all <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_all_red)
summary(gm_bio_all)
alias(gm_bio_all)
AIC(gm_bio_all)
BIC(gm_bio_all)
gmbioallres <- resid(gm_bio_all)
qqnorm(gmbioallres)
hist(gmbioallres)
plot(density(gmbioallres))


backup_options <- options()
options(na.action = "na.fail")
modelbioall <- dredge(gm_bio_all, trace = 2)
View(modelbioall)
sw(modelbioall)
swbioall <- sw(modelbioall)
swbioall <- tidy(swbioall)
names(swbioall) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swbioall)


gm_bio_all_b <- eval(getCall(modelbioall, 1))
summary(gm_bio_all_b)

# graficos
visreg(gm_bio_all_b, gg = FALSE, scale = "response", ask=FALSE)



# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de todas las especies y clases para las 
# variables bioticas
View(swbioall)







