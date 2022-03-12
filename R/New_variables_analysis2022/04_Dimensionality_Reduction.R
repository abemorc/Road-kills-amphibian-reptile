
#
#

nums <- sapply(dfall, is.numeric)
data.numeric <- dfall[ , nums]
dfall
data.numeric
is.na(data.numeric)
anyNA(data.numeric)
data.without_na <- na.omit(data.numeric)
identical(data.numeric, data.without_na)
# cor_matrix <- cor(data.without_na[base::setdiff(names(data.without_na), 
#                                                 "N_INVESTIGATOR")], 
#                   use ="complete.obs",
#                   method = "spearman")
cor_matrix <- cor(data.without_na, 
                  use ="complete.obs",
                  method = "spearman")


View(cor_matrix)
str(cor_matrix)
findCorrelation(cor_matrix, 0.7, verbose = T, names = T)
varsc <- findCorrelation(cor_matrix, 0.9, verbose = T, names = T)

varsc
# sustituir variable dist urbanizacion con subtransecto

dfallreduced <- dfall %>% select(-any_of(varsc))
dfallreduced


# AllOk

#############################


dfallcor <- dfall %>% 
  select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")

varsc <- findCorrelation(dfallcor, 0.9, verbose = T, names = T)
varsc

dfallreduced <- dfall %>% select(-any_of(varsc))
dfallreduced


# randomforest
rf <- randomForest(CONTEO ~ ., dfallreduced, importance=TRUE)
importance(rf)
print(importance(rf))
i <- importance(rf)
best <- rownames(i)[order(i[, "%IncMSE"], decreasing=TRUE)[1:45]]
best










# random forest all data (50+) --------------------------------------------

##################
# randomforest
dfall <- list_all_data$All %>% select(-c(N_INVESTIGATOR, CARROS_TUX, CARROS_OAX,
                                         AÑO, DIAYEAR, TRANSECTO, VIVO, MUERTO,
                                         DIRECCION, UBICACION_T,
                                         CONDICION, DIST_RIOS_MEAN))
str(dfall)
rf <- randomForest(CONTEO ~ ., dfall, importance=TRUE)
importance(rf)
print(importance(rf))
imp <- importance(rf)
imp
best <- rownames(imp)[order(imp[, "%IncMSE"], decreasing=TRUE)[1:29]]
best <- na.omit(best)
best

df_bio_all_red1 <- df_bio_all %>% select(any_of(best))
df_bio_all_red1
str(df_bio_all_red1)
#################33









