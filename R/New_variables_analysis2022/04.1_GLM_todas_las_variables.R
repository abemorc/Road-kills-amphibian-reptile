

##################
View(list_all_data$All)
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

dfall <- dfall %>% select(any_of(best), CONTEO)
dfall
str(dfall)
#################33

gmall <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = dfall)
summary(gmall)
alias(gmall)
AIC(gmall)
BIC(gmall)
gmallres <- resid(gmall)
# qqnorm(gmabioallres)
# hist(gmabioallres)
# plot(density(gmabioallres))
