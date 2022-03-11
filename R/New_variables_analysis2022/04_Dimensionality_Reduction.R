
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









