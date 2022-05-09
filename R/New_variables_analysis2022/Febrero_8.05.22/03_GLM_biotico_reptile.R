

View(list_bio3)
View(list_abio3)

str(list_bio3)


# GLMs variables bioticas para reptiles-------------------------------------

# reduccion de variables
#############################
df_bio_rep3 <- list_bio3[["Reptilia"]]
df_bio_rep3

bio_rep_cor3 <- df_bio_rep3 %>% 
  select(where(is.numeric), -c(CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(bio_rep_cor3)
varscrep3 <- findCorrelation(bio_rep_cor3, 0.80, verbose = T, names = T)
varscrep3

df_bio_rep_red3 <- df_bio_rep3 %>% select(-any_of(varscrep3))
df_bio_rep_red3
str(df_bio_rep_red3)

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

# GLM

gm_bio_rep3 <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_rep_red3)
summary(gm_bio_rep3)
alias(gm_bio_rep3)
AIC(gm_bio_rep3)
BIC(gm_bio_rep3)
gmbiorepres3 <- resid(gm_bio_rep3)
qqnorm(gmbiorepres3)
hist(gmbiorepres)
plot(density(gmbiorepres))


backup_options <- options()
options(na.action = "na.fail")
modelbiorep3 <- dredge(gm_bio_rep3, trace = 2)
View(modelbiorep3)
sw(modelbiorep3)
swbiorep3 <- sw(modelbiorep3)
swbiorep3 <- tidy(swbiorep3)
names(swbiorep3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swbiorep3)


gm_bio_rep_b3 <- eval(getCall(modelbiorep3, 1))
summary(gm_bio_rep_b3)

# graficos

visreg(gm_bio_rep_b3, xvar = "BARO", gg = FALSE, scale = "response",
       main = "Reptiles",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Presión Barométrica",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)

visreg(gm_bio_rep_b3, xvar = "DISTANCIA_ARROYOS_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Distancia a Arroyos",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)



# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de los reptiles para las 
# variables bioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc subtransecto)
# y la nueva sin esas variables
# 
View(swbiorep)
View(swbiorep3)






