

View(list_bio1)
View(list_abio1)

str(list_bio1)


# GLMs variables bioticas para reptiles-------------------------------------

# reduccion de variables
#############################
df_bio_rep <- list_bio1[["Reptilia"]]
df_bio_rep

bio_rep_cor <- df_bio_rep %>% 
  select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
View(bio_rep_cor)
varscrep <- findCorrelation(bio_rep_cor, 0.80, verbose = T, names = T)
varscrep

df_bio_rep_red <- df_bio_rep %>% select(-any_of(varscrep))
df_bio_rep_red
str(df_bio_rep_red)

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

gm_bio_rep <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_rep_red)
summary(gm_bio_rep)
alias(gm_bio_rep)
AIC(gm_bio_rep)
BIC(gm_bio_rep)
gmbiorepres <- resid(gm_bio_rep)
qqnorm(gmbiorepres)
hist(gmbiorepres)
plot(density(gmbiorepres))


backup_options <- options()
options(na.action = "na.fail")
modelbiorep <- dredge(gm_bio_rep, trace = 2)
View(modelbiorep)
sw(modelbiorep)
swbiorep <- sw(modelbiorep)
swbiorep <- tidy(swbiorep)
names(swbiorep) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swbiorep)


gm_bio_rep_b <- eval(getCall(modelbiorep, 1))
summary(gm_bio_rep_b)

# graficos

visreg(gm_bio_rep_b, xvar = "BARO", gg = FALSE, scale = "response",
       main = "Reptiles",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Presión Barométrica",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)

visreg(gm_bio_rep_b, xvar = "DISTANCIA_ARROYOS_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Distancia a Arroyos",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)



# Conclusion: las variables que mas  influyen en el atropellamiento de los 
# reptiles para las variables bioticas son las siguientes:
View(swbiorep)






