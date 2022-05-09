
View(list_bio3)
View(list_abio3)



# GLMs variables bioticas para anfibios ----------------------------

# reduccion de variables
#############################
df_bio_amp3 <- list_bio3[["Amphibia"]]
df_bio_amp3

bio_amp_cor3 <- df_bio_amp3 %>% 
  select(where(is.numeric), -c(CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
varscamp3 <- findCorrelation(bio_amp_cor3, 0.9, verbose = T, names = T)
varscamp3

df_bio_amp_red3 <- df_bio_amp3 %>% select(-any_of(varscamp3))
df_bio_amp_red3
str(df_bio_amp_red3)

# # randomforest
# rf <- randomForest(CONTEO ~ ., df_bio_amp_red, importance=TRUE)
# importance(rf)
# print(importance(rf))
# imp <- importance(rf)
# imp
# best <- rownames(imp)[order(imp[, "%IncMSE"], decreasing=TRUE)[1:30]]
# best <- na.omit(best)
# best
# 
# df_bio_amp_red1 <- df_bio_amp %>% select(any_of(best))
# df_bio_amp_red1
# str(df_bio_amp_red1)



# GLM

gm_bio_amp3 <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_amp_red3)
summary(gm_bio_amp)
summary(gm_bio_amp3)
alias(gm_bio_amp3)
AIC(gm_bio_amp3)
BIC(gm_bio_amp3)
gmbioampres3 <- resid(gm_bio_amp3)
qqnorm(gmbioampres3)
hist(gmbioampres3)
plot(density(gmbioampres3))


backup_options <- options()
options(na.action = "na.fail")
modelbioamp3 <- dredge(gm_bio_amp3, trace = 2)
View(modelbioamp3)
sw(modelbioamp3)
swbioamp3 <- sw(modelbioamp3)
swbioamp3 <- tidy(swbioamp3)
names(swbioamp3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swbioamp3)


gm_bio_amp_b3 <- eval(getCall(modelbioamp3, 1))
summary(gm_bio_amp_b3)

# graficos

visreg(gm_bio_amp_b3, xvar = "BARO", gg = FALSE, scale = "response",
       main = "Anfibios",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Presión Barométrica",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)

visreg(gm_bio_amp_b3, xvar = "DOSEL_MEDIA", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Dosel",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)


visreg(gm_bio_amp_b3, xvar = "PENDIENTES_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Pendiente carretera",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)





# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de los anfibios para las 
# variables bioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc subtransecto)
# y la nueva sin esas variables
# 
View(swbioamp)
View(swbioamp3)




