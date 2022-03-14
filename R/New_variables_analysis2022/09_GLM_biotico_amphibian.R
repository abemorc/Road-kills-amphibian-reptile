
View(list_bio1)
View(list_abio1)



# GLMs variables bioticas para anfibios ----------------------------

# reduccion de variables
#############################
df_bio_amp <- list_bio1[["Amphibia"]]
df_bio_amp

bio_amp_cor <- df_bio_amp %>% 
  select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
  cor(use ="complete.obs", method = "spearman")
varscamp <- findCorrelation(bio_amp_cor, 0.9, verbose = T, names = T)
varscamp

df_bio_amp_red <- df_bio_amp %>% select(-any_of(varscamp))
df_bio_amp_red
str(df_bio_amp_red)

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

gm_bio_amp <- glm(formula = CONTEO ~.,
                  family = poisson(link = "log"), 
                  data = df_bio_amp_red)
summary(gm_bio_amp)
alias(gm_bio_amp)
AIC(gm_bio_amp)
BIC(gm_bio_amp)
gmbioampres <- resid(gm_bio_amp)
qqnorm(gmbioampres)
hist(gmbioampres)
plot(density(gmbioampres))


backup_options <- options()
options(na.action = "na.fail")
modelbioamp <- dredge(gm_bio_amp, trace = 2)
View(modelbioamp)
sw(modelbioamp)
swbioamp <- sw(modelbioamp)
swbioamp <- tidy(swbioamp)
names(swbioamp) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swbioamp)


gm_bio_amp_b <- eval(getCall(modelbioamp, 1))
summary(gm_bio_amp_b)

# graficos

visreg(gm_bio_amp_b, xvar = "BARO", gg = FALSE, scale = "response",
       main = "Anfibios",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Presión Barométrica",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)

visreg(gm_bio_amp_b, xvar = "DOSEL_MEDIA", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Dosel",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)


visreg(gm_bio_amp_b, xvar = "PENDIENTES_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable biótica",
       cex.sub=0.7,
       xlab = "Pendiente carretera",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)



View(swbioamp)




