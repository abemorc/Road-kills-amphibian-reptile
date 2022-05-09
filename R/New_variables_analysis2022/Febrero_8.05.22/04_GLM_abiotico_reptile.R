




# GLMs variables abioticas para reptiles ---------------------------



df_abio_rep3 <- list_abio3[["Reptilia"]]
df_abio_rep3

# abio_all_cor <- df_abio_all %>% 
#   select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
#   cor(use ="complete.obs", method = "spearman")
# view(abio_all_cor)
# varsca <- findCorrelation(abio_all_cor, 0.9, verbose = T, names = T)
# varsca
# 
# df_abio_all_red <- df_abio_all %>% select(-any_of(varsca))
# df_abio_all_red
# str(df_abio_all_red)


gm_abio_rep3 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_rep3)
summary(gm_abio_rep3)
alias(gm_abio_rep3)
AIC(gm_abio_rep3)
BIC(gm_abio_rep3)
gmabiorepres3 <- resid(gm_abio_rep3)
qqnorm(gmabiorepres3)
hist(gmabiorepres3)
plot(density(gmabiorepres3))


backup_options <- options()
options(na.action = "na.fail")
modelabiorep3 <- dredge(gm_abio_rep3, trace = 2)
View(modelabiorep3)
sw(modelabiorep3)
swabiorep3 <- sw(modelabiorep3)
swabiorep3 <- tidy(swabiorep3)
names(swabiorep3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)

View(swabiorep)
View(swabiorep3)




gm_abio_rep_b3 <- eval(getCall(modelabiorep3, 2))
summary(gm_abio_rep_b3)
alias(gm_abio_rep_b3)

# graficos

visreg(gm_abio_rep_b3, xvar = "DIST_AGRICOLA_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Agrícolas",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)

visreg(gm_abio_rep_b3, xvar = "DIST_ELECTRICOS_MEAN", gg = FALSE,
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Zonas Eléctricas",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)

visreg(gm_abio_rep_b3, xvar = "TURNO", gg = FALSE,
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Turno",
       ylab = "Número de atropellamientos",
       line=list(col="orangered"),
       fill=list(col="orange")
)




# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de los reptiles para las 
# variables abioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc subtransecto)
# y la nueva sin esas variables
# 
View(swabiorep)
View(swabiorep3)






