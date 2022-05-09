



# GLMs variables abioticas para anfibios --------------------------



df_abio_amp3 <- list_abio3[["Amphibia"]]
df_abio_amp3

# abio_amp_cor <- df_abio_amp %>% 
#   select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>% 
#   cor(use ="complete.obs", method = "spearman")
# view(abio_amp_cor)
# varsca <- findCorrelation(abio_amp_cor, 0.9, verbose = T, names = T)
# varsca
# 
# df_abio_amp_red <- df_abio_amp %>% select(-any_of(varsca))
# df_abio_amp_red
# str(df_abio_amp_red)


gm_abio_amp3 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_amp3)
summary(gm_abio_amp3)
alias(gm_abio_amp3)
AIC(gm_abio_amp3)
BIC(gm_abio_amp3)
gmabioampres3 <- resid(gm_abio_amp3)
qqnorm(gmabioampres3)
hist(gmabioampres3)
plot(density(gmabioampres3))


backup_options <- options()
options(na.action = "na.fail")
modelabioamp3 <- dredge(gm_abio_amp3, trace = 2)
View(modelabioamp3)
sw(modelabioamp3)
swabioamp3 <- sw(modelabioamp3)
swabioamp3 <- tidy(swabioamp3)
names(swabioamp3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swabioamp3)

gm_abio_amp_b3 <- eval(getCall(modelabioamp3, 1))
summary(gm_abio_amp_b3)

# graficos

visreg(gm_abio_amp_b3, xvar = "DIST_AGRICOLA_MEAN", gg = FALSE,
       main = "Anfibios",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Zonas agrícolas",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)

visreg(gm_abio_amp_b3, xvar = "DIST_URBANIZACION_MEAN", gg = FALSE,
       main = "Anfibios",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Zonas Urbanizadas",
       ylab = "Número de atropellamientos",
       line=list(col="olivedrab"),
       fill=list(col="yellowgreen")
)






# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de los anfibios para las 
# variables abioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc subtransecto)
# y la nueva sin esas variables
# 
View(swabioamp)
View(swabioamp3)







