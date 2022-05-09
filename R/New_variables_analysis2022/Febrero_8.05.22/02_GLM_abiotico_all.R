



# GLMs variables abioticas para todas las especies ------------------------



df_abio_all3 <- list_abio3[["All"]]
df_abio_all3

# No es necesario eliminar variables correlacionadas debido a que son muy pocas
# variables


# abio_all_cor <- df_abio_all %>%
#   select(where(is.numeric), -c(SUBTRANSECTO, CONTEO)) %>%
#   cor(use ="complete.obs", method = "spearman")
# view(abio_all_cor)
# varsca <- findCorrelation(abio_all_cor, 0.8, verbose = T, names = T)
# varsca
# 
# df_abio_all_red <- df_abio_all %>% select(-any_of(varsca))
# df_abio_all_red
# str(df_abio_all_red)


gm_abio_all3 <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_all3)
summary(gm_abio_all)
summary(gm_abio_all3)
alias(gm_abio_all3)
AIC(gm_abio_all3)
BIC(gm_abio_all3)
gmabioallres3 <- resid(gm_abio_all3)
qqnorm(gmabioallres3)
hist(gmabioallres3)
plot(density(gmabioallres3))


backup_options <- options()
options(na.action = "na.fail")
modelabioall3 <- dredge(gm_abio_all3, trace = 2)
View(modelabioall3)
sw(modelabioall3)
swabioall3 <- sw(modelabioall3)
swabioall3 <- tidy(swabioall3)
names(swabioall3) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swabioall)
View(swabioall3)

gm_abio_all_b3 <- eval(getCall(modelabioall3, 5))
summary(gm_abio_all_b)
summary(gm_abio_all_b3)



# graficos

visreg(gm_abio_all_b3, xvar = "DIST_ELECTRICOS_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Zonas Eléctricas",
       ylab = "Número de atropellamientos",
       line=list(col="steelblue"),
       fill=list(col="steelblue1")
)

visreg(gm_abio_all_b3, xvar = "DIST_URBANIZACION_MEAN", gg = FALSE, scale = "response",
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Distancia a Zonas de Urbanización",
       ylab = "Número de atropellamientos",
       line=list(col="steelblue"),
       fill=list(col="steelblue1")
)

visreg(gm_abio_all_b3, xvar = "TURNO", gg = FALSE,
       main = "Todas las Clases",
       col.main= "gray60",
       sub = "variable abiótica",
       cex.sub=0.7,
       xlab = "Turno",
       ylab = "Número de atropellamientos",
       line=list(col="steelblue"),
       fill=list(col="skyblue")
)


# Conclusion: tenemos la lista en orden descendente de las variables que mas 
# influyen en el atropellamiento de todas las especies y clases para las 
# variables abioticas

# Se presentan ambas tablas, el anterior con las variables (longitud peso tc, subtransecto)
# y la nueva sin esas variables
# 
View(swabioall)
View(swabioall3)





