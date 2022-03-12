



# GLMs variables abioticas para anfibios --------------------------



df_abio_amp <- list_abio1[["Amphibia"]]
df_abio_amp

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


gm_abio_amp <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_amp)
summary(gm_abio_amp)
alias(gm_abio_amp)
AIC(gm_abio_amp)
BIC(gm_abio_amp)
gmabioampres <- resid(gm_abio_amp)
qqnorm(gmabioampres)
hist(gmabioampres)
plot(density(gmabioampres))


backup_options <- options()
options(na.action = "na.fail")
modelabioamp <- dredge(gm_abio_amp, trace = 2)
View(modelabioamp)
sw(modelabioamp)
swabioamp <- sw(modelabioamp)
swabioamp <- tidy(swabioamp)
names(swabioamp) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swabioamp)

gm_abio_amp_b <- eval(getCall(modelabioamp, 1))
summary(gm_abio_amp_b)

# graficos
visreg(gm_abio_amp_b, gg = FALSE, scale = "response", ask=FALSE)








