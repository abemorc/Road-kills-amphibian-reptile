




# GLMs variables abioticas para reptiles ---------------------------



df_abio_rep <- list_abio1[["Reptilia"]]
df_abio_rep

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


gm_abio_rep <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_rep)
summary(gm_abio_rep)
alias(gm_abio_rep)
AIC(gm_abio_rep)
BIC(gm_abio_rep)
gmabiorepres <- resid(gm_abio_rep)
qqnorm(gmabiorepres)
hist(gmabiorepres)
plot(density(gmabiorepres))


backup_options <- options()
options(na.action = "na.fail")
modelabiorep <- dredge(gm_abio_rep, trace = 2)
View(modelabiorep)
sw(modelabiorep)
swabiorep <- sw(modelabiorep)
swabiorep <- tidy(swabiorep)
names(swabiorep) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swabiorep)

gm_abio_rep_b <- eval(getCall(modelabiorep, 2))
summary(gm_abio_rep_b)
alias(gm_abio_rep_b)

# graficos
visreg(gm_abio_rep_b, gg = FALSE, scale = "response", ask=FALSE)






