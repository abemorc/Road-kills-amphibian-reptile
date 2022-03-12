



# GLMs variables abioticas para todas las especies ------------------------



df_abio_all <- list_abio1[["All"]]
df_abio_all

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


gm_abio_all <- glm(formula = CONTEO ~.,
                   family = poisson(link = "log"), 
                   data = df_abio_all)
summary(gm_abio_all)
alias(gm_abio_all)
AIC(gm_abio_all)
BIC(gm_abio_all)
gmabioallres <- resid(gm_abio_all)
qqnorm(gmabioallres)
hist(gmabioallres)
plot(density(gmabioallres))


backup_options <- options()
options(na.action = "na.fail")
modelabioall <- dredge(gm_abio_all, trace = 2)
View(modelabioall)
sw(modelabioall)
swabioall <- sw(modelabioall)
swabioall <- tidy(swabioall)
names(swabioall) <- c("Variables", "Suma de los pesos de akaike")
options(backup_options)
View(swabioall)

gm_abio_all_b <- eval(getCall(modelabioall, 5))
summary(gm_abio_all_b)

# graficos
visreg(gm_abio_all_b, gg = FALSE, scale = "response", ask=FALSE)






