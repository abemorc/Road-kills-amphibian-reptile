



# GLMs per group  ---------------------------------------------------------




View(list_grupos)
View(list_ready)

xx <- list_ready$Main
xx
any(is.na(xx))


# primer glm exploratorio -------------------------------------------------



gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = xx)
gm2 <- glm(formula = CONTEO ~., family = poisson, data = xx)

alias(gm2)


summary(gm1)
gm1res <- resid(gm1)
qqnorm(gm1res)
hist(gm1res)
plot(density(gm1res))

#pruebas mas formales
AIC(gm1)
BIC(gm1)
1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2


# parece que hay varias variables muy relacionadas lo cual ocasiona NA en el
# modelo del glm



# reduccion de variables correlacionadas ----------------------------------

xxcor <- list_cor$Main
xxcor1 <- as.matrix(xxcor)
str(xxcor1)

quitar <- findCorrelation(x = xxcor1, cutoff = 0.4, names = FALSE)
quitar <- quitar[-15]
quitar
xxx <- list_ready$Main
xxx <- xxx[,-quitar]

view(xxx)
str(xxcor1)

xxx <- xxx %>% 
  select(-c(VIVO, MUERTO))


gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = xxx)
summary(gm1)












xx <- list_ready$Reptilia
cor(xx[,sapply(xx, is.numeric)])
# columnas numericas
nums <- unlist(lapply(xx, is.numeric))  
nums

xx[, nums]






