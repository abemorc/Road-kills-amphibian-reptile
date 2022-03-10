
library(reshape2)


view(dfobservaciones)
dfcurva <- dfobservaciones %>% 
  select(TRANSECTO, ESPECIE)
dfcurva

melt(dfcurva)
?dcast

dfcurva1 <- dcast(data = dfcurva, formula = TRANSECTO~ESPECIE, 
                  fun.aggregate = length, value.var = "ESPECIE")
view(dfcurva1)
dfcurva1 <- as_tibble( dfcurva1[-1,])
row.names(dfcurva1)

?as.tibble
