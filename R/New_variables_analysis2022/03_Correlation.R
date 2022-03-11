
# ver matrices de correlacion

dfall <- list_all_data$All
dfallcor <- dfall %>% 
  select(where(is.numeric))
dfallcor 
anyNA(dfallcor)

cor(dfallcor, use ="complete.obs", method = "spearman") %>% View()

# explicar
dfallcor <- dfall %>% 
  select(where(is.numeric)) %>% 
  cor(use ="complete.obs", method = "spearman") %>% View()

# dfall <- dfall %>% select(-c(N_INVESTIGATOR, TRANSECTO))
dfall# 
str(dfall)
# vemos que son demasiadas variables por lo que es impractico graficarlas




# Reptile -----------------------------------------------------------------







# Anfibio -----------------------------------------------------------------













