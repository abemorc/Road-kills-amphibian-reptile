

# Correlacion entre variables ---------------------------------------------





View(list_grupos)
View(list_ready)

#obtener unicamente las columnas numericas
list_numeric <- map(list_ready, .f = ~.x %>% select(where(is.numeric)))

#crear lista de matrices de correlacion
list_cor <- map(list_numeric, 
                .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                        method = "spearman")))

View(list_cor)

# vemos que la variable numero de investigadores no esta
# relacionada con nada, por lo tanto la podemos desechar

list_index <- map(list_ready, 
                    .f = ~.x %>% select(c(where(is.numeric), -c(N_INVESTIGATOR))))

#crear lista de matrices de correlacion
list_cor <- map(list_index, 
                .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                        method = "spearman")))

View(list_cor)


# map(list_cor, .f = ~write.csv(x = .x))







# aqui esta el orden de importancia de las variables de acuerdo al la correlacion
# por el metodo de spearman
list_cor1 <- map(list_cor, 
                 .f = ~.x[order(abs(.x[,"CONTEO"]), decreasing = TRUE) ,"CONTEO", drop = FALSE])
View(list_cor1)




# xx <- list_cor$Reptilia

# graficas (hay que hacerlas mejor, con mas cuidado)
list_index <- map(list_ready, .f = ~.x %>% select(c(2,4:10))) #solo unas variables
map(list_index, .f = chart.Correlation)




map(list_index, .f = pairs.panels)




map(list_cor ,.f = ~write())




write.csv(x = list_cor$Amphibia, file = here("Data/Output_data/correlaciones_amphibia.csv"))
write.csv(x = list_cor$Reptilia, file = here("Data/Output_data/correlaciones_reptilia.csv"))
write.csv(x = list_cor$Main, file = here("Data/Output_data/correlaciones_main.csv"))



write.csv(x = list_cor1$Main, file = here("Data/Output_data/correlaciones_main_order.csv"))
write.csv(x = list_cor1$Reptilia, file = here("Data/Output_data/correlaciones_rep_order.csv"))
write.csv(x = list_cor1$Amphibia, file = here("Data/Output_data/correlaciones_amp_order.csv"))






