

# grupos deseados
genero <- c("GENERO")

list_genero <- subgrupos(genero)
View(list_genero)

list_genero1 <- func_df_ready(list_genero)
View(list_genero1)

list_genero2 <- func_df_ready4(list_genero1)
View(list_genero2)
df_geophis <- list_genero2[["Geophis"]]
View(df_geophis)

