
# Añadir nuevas variables al conjunto de datos principal

dfdosel_pen <- read_excel(path = here("Data/Ready_data/Dosel_pendientes.xlsx"), 
                          sheet = "Hoja1",
                          col_types = c("numeric", "numeric", "text", 
                                        "numeric", "numeric"
                                        )
                          )
View(dfdosel_pen)
str(dfdosel_pen)

dfdosel_pen <- dfdosel_pen %>% 
  rename_all(toupper)
str(dfdosel_pen)


dfdosel <- dfdosel_pen %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(.cols = DOSEL_MEDIA:PENDIENTE_MEDIA,
                   .fns = list(MEAN = ~mean(x = .x, trim = 0.05, na.rm=T)),
                   .names = "{.col}"),
            TRANSECTO = max(TRANSECTO)) %>% 
  select(everything(), -c(TRANSECTO))
View(dfdosel)


##
dfcana_arrollo <- 
  read_excel(path = here("Data/Ready_data/Distanca_canadas_arroyos_2022.xlsx"), 
             sheet = "Hoja1",
             col_types = c("numeric", "numeric", "numeric", "numeric", 
                           "numeric", "numeric"
                           )
             )
View(dfcana_arrollo)
dfcana_arrollo <- dfcana_arrollo %>% 
  rename_all(toupper)
str(dfcana_arrollo)

dfcana <- dfcana_arrollo %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(.cols = DISTANCIA_CAÑADAS:DISTANCIA_ARROYOS,
                   .fns = list(MEAN = ~mean(x = .x, trim = 0.05, na.rm=T)),
                   .names = "{.col}_{fn}"
                   )
            ) %>% 
  select(everything())
View(dfcana)


############################################################
# cambios por nueva data proporcionada
# AVAN_DOSEL
AVAN_DOSEL <- 
  read_excel(path = here("Data/Ready_data/AVAN_DOSEL.xlsx"),
             sheet = "Hoja1",
             col_types = c("numeric", "numeric", "numeric", "numeric", 
                           "numeric", "text", "numeric", "numeric", "numeric")
             )
View(AVAN_DOSEL)
dfavan <- AVAN_DOSEL %>% 
  rename_all(toupper)
str(dfavan)

dfavan <- dfavan %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(.cols = c(PENDIENTE_D, PENDIENTE_I, DOSEL_MEDIA),
                   .fns = list(MEAN = ~mean(x = .x, trim = 0.05, na.rm=T)),
                   .names = "{.col}"
                   )
            ) %>% 
  select(everything())
View(dfavan)

# NDWI_GAO
NDWI_GAO <- 
  read_excel(path = here("Data/Ready_data/NDWI_GAO.xlsx"), 
             sheet = "Hoja1", 
             col_types = c("text", "numeric", "numeric", "numeric", "numeric"))
View(NDWI_GAO)

dfgao <- NDWI_GAO %>% 
  rename_all(toupper)
str(dfgao)

dfgao <- dfgao %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(.cols = c(NDWI_GAO),
                   .fns = list(MEAN = ~mean(x = .x, trim = 0.05, na.rm=T)),
                   .names = "{.col}"
                   )
            ) %>% 
  select(everything())
View(dfgao)









# probar all
# 
# View(list_main)
# list_all_especie <- func_df_ready2(list_main)
# View(list_all_especie)
# list_grupo_clase <- func_df_ready2(list_ready)
# View(list_grupo_clase)
# list_all_data <- list_grupo_clase
# list_all_data$All <- list_all_especie[[1]]
# View(list_all_data)
# 
# 
# View(list_main)
# list_all_especie <- func_df_ready2(list_main)
# View(list_all_especie)
# list_all_especie <- func_df_ready3(list_main)
# View(list_all_especie)
# list_grupo_clase <- func_df_ready2(list_ready)
# View(list_grupo_clase)
# list_grupo_clase <- func_df_ready3(list_ready)
# View(list_grupo_clase)
# 
# list_all_data <- list_grupo_clase
# list_all_data$All <- list_all_especie[[1]]
# View(list_all_data)
##

View(list_main)
list_all_especie <- func_df_ready4(list_main)
View(list_all_especie)
list_grupo_clase <- func_df_ready4(list_ready)
View(list_grupo_clase)

list_all_data <- list_grupo_clase
list_all_data$All <- list_all_especie[[1]]
View(list_all_data)




