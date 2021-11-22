# crear data set de los recorridos realizados
dfrecorridos <- dfobservaciones %>% 
  group_by(RECORRIDO) %>% 
  summarise(MIN = max(MIN, na.rm = T),
            OBSERVACIONES = mfv1(OBSERVACIONES, na_rm = T),
            TURNO = mfv(TURNO, na_rm = T),
            AÑO = mfv1(AÑO, na_rm = T),
            MES = mfv1(MES, na_rm = T),
            SEMANA = mfv1(SEMANA, na_rm = T),
            DIA = mfv1(DIA, na_rm = T),
            MOST_TRANSECTO = mfv1(TRANSECTO, na_rm = T),
            MOST_SUBTRANSECTO = mfv1(SUBTRANSECTO, na_rm = T),
            MOST_CLASE = mfv1(CLASE, na_rm = F),
            MOST_GENERO = mfv1(GENERO, na_rm = F),
            MOST_ESPECIE = mfv1(ESPECIE, na_rm = F),
            N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
            DEG_MEAN = mean(DEG, trim = 0.05, na.rm = T),
            CHILL_MEAN = mean(CHIL, trim = 0.05, na.rm = T),
            RH_MEAN = mean(RH_C, trim = 0.05, na.rm = T),
            HI_MEAN = mean(HI, trim = 0.05, na.rm = T),
            DP_MEAN = mean(DP, trim = 0.05, na.rm = T),
            BULB_MEAN = mean(BULB, trim = 0.05, na.rm = T),
            BARO_MEAN = mean(BARO, trim = 0.05, na.rm = T),
            ALT_MEAN = mean(ALT, trim = 0.05, na.rm = T),
            SPD_MEAN = mean(SPD, trim = 0.05, na.rm = T),
            TC_MEAN = mean(TC, trim = 0.05, na.rm = T),
            TS_MEAN = mean(TS, trim = 0.05, na.rm = T),
            LHC_MEAN = mean(LHC, trim = 0.05, na.rm = T),
            L_COLA_MEAN = mean(LONG_COLA, trim = 0.05, na.rm = T),
            L_TOTAL_MEAN = mean(LONG_TOTAL, trim = 0.05, na.rm = T),
            PESO_MEAN = mean(PESO, trim = 0.05, na.rm = T),
            T_ASF_MEAN = mean(TEMP_ASF, trim = 0.05, na.rm = T),
            T_SUELO_MEAN = mean(TEMP_SUELO, trim = 0.05, na.rm = T),
            DIRECCION = mfv1(DIRECCION, na_rm = F),
            UBICACION_T = mfv1(UBICACION_T, na_rm = F),
            CONDICION = mfv1(CONDICION, na_rm = F),
            VIVO = mfv1(VIVO, na_rm = T),
            MUERTO = mfv1(MUERTO, na_rm = T),
            CARROS_TUX = mfv1(CARROS_TUX, na_rm = T),
            CARROS_OAX = mfv1(CARROS_OAX, na_rm = T),
            TOTAL_CARROS = mfv1(TOTAL_CARROS, na_rm = T),
            TEMPORADA = mfv1(TEMPORADA, na_rm = T),
            CAÑADA = mfv1(CAÑADA, na_rm = T),
            ESCURRIMIENTOS = mfv1(ESCURRIMIENTOS, na_rm = T),
            REGISTROS = sum(REGISTROS, na.rm = T)
  )
view(dfrecorridos)

# reemplazo NaN 
dfrecorridos <- dfrecorridos %>% 
  mutate(across(.fns = ~replace(., is.nan(.), NA)))
view(dfrecorridos)
view(df1)
# 
# AllOk 



dfrecorridos %>% group_by(TOTAL_CARROS) %>% 
  summarise()


p1 <- dfobservaciones %>% arrange(DIAWEEK)
view(p1)




names(mtcars)
variable <- names(mtcars)[1]
variable

mtcars %>% count(mpg) %>% 
  print()


mtcars %>% count(.data[[variable]]) %>% 
  print()

mtcars %>% count(.data$disp) %>% 
  print()




######### no funciono correctamente

funcion2 <- function(variables) {
  dfgrupos3 <- dfobservaciones %>% 
    group_split.(.data[[variables]], .keep = TRUE, .named = TRUE)
  return(dfgrupos3)
}

variable2 <- c("CLASE")

funcion2(variable2)




funcion3 <- function(variables) {
  dfgrupos4 <- dfobservaciones %>% 
    group_split.(unlist(.data[variables]), .keep = TRUE, .named = TRUE)
  return(dfgrupos4)
}

variable2 <- c("CLASE")

funcion3(variable1)



#########################







# perfecto
dfsub2 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~mean(.x, trim = 0.05, na.rm = TRUE)))

view(dfsub2)

identical(dfsub1, dfsub2)

























# creacion nuevas variables -----------------------------------------------

# Data main
## empezaremos por el tiempo
#
#recorrido nocturno 9 pm hasta terminar
# recorrido mañana de 5 am hasta terminar
mañana <- c(3:19)

#mañana <- c(4:16)
#noche <- c(17:24,0:3)

dfobservaciones <- df1 %>% 
  mutate(AÑO = year(FECHA),
         MES = month(FECHA,label = TRUE, abbr = FALSE),
         DIA = mday(FECHA),
         DIAYEAR = yday(FECHA),
         DIAWEEK = wday(FECHA, label = TRUE, abbr = FALSE),   
         SEMANA = week(FECHA),
         TURNO = ifelse(hour(HORA_1ST) %in% mañana, "mañana", "noche"),
         OBSERVACION_TF = ifelse(OBSERVACION_TF == "TRUE", TRUE, FALSE),
  ) %>% 
  select(FECHAF, AÑO, MES, SEMANA, DIA, TURNO, TRANSECTO, SUBTRANSECTO, everything())

dfobservaciones
view(dfobservaciones)
view(df1)



# Separar informacion  -----------------------------------------------------


# Separar observaciones y recorridos

# crear data set de los recorridos realizados
dfrecorridos <- dfobservaciones %>% 
  group_by(RECORRIDO) %>% 
  summarise(MIN = max(MIN, na.rm = T),
            OBSERVACIONES = mfv1(OBSERVACIONES, na_rm = T),
            TURNO = mfv(TURNO, na_rm = T),
            AÑO = mfv1(AÑO, na_rm = T),
            MES = mfv1(MES, na_rm = T),
            SEMANA = mfv1(SEMANA, na_rm = T),
            DIA = mfv1(DIA, na_rm = T),
            MOST_TRANSECTO = mfv1(TRANSECTO, na_rm = T),
            MOST_SUBTRANSECTO = mfv1(SUBTRANSECTO, na_rm = T),
            MOST_CLASE = mfv1(CLASE, na_rm = F),
            MOST_GENERO = mfv1(GENERO, na_rm = F),
            MOST_ESPECIE = mfv1(ESPECIE, na_rm = F),
            N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
            DEG_MEAN = mean(DEG, trim = 0.05, na.rm = T),
            CHILL_MEAN = mean(CHIL, trim = 0.05, na.rm = T),
            RH_MEAN = mean(RH_C, trim = 0.05, na.rm = T),
            HI_MEAN = mean(HI, trim = 0.05, na.rm = T),
            DP_MEAN = mean(DP, trim = 0.05, na.rm = T),
            BULB_MEAN = mean(BULB, trim = 0.05, na.rm = T),
            BARO_MEAN = mean(BARO, trim = 0.05, na.rm = T),
            ALT_MEAN = mean(ALT, trim = 0.05, na.rm = T),
            SPD_MEAN = mean(SPD, trim = 0.05, na.rm = T),
            TC_MEAN = mean(TC, trim = 0.05, na.rm = T),
            TS_MEAN = mean(TS, trim = 0.05, na.rm = T),
            LHC_MEAN = mean(LHC, trim = 0.05, na.rm = T),
            L_COLA_MEAN = mean(LONG_COLA, trim = 0.05, na.rm = T),
            L_TOTAL_MEAN = mean(LONG_TOTAL, trim = 0.05, na.rm = T),
            PESO_MEAN = mean(PESO, trim = 0.05, na.rm = T),
            T_ASF_MEAN = mean(TEMP_ASF, trim = 0.05, na.rm = T),
            T_SUELO_MEAN = mean(TEMP_SUELO, trim = 0.05, na.rm = T),
            DIRECCION = mfv1(DIRECCION, na_rm = F),
            UBICACION_T = mfv1(UBICACION_T, na_rm = F),
            CONDICION = mfv1(CONDICION, na_rm = F),
            VIVO = mfv1(VIVO, na_rm = T),
            MUERTO = mfv1(MUERTO, na_rm = T),
            CARROS_TUX = mfv1(CARROS_TUX, na_rm = T),
            CARROS_OAX = mfv1(CARROS_OAX, na_rm = T),
            TOTAL_CARROS = mfv1(TOTAL_CARROS, na_rm = T),
            TEMPORADA = mfv1(TEMPORADA, na_rm = T),
            CAÑADA = mfv1(CAÑADA, na_rm = T),
            ESCURRIMIENTOS = mfv1(ESCURRIMIENTOS, na_rm = T),
            REGISTROS = sum(REGISTROS, na.rm = T)
  )
view(dfrecorridos)

# reemplazo NaN 
dfrecorridos <- dfrecorridos %>% 
  mutate(across(.fns = ~replace(., is.nan(.), NA)))
view(dfrecorridos)
view(df1)
# 
# AllOk 


# Data set observaciones
index <- dfobservaciones$OBSERVACION_TF == TRUE
index
dfobservaciones <- dfobservaciones[index,]
view(dfobservaciones)

# AllOk


# Subtransectos -----------------------------------------------------------

dfsub <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~ ./ max(., na.rm = TRUE)))

view(dfsub)

dfsub1 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = mean, na.rm=T))

view(dfsub1)




# perfecto
dfsub2 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~mean(.x, trim = 0.05, na.rm = TRUE)))

view(dfsub2)

identical(dfsub1, dfsub2)




















