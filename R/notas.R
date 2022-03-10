# la base final tiene 3 lvls en temporada

# cambiar secas por sequias




especies <- dfobservaciones %>% 
  group_by(TRANSECTO, ESPECIE) %>% 
  summarise(conteo = n())
view(especies)



especies1 <- dfobservaciones %>% 
  group_by(ESPECIE, TRANSECTO) %>% 
  summarise(conteo = n())
view(especies1)

especies1 %>% 
  mutate(t1 = ifelse(TRANSECTO==1, conteo, 0),
         t2 = ifelse(TRANSECTO==2, conteo, 0),
         t3 = ifelse(TRANSECTO==3, conteo, 0),
         t4 = ifelse(TRANSECTO==4, conteo, 0),
         t5 = ifelse(TRANSECTO==5, conteo, 0))



especies1 %>% 
  summarise(t1 = mean(conteo))


prueba <- dfobservaciones %>% 
  select(TRANSECTO, ESPECIE) %>% 
  group_by(ESPECIE, TRANSECTO) %>% 
  mutate(n= n(),
         t1=ifelse(TRANSECTO==1, n, 0))
view(prueba)




dfobservaciones %>% 
  group_by(ESPECIE, TRANSECTO) %>% 
  summarise(conteo = n())

























tesp <- transpose(especies1)
view(tesp)



a <- especies %>% 
  group_by(TRANSECTO) %>% 
  mutate(esp1 = n())
view(a)

especies %>% 
  group_by(ESPECIE) %>% 
  


unique(dfobservaciones$ESPECIE)




library(data.table)


transpose(especies)






dfobservaciones %>% 
  select(TRANSECTO, ESPECIE) %>% 
  mutate(t1=)







especies %>% 
     group_by(TRANSECTO) %>% 
     summarise(conteo = n(),
               esp1= sum(ESPECIE=="Adelphicos sp."))
