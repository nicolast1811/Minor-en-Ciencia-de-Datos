cartas_auto <- function(anio,mes,hoja,parametro_dt){
  #' Automatiza cartas Dirección del Trabajo.
  #' 
  #' Automatiza la creación de tablas donde se almacena la información de las cartas de despido que llegan desde la DT.
  #' 
  #' Argumentos que recibe:
  #' 
  #' anio = año del excel que ocuparemos, para el año actual hay que usar y1.
  #' 
  #' mes = mes del excel que ocuparemos (mismo parametro que mes_cartas).
  #' 
  #' hoja = hoja del excel en la que están los datos (revisar siempre el excel que llega desde la DT).
  #' 
  #' parametro_dt = mes que usaremos, se refiere al mes que usaremos de " dt = c("mes1", "mes2") "
  #' 
  #' Ejemplo:
  #' 
  #' dt = c("Dic","Ago")
  #' 
  #' Cuando parametro_dt = 1, nos referimos a "Dic"
  #' 
  #' Cuando parametro_dt = 2, nos referimos a "Ago"
  
  dates_anio <- translate_date(format(seq.Date(from = as.Date(glue::glue("{anio}-01-01")), length.out = 12, by = "month"), "%B_%Y")) #esto varía (anio)
  
  letra_mes <- c("B","C","D","E","F","G","H","I","J","K","L","M","N")
  
  nom_col <- list()
  
  mes <- ifelse(mes==12 , mes<-13, mes<-mes)
  
  mes_cartas_anio <- mes
  
  for (n in dates_anio)
  {
    ifelse(mes_cartas_anio == 13, (nom_col <- c(dates_anio[1:12], paste0("total_",anio))), (nom_col <- c(dates_anio[1: mes_cartas_anio])))
  }
  
  cartas_anio <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[parametro_dt]}{anio}'), ".xlsx"), #parametro_dt corresponde a la posición 1 0 2 de dt
                            sheet = hoja, #esto varía
                            range = paste0(glue::glue("{letra_mes[1]}"), 4,":", glue::glue("{letra_mes[mes_cartas_anio]}"), 19),
                            col_names = nom_col)
  return(cartas_anio)
}

?cartas_auto

dt <- c("Dic", "Ago")
mes_cartas <- 8

#---- Creación del dataframe + Cartas del 2021----
#Este dataframe es distinto al ser el primero, a su vez, este excel en particular es distinto a los otros
dates1 <- translate_date(format(seq.Date(from = as.Date("2021-01-01"), length.out = 12, by = "month"), "%B_%Y"))
cartas2021_test <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[1]}{y0-1}'), ".xlsx"),
                              sheet = 1,
                              range = paste0("A", 22, ":P", 37),
                              col_names = c("causal",
                                            "var",
                                            dates1[1:10],
                                            "var",
                                            dates1[11:12],
                                            paste0("total_",y0-2))) %>%
                     select(-starts_with("var"))

#----Prueba de la función----
cartas_22_funcion <- cartas_auto(anio=y0-1, mes=12, hoja=2, parametro_dt=1)
cartas_23_funcion <- cartas_auto(anio=y0, mes=12, hoja=3, parametro_dt=1)
cartas_24_funcion <- cartas_auto(anio=y1, mes=mes_cartas, hoja=3, parametro_dt=2)

#se juntan todas
cartas_funcion <- mutate(cartas2021_test,cartas_22_funcion,cartas_23_funcion,cartas_24_funcion)
cartas_funcion <- cartas_funcion %>% 
                  janitor::clean_names()

#comprobamos que sean iguales al original
all.equal(cartas,cartas_funcion)
identical(cartas,cartas_funcion)

#----Cantidad de cartas por tamaño de empresa----
dates3 <- format(seq.Date(from = as.Date("2023-01-01"), length.out = 12, by = "month"), "%B_%Y")
cartas_tamanio_head <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[2]}{y1}'), ".xlsx"),
                             sheet = 9,
                             range = paste0("A", 3, ":A", 7),
                             col_names = c("tipo_empresa"))

cartas_tamanio <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[2]}{y1}'), ".xlsx"),
                             sheet = 9,
                             range = paste0("D", 3, ":V", 7),
                             col_names =  c(dates3[1:3],
                                           "var",
                                           dates3[4],
                                           "var",
                                           dates3[5],
                                           "var",
                                           dates3[6],
                                           "var",
                                           dates3[7],
                                           "var",
                                           dates3[8:9],
                                           "var",
                                           dates3[10:12],
                                           paste0("total_",y0))) %>%
                               select(-starts_with("var"))

tamanio_empresa <- mutate(cartas_tamanio_head,cartas_tamanio)

dates3

# #----Método un poco más engorroso pero que funciona igual----
# #----Cartas del 2022----
# letra_mes <- c("B","C","D","E","F","G","H","I","J","K","L","M","N") #no se ocupa en funcion, sí en el segundo metodo
# hoja1 <- 1 #no se ocupa en funcion, sí en el segundo metodo
# dates2 <- translate_date(format(seq.Date(from = as.Date("2022-01-01"), length.out = 12, by = "month"), "%B_%Y"))
# mes_cartas_22 <- 13
# nom_col <- list()
# 
# for (n in dates2)
# {
#   ifelse(mes_cartas_22 == 13, (nom_col <- c(dates2[1:12], paste0("total_",y0-1))), (nom_col <- c(dates2[1: mes_cartas_22])))
# }
# 
# cartas2022_test <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[1]}{y0-1}'), ".xlsx"),
#                             sheet = hoja1 + 1,
#                             range = paste0(glue::glue("{letra_mes[1]}"), 4,":", glue::glue("{letra_mes[mes_cartas_22]}"), 19),
#                             col_names = nom_col)
# #----Cartas del 2023----
# dates3 <- translate_date(format(seq.Date(from = as.Date("2023-01-01"), length.out = 12, by = "month"), "%B_%Y"))
# mes_cartas_23 <- 13
# nom_col <- list()
# 
# for (n in dates3)
# {
#   ifelse(mes_cartas_23 == 13, (nom_col <- c(dates3[1:12], paste0("total_",y0))), (nom_col <- c(dates3[1: mes_cartas_23])))
# }
# 
# cartas2023_test <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[1]}{y0}'), ".xlsx"),
#                           sheet = hoja1 + 2,
#                           range = paste0(glue::glue("{letra_mes[1]}"), 4,":", glue::glue("{letra_mes[mes_cartas_23]}"), 19),
#                           col_names = nom_col)
# 
# #----Cartas del 2024----
# dates4 <- translate_date(format(seq.Date(from = as.Date("2024-01-01"), length.out = 12, by = "month"), "%B_%Y"))
# mes_cartas <- ifelse(mes_cartas==12 , mes_cartas<-13, mes_cartas<-mes_cartas)
# mes_cartas_24 <- mes_cartas
# #mes_cartas_24 <- 13 #dejar en 13 cundo terminen de llegar las cartas de 2024
# nom_col <- list()
# 
# for (n in dates4)
# {
#   ifelse(mes_cartas_24 == 13, (nom_col <- c(dates4[1:12], paste0("total_",y1))), (nom_col <- c(dates4[1: mes_cartas_24])))
# }
# 
# cartas2024_test <- read_excel(path = paste0("X:/Datos Mercado Laboral/Coyuntura/INE/Minuta Complementaria Empleo/datos/DT/Cartas_Aviso_",glue::glue('{dt[2]}{y1}'), ".xlsx"),
#                             sheet = hoja1 + 2,
#                             range = paste0(glue::glue("{letra_mes[1]}"), 4,":", glue::glue("{letra_mes[mes_cartas_24]}"), 19),
#                             col_names = nom_col)
# #----DF final----
# cartas_test <- mutate(cartas2021_test, cartas2022_test, cartas2023_test, cartas2024_test)
# cartas_test <- cartas_test %>%
#   janitor::clean_names()
# 
# all.equal(cartas,cartas_test)
# identical(cartas,cartas_test)
