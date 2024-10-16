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
  
  dates_anio <- format(seq.Date(from = as.Date(glue::glue("{anio}-01-01")), length.out = 12, by = "month"), "%B_%Y") #esto varía (anio)
  
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
