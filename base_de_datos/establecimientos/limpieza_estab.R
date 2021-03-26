##### Tratamiento de las bases de datos de salida, para #####
##            obtener bases de datos validadas.            ##

setwd("D:/inegi/eramo/prueba_piloto_2020/bases_de_datos/bd_eramo _pp2020_validada/establecimientos")

## Insumo: tablas csv de la BD antes de validación



##### Tabla desperdicio #####

establecimientos <- read.csv("desperdicio.csv")

# concatena campos de observaciones
establecimientos$obs <- paste(establecimientos$obs_ent_c,
                              establecimientos$V70_0, sep="")


# elimina campos innecesarios para el procesamiento:
establecimientos <- establecimientos[,-c(2,6:8,42:50,56:76)]

# corrige otra modalidad
establecimientos$otra_modalidad[which(establecimientos$interview__key == "30-93-43-10")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "76-24-26-14")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "10-79-05-02")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "48-02-88-02")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "31-42-14-83")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "65-34-10-09")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "91-74-34-96")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "90-77-74-61")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "20-35-84-30")] <- "presencial, en papel"
establecimientos$otra_modalidad[which(establecimientos$interview__key == "88-17-81-08")] <- "presencial, en papel"




# Elimina registros sin información (negativa o no dieron datos de productos)
establecimientos <- establecimientos[-which(establecimientos$interview__key %in% c("04-65-89-03", 
                                                  "28-16-90-16","11-99-56-10","41-15-60-61",
                                                  "20-34-44-65","47-22-19-75","93-81-73-58",
                                                  "07-31-10-61","18-25-26-96","75-81-04-87")),]



# Coloca datos de georreferenciación registros:

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "77-01-74-26")] <- -102.8044149
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "77-01-74-26")] <- 25.3325315

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "96-31-44-15")] <- -99.1278843
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "96-31-44-15")] <- 23.7198343

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "74-51-28-39")] <- -99.1281397
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "74-51-28-39")] <- 23.7198294

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "76-26-19-30")] <- -100.5335
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "76-26-19-30")] <- 28.7069167

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "20-51-32-82")] <- -102.5734749
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "20-51-32-82")] <- 22.7746184

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "26-55-87-94")] <- -102.5475399
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "26-55-87-94")] <- 22.7602031

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "12-33-17-05")] <- -99.1707222
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "12-33-17-05")] <- 19.688667

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "02-22-73-77")] <- -99.6866389
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "02-22-73-77")] <- 19.689194

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "09-55-11-13")] <- -99.166361
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "09-55-11-13")] <- 19.6891667

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "20-27-37-35")] <- -99.16575
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "20-27-37-35")] <- 19.688972

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "97-73-87-30")] <- -99.1659444
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "97-73-87-30")] <- 19.6904167

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "53-73-04-92")] <- -99.1658889
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "53-73-04-92")] <- 19.6906667

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "47-84-82-42")] <- -99.1497096
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "47-84-82-42")] <- 19.4787908

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "31-28-80-45")] <- -99.1538724
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "31-28-80-45")] <- 19.4763635

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "44-35-84-50")] <- -99.09650
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "44-35-84-50")] <- 19.65439

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "91-51-53-80")] <- -99.09657
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "91-51-53-80")] <- 19.64841

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "49-55-81-49")] <- -99.09360
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "49-55-81-49")] <- 19.65283

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "83-07-51-12")] <- -99.11048
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "83-07-51-12")] <- 19.65508

establecimientos$coordenada__Longitude[which(establecimientos$interview__key == "25-40-36-43")] <- -99.09838
establecimientos$coordenada__Latitude[which(establecimientos$interview__key == "25-40-36-43")] <- 19.66160





##### Tabla interview_comments_desperdicios: #####

comentarios <- read.csv("interview__comments_desperdicios.csv")

# elimina campos innecesarios para el procesamiento:
comentarios <- comentarios[,-c(2,4,10)]

# Nombra variables
names(comentarios) <- c("interview__key","seccion","variable","num",
                        "fecha", "hora_ini", "cve_entrevistador", "comentario")




##### Tabla interview_actions_desperdicios: #####

acciones <- read.csv("interview__actions_desperdicios.csv")

# elimina campos innecesarios para el procesamiento:
acciones <- acciones[,-c(2,5,7:9)]

# Nombra variables
names(acciones) <- c("interview__key","fecha", "hora_acc", "cve_entrevistador")





##### Tabla lista_residuos: #####

prod_estab <- read.csv("lista_productos.csv")


# elimina campos innecesarios para el procesamiento:
prod_estab <- prod_estab[,-c(2,9)]


# asigna dato recuperado de pérdida
prod_estab$perdida_prod[which(establecimientos$interview__key == "20-71-86-36")] <- 2

# elimina registros sin datos numericos de manejo y de pérdidas
prod_estab <- prod_estab[-which(prod_estab$interview__key %in% c("02-85-64-84",
                                                                 "54-20-67-43",
                                                                 "01-43-03-64",
                                                                 "56-93-50-65",
                                                                 "09-43-30-81",
                                                                 "16-17-40-85",
                                                                 "26-55-87-94",
                                                                 "10-79-05-02",
                                                                 "97-40-86-36",
                                                                 "74-62-60-95",
                                                                 "88-17-81-08",
                                                                 "90-77-74-61",
                                                                 "18-58-26-99",
                                                                 "20-51-32-82",
                                                                 "18-50-52-79")),]


# Renombra variables

names(prod_estab)[2] <- "num_prod"
names(prod_estab)[10] <- "din_perd"
names(prod_estab)[12] <- "comestible"
names(prod_estab)[13] <- "u_comestible"
names(prod_estab)[14] <- "otra_u_comestible"




write.csv(establecimientos,"establecimientos.csv")
write.csv(acciones,"acciones_entrevistas.csv")
write.csv(comentarios,"comentarios.csv")
write.csv(prod_estab,"productos_establecimientos.csv")

