##### Tratamiento de las bases de datos de salida, para #####
##            obtener bases de datos validadas.            ##

setwd("D:/inegi/eramo/prueba_piloto_2020/bases_de_datos/bd_eramo _pp2020_validada/agropecuario")

## Insumo: tablas csv de la BD antes de validación



##### Tabla eramo #####

eramo <- read.csv("eramo.csv")
dim(eramo)


# elimina campos innecesarios para el procesamiento:
eramo <- eramo[,-c(2,6:8,19,20,27,28,36:48,54,
                   55,64,65,69:85,91,92,94:105,107:110)]

dim(eramo)

# cambia nombres de campos:
names(eramo)[15] <- "registro_agr"
names(eramo)[16] <- "reg_agr_p1"
names(eramo)[17] <- "reg_agr_p2"
names(eramo)[18] <- "reg_agr_p3"
names(eramo)[19] <- "reg_agr_p4"
names(eramo)[20] <- "reg_agr_p5"
names(eramo)[21] <- "prod_agr_1"
names(eramo)[22] <- "prod_agr_2"
names(eramo)[23] <- "prod_agr_3"
names(eramo)[24] <- "prod_agr_4"
names(eramo)[25] <- "prod_agr_5"
names(eramo)[26] <- "prod_agr_6"
names(eramo)[27] <- "prod_agr_7"
names(eramo)[33] <- "obs_agr_1"
names(eramo)[34] <- "obs_agr_2"
names(eramo)[35] <- "registro_gan"
names(eramo)[36] <- "reg_gan_p1"
names(eramo)[37] <- "reg_gan_p2"
names(eramo)[38] <- "reg_gan_p3"
names(eramo)[39] <- "reg_gan_p4"
names(eramo)[40] <- "reg_gan_p5"
names(eramo)[41] <- "prod_gan_1"
names(eramo)[42] <- "prod_gan_2"
names(eramo)[43] <- "prod_gan_3"
names(eramo)[49] <- "obs_gan_1"
names(eramo)[50] <- "obs_ent"

names(eramo)

# Elimina registros sin información
eramo <- eramo[-which(eramo$interview__key %in% c("47-57-77-73", 
                                                  "27-62-48-05",
                                                  "51-69-57-33")),]

# Elimina registros con negativa
eramo <- eramo[-which(eramo$ok_entrev == 3),]

# Coloca datos de georreferenciación registros:

eramo$coordenada__Longitude[which(eramo$interview__key == "06-17-29-37")] <- -100-39/60
eramo$coordenada__Latitude[which(eramo$interview__key == "06-17-29-37")] <- 24+24/60

eramo$coordenada__Longitude[which(eramo$interview__key == "23-92-41-18")] <- -100-54/60
eramo$coordenada__Latitude[which(eramo$interview__key == "23-92-41-18")] <- 28+31/60





##### Tabla interview_comments_eramo: #####

comentarios <- read.csv("interview__comments_eramo.csv")

# elimina campos innecesarios para el procesamiento:
comentarios <- comentarios[,-c(2,4,5,11)]

# Nombra variables
names(comentarios) <- c("interview__key","seccion","variable","num",
                        "fecha", "hora_ini", "cve_entrevistador", "comentario")

# Modifica textos de referencia a secciones

comentarios$seccion[which(comentarios$seccion == "lista_residuos")] <- "prod_agr"
comentarios$seccion[which(comentarios$seccion == "reg_pc_perdidas")] <- "perdidas_agr"
comentarios$seccion[which(comentarios$seccion == "reg_pp_perdidas")] <- "perdidas_gan"
comentarios$seccion[which(comentarios$seccion == "lista_crias")] <- "prod_gan"




##### Tabla interview_actions_eramo: #####

acciones <- read.csv("interview__actions_eramo.csv")

# elimina campos innecesarios para el procesamiento:
acciones <- acciones[,-c(2,5,7:9)]

# Nombra variables
names(acciones) <- c("interview__key","fecha", "hora_acc", "cve_entrevistador")





##### Tabla lista_residuos: #####

prod_agr <- read.csv("lista_residuos.csv")
#prod_agr <- read.csv("D:/inegi/eramo/prueba_piloto_2020/bases_de_datos/bd_eramo _pp2020_validada/agropecuario/lista_residuos.csv")

# elimina campos innecesarios para el procesamiento:
prod_agr <- prod_agr[,-c(2,10,13,14,21,22,26,30)]

# Nombra variables

names(prod_agr) <- c("interview__key","num_prod","tipo_prod",
             "cosecha_anual", "u_cosecha_anual", 
             "otra_unid_c_anual", 
             "consum_humano", "consum_animal",
             "porc_humano", "porc_animal", 
             "mercad_nac_min", "mercad_nac_may", "mercad_extra",
             "por_nac_min", "por_nac_may", "por_extra", 
             "cosecha_estimada", 
             "u_cosecha_estimada", "ha_cosecha_estimada", 
             "cosecha_final", 
             "u_cosecha_final", "ha_cosecha_final", 
             "pro_c_perdida1","pro_c_perdida2","pro_c_perdida3",
             "pro_c_perdida4","pro_c_perdida5","pro_c_perdida6",
             "otro_pro_c_perdida")

# Elimina registros sin información
prod_agr <- prod_agr[-which(prod_agr$interview__key %in% c("89-64-98-87",
"25-83-35-42","51-32-34-93")),]


89-64-98-87
25-83-35-42






##### Tabla reg_pc_perdidas: #####

perdidas_agr <- read.csv("reg_pc_perdidas.csv")
#perdidas_agr <- read.csv("D:/inegi/eramo/prueba_piloto_2020/bases_de_datos/bd_eramo _pp2020_validada/agropecuario/reg_pc_perdidas.csv")


# elimina campos innecesarios para el procesamiento:
perdidas_agr <- perdidas_agr[,-2]


# Nombra variables

names(perdidas_agr) <- c("interview__key","num_prod",
                     "pro_c_perdida", "cant_per", "u_med_per", 
                     "ha_proceso_perd", "otra_u_med_per", 
                     "cant_mon_per", "causa", "destino")

# Elimina registros sin información
perdidas_agr <- perdidas_agr[-which(perdidas_agr$interview__key == "76-56-81-14"),]







##### Tabla lista_crias #####

prod_gan <- read.csv("lista_crias.csv")

# elimina campos innecesarios para el procesamiento:
prod_gan <- prod_gan[,-c(2,7,10:14,21,22,25,28)]

# Nombra variables

names(prod_gan) <- c("interview__key","num_prod","tipo_prod",
                     "prod_anual", "u_prod_anual", 
                     "c_humano", "c_animal", 
                     "c_mercad_nac_min", 
                     "c_mercad_nac_may", "c_mercad_extranjero",
                     "c_por_nac_min", 
                     "c_por_nac_may", "c_por_extranjero", 
                     "prod_estimada", "u_prod_estimada", 
                     "prod_final", "u_prod_final",
                     "pro_p_perdida1", "pro_p_perdida2",
                     "pro_p_perdida3", "pro_p_perdida4",
                     "pro_p_perdida5", "pro_p_perdida6",
                     "otro_pro_p_perdida")






##### Tabla reg_pp_perdidas #####

perdidas_gan <- read.csv("reg_pp_perdidas.csv")
#perdidas_gan <- read.csv("D:/inegi/eramo/prueba_piloto_2020/bases_de_datos/bd_eramo _pp2020_validada/agropecuario/reg_pp_perdidas.csv")


# elimina campos innecesarios para el procesamiento:
perdidas_gan <- perdidas_gan[,-2]

# Nombra variables

names(perdidas_gan) <- c("interview__key", "num_prod", "pro_p_perdida",
                     "cant_p_per", "u_med_p", 
                     "otra_u_med_p", "cant_mon_per_p", "causa_p", 
                     "destino_p")

# Elimina registros sin información
perdidas_gan <- perdidas_gan[-which(perdidas_gan$interview__key %in% c("06-17-29-37","09-39-85-34")
                          | (perdidas_gan$interview__key == "51-30-77-57" & 
                               perdidas_gan$num_prod %in% c(2,3))),]


                  





write.csv(eramo,"agropecuario.csv")
write.csv(acciones,"acciones_entrevistas.csv")
write.csv(comentarios,"comentarios.csv")
write.csv(prod_agr,"productos_agricolas.csv")
write.csv(perdidas_agr,"perdidas_prod_agricolas.csv")
write.csv(prod_gan,"productos_pecuarios.csv")
write.csv(perdidas_gan,"perdidas_prod_pecuarios.csv")
