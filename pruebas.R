
# Cosecha
prod_agri <- read.csv("base_de_datos/agropecuario/productos_agricolas.csv")

prods_princ_med <- which((prod_agri$tipo_prod == "Maíz" | prod_agri$tipo_prod == "Jitomate" |
                            prod_agri$tipo_prod == "Mango manila" | prod_agri$tipo_prod == "Frijol" |
                            prod_agri$tipo_prod == "Melón") &
                           prod_agri$u_cosecha_anual %in% c(1,2)) 

Cultivos <- as.factor(prod_agri$tipo_prod[prods_princ_med])
cose <- prod_agri$cosecha_anual[prods_princ_med]
unidad <- prod_agri$u_cosecha_anual[prods_princ_med]
Cosecha <- ifelse(unidad == 1, cosecha/1000, cose)

datos1 <- data.frame(Cultivos,Cosecha)


library(plotly)

fig1 <- plot_ly(data = datos1, x = ~Cultivos, y = ~Cosecha, type = "box")
fig1 %>% layout(title = "Toneladas cosechadas por tipo de cultivo")





prods_princ_med <- which((prod_agri$tipo_prod == "Maíz" | prod_agri$tipo_prod == "Jitomate" |
                            prod_agri$tipo_prod == "Mango manila" | prod_agri$tipo_prod == "Frijol" |
                            prod_agri$tipo_prod == "Melón") &
                           prod_agri$u_cosecha_anual %in% c(1,2)) 

Cultivos <- prod_agri$tipo_prod[prods_princ_med]
cEstimada <- prod_agri$cosecha_estimada[prods_princ_med]
cFinal <- prod_agri$cosecha_final[prods_princ_med]
u_cEstimada <- prod_agri$u_cosecha_estimada[prods_princ_med]
u_cFinal <- prod_agri$u_cosecha_final[prods_princ_med]
Estimada <- ifelse(u_cEstimada == 1, cEstimada/1000, cEstimada)
Final <- ifelse(u_cFinal == 1, cFinal/1000, cFinal)

datos1a <- data.frame(Cultivos, Estimada)
datos1b <- data.frame(Cultivos, Final)



fig1a <- plot_ly(data = datos1a, x = ~Cultivos, y = ~Estimada,
                 type = "box") 

fig1b <- plot_ly(data = datos1b, x = ~Cultivos, y = ~Final,
                 type = "box") 

subplot(list(fig1a, fig1b), shareY = T, shareX = F, titleX = F, titleY = F) %>% 
  layout(showlegend = F) %>% layout(title = "Toneladas por tipo de cultivo
                   Azul = Estimadas, Naranja = Finales")






vegs <- c("Melon", 
          "Melon", 
          "Melon", 
          "Melon", 
          "Melon", 
          "Melon", 
          "Pepino", 
          "Pepino", 
          "Melon", 
          "Melon", 
          "Melon", 
          "Melon", 
          "Frijol", 
          "Garbanzo", 
          "Maiz", 
          "Sorgo", 
          "Esparrago", 
          "Lechuga", 
          "Brocoli", 
          "Apio", 
          "Coliflor", 
          "Maiz", 
          "Garbanzo", 
          "Papaya", 
          "Tomate", 
          "Elote", 
          "Maiz", 
          "Caña de azúcar", 
          "Caña de azúcar", 
          "Calabaza pipian", 
          "Papaya", 
          "Papaya", 
          "Papaya", 
          "Chile habanero", 
          "Sandia", 
          "Nuez", 
          "Tomate variedad maviri", 
          "Tomate variedad maviri", 
          "Tomate variedad maviri", 
          "Tomate variedad maviri", 
          "Jitomate", 
          "Jitomate", 
          "Jitomate", 
          "Trigo", 
          "Datil", 
          "Datil", 
          "Maiz", 
          "Frijol", 
          "Maiz", 
          "Cebada", 
          "Avena", 
          "Maiz", 
          "Frijol", 
          "Maiz", 
          "Frijol", 
          "Maiz", 
          "Frijol", 
          "Maiz", 
          "Frijol", 
          "Maiz", 
          "Maiz", 
          "Jitomate", 
          "Rambutan", 
          "Jitomate", 
          "Jitomate", 
          "Jitomate", 
          "Maiz", 
          "Jitomate", 
          "Maiz", 
          "Frijol", 
          "Platano", 
          "Papaya", 
          "Papaya", 
          "Mango Tommy Atkins", 
          "Naranja malta", 
          "Naranja malta", 
          "Mango manila", 
          "Mango manila", 
          "Mango manila", 
          "Mango manila", 
          "Mango manila", 
          "Mango manila", 
          "Chayote", 
          "Limon", 
          "Limon", 
          "Limon", "persa", 
"Ciruelo", 
"Ciruelo", 
"Mango manila", 
"Chile serrano", 
"Chile en conserva", 
"Frijol negro", 
"Pepino", 
"Calabacita bola", 
"Mango manila", 
"Jitomate", 
"Brocoli", 
"Limon" )

sort(table(vegs),decreasing = T)

Maiz                  Melon               Jitomate           Mango manila 
13                     10                      9                      8 
Frijol                 Papaya                  Limon Tomate variedad maviri 
7                      6                      4                      4 
Pepino                Brocoli




prod_perd <- read.csv("base_de_datos/agropecuario/perdidas_prod_agricolas.csv")

prods_princ_perd <- which((prod_perd$tipo_prod == "Maiz" | prod_perd$tipo_prod == "Melon" |
                             prod_perd$tipo_prod == "Jitomate" | prod_perd$tipo_prod == "Mango manila" |
                             prod_perd$tipo_prod == "Frijol" | prod_perd$tipo_prod == "Papaya" |
                             prod_perd$tipo_prod == "Tomate variedad maviri" | 
                             prod_perd$tipo_prod == "Pepino" |
                             prod_perd$tipo_prod == "Brocoli") &
                            prod_perd$u_med_per %in% c(1,2))

Cultivos <- as.factor(prod_perd$tipo_prod[prods_princ_perd])
perd <- prod_perd$cant_per[prods_princ_perd]
unidadp <- prod_perd$u_med_per[prods_princ_perd]
Perdida <- ifelse(unidadp == 1, perd/1000, perd)

datos4 <- data.frame(Cultivos,Perdida)

fig3 <- plot_ly(data = datos4, x = ~Cultivos, y = ~Perdida, type = "box")
fig3 %>% layout(title = "Toneladas perdidas por tipo de cultivo")







prods_princ_perd <- which(prod_perd$u_med_per %in% c(1:3) & 
                            !(prod_perd$tipo_prod %in% c("Papaya","Mango manila",
                                                         "Trigo")))

Cultivos <- as.factor(prod_perd$tipo_prod[prods_princ_perd])
perd <- prod_perd$cant_per[prods_princ_perd]
perd2 <- prod_perd$cant_per[prods_princ_perd]*prod_perd$ha_proceso_perd[prods_princ_perd]
unidadp <- prod_perd$u_med_per[prods_princ_perd]
Perdida <- ifelse(unidadp == 1, perd/1000, ifelse(unidadp == 2, perd, perd2))

datos4 <- data.frame(Cultivos,Perdida)

fig4 <- plot_ly(data = datos4, x = ~Cultivos, y = ~Perdida, type = "box")
fig4 %>% layout(title = "Toneladas perdidas por tipo de cultivo")





Cultivos <- as.factor(prod_perd$tipo_prod)
Pesos <- prod_perd$cant_mon_per
datos5 <- data.frame(Cultivos,Pesos) 

fig5 <- plot_ly(data = datos5, x = ~Cultivos, y = ~Pesos, type = "box")
fig5 %>% layout(title = "Valor de las pérdidas por tipo de cultivo")



sum(prod_perd$cant_mon_per)





prods_princ_perd <- which(prod_perd$u_med_per %in% c(1:3) & 
                            prod_perd$u_cosecha_anual %in% c(1,2))

Cultivos <- as.factor(prod_perd$tipo_prod[prods_princ_perd])

perd <- prod_perd$cant_per[prods_princ_perd]
perd2 <- prod_perd$cant_per[prods_princ_perd]*prod_perd$ha_proceso_perd[prods_princ_perd]
unidadp <- prod_perd$u_med_per[prods_princ_perd]
Perdida <- ifelse(unidadp == 1, perd/1000, ifelse(unidadp == 2, perd, perd2))

cant_cos <- prod_perd$cosecha_anual[prods_princ_perd]
unidadc <- prod_perd$u_cosecha_anual[prods_princ_perd]
cos_anual <- ifelse(unidadc == 1, cant_cos/1000, cant_cos)

Porcentaje <- round(Perdida/cos_anual*100,0)
Porcentaje <- Porcentaje[-c(45,47,48,55,56)]
Cultivos <- Cultivos[-c(45,47,48,55,56)]

datos6 <- data.frame(Cultivos,Porcentaje)

fig4 <- plot_ly(data = datos6, 
                x = ~Cultivos, 
                y = ~Porcentaje, type = "box")
fig4 %>% layout(title = "Porcentaje de pérdidas por tipo de cultivo")






filePath <- "archivos/causas_perd.txt"
text <- readLines(filePath,encoding="UTF-8")     #Leemos el archivo
text = iconv(text, to="ASCII//TRANSLIT") # Convert Character Vector between Encodings
corpus <- Corpus(VectorSource(text)) # formato de texto
# llevamos a minúsculas
d  <- tm_map(corpus, tolower)
# quitamos espacios en blanco
d  <- tm_map(d, stripWhitespace)
# quitamos la puntuación
d <- tm_map(d, removePunctuation)
# quitamos los números
d <- tm_map(d, removeNumbers)

#Un comando útil es quitar la palabras genéricas, la paquetería “tm” tiene un grupo de palabras. Para verlas:
# stopwords("spanish")
# Importante: “stopwords(”spanish“)” es un vector, se puede ampliar y actualizar de forma personalizada.

# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("spanish"))


barplot(frec3[1:6], ylim=c(0,8),
        main = "Principales productos declarados, PP-ERAMO 2021",
        xlab = "",
        ylab = "Frec",
        names.arg = c("Ganado", "Borregos", "Tilapia"),
        border="red",
        col="blue",
        density=15)




filePath <- "archivos/prods_gan.txt"
text3 <- readLines(filePath,encoding="UTF-8") 
frec3 <- sort(table(text3), decreasing = T)
  







### Paquetes necesarios:
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Se lee el archivo txt:
filePath <- "archivos/prods_estab.txt"
text <- readLines(filePath,encoding="UTF-8")     #Leemos el archivo
text = iconv(text, to="ASCII//TRANSLIT") # Convert Character Vector between Encodings
corpus <- Corpus(VectorSource(text)) # formato de texto
# llevamos a minúsculas
d  <- tm_map(corpus, tolower)
# quitamos espacios en blanco
d  <- tm_map(d, stripWhitespace)
# quitamos la puntuación
d <- tm_map(d, removePunctuation)
# quitamos los números
d <- tm_map(d, removeNumbers)

#Un comando útil es quitar la palabras genéricas, la paquetería “tm” tiene un grupo de palabras. Para verlas:
# stopwords("spanish")
# Importante: “stopwords(”spanish“)” es un vector, se puede ampliar y actualizar de forma personalizada.

# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("spanish"))


##### Conteo: 

# Una vez “limpio” el texto, tenemos que pasarlo a un formato de base de datos.
tdm <- TermDocumentMatrix(d)

# Encontramos sus asociaciones;
frecuentes<-findFreqTerms(tdm, lowfreq=20) # el parámetro lowfreq fija el umbral de frecuencia deseado
# findAssocs(tdm, frecuentes, 0.45)  # se determinan las asociaciones


##### Sumarización:

m <- as.matrix(tdm)                            # lo vuelve una matriz
v <- sort(rowSums(m),decreasing=TRUE)          # lo ordena y suma
df <- data.frame(word = names(v),freq=v)       # lo nombra y hace data.frame

# Y se hace la nube de palabras: 

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))









prod_estab <- read.csv("archivos/productos_estab.csv")

prods_princ <- which((prod_estab$Productos_genericos == "Quesos" | 
                        prod_estab$tipo_prod == "Pollo" |
                        prod_estab$tipo_prod == "Comida preparada" | 
                        prod_estab$tipo_prod == "Carnes frías y embutidos" |
                        prod_estab$tipo_prod == "Carne de res" | 
                        prod_estab$tipo_prod == "Jitomate" |
                        prod_estab$tipo_prod == "Tomate" | 
                        prod_estab$tipo_prod == "Cebolla" |
                        prod_estab$tipo_prod == "Carne de cerdo" | 
                        prod_estab$tipo_prod == "Papa" | 
                        prod_estab$tipo_prod == "Tortillas" |
                        prod_estab$tipo_prod == "Huevo" | 
                        prod_estab$tipo_prod == "Limón" | 
                        prod_estab$tipo_prod == "Lechuga" |
                        prod_estab$tipo_prod == "Aguacate" | 
                        prod_estab$tipo_prod == "Naranja" |
                        prod_estab$tipo_prod == "Pescados y mariscos" |
                        prod_estab$tipo_prod == "Verduras" |
                        prod_estab$tipo_prod == "Zanahoria" |
                        prod_estab$tipo_prod == "Plátano") &
                       prod_estab$u_prom_men %in% c(1,2)) 



Producto <- as.factor(prod_estab$Productos_genericos[prods_princ])
cant <- prod_estab$prom_men[prods_princ]
unidad <- prod_estab$u_prom_men[prods_princ]
kg <- ifelse(unidad == 1, cant, cant*1000)

datos1 <- data.frame(Producto,kg)


library(plotly)

fig1 <- plot_ly(data = datos1, x = ~Producto, y = ~kg, type = "box")
fig1 %>% layout(title = "Cantidad de producto vendido al mes")







## Porcentajes


prods_princ <- which((prod_estab$Productos_genericos == "Quesos" | 
                        prod_estab$Productos_generico == "Pollo" |
                        prod_estab$Productos_generico == "Comida preparada" | 
                        prod_estab$Productos_generico == "Carnes frías y embutidos" |
                        prod_estab$Productos_generico == "Carne de res" | 
                        prod_estab$Productos_generico == "Jitomate" |
                        prod_estab$Productos_generico == "Tomate" | 
                        prod_estab$Productos_generico == "Cebolla" |
                        prod_estab$Productos_generico == "Carne de cerdo" | 
                        prod_estab$Productos_generico == "Papa" | 
                        prod_estab$Productos_generico == "Tortillas" |
                        prod_estab$Productos_generico == "Huevo" | 
                        prod_estab$Productos_generico == "Limón" | 
                        prod_estab$Productos_generico == "Lechuga" |
                        prod_estab$Productos_generico == "Aguacate" | 
                        prod_estab$Productos_generico == "Naranja" |
                        prod_estab$Productos_generico == "Pescados y mariscos" |
                        prod_estab$Productos_generico == "Verduras" |
                        prod_estab$Productos_generico == "Zanahoria" |
                        prod_estab$Productos_generico == "Plátano") &
                       prod_estab$u_perdida_prod %in% c(1,2) &
                       prod_estab$u_prom_men %in% c(1,2)) 

Producto <- as.factor(prod_estab$Productos_genericos[prods_princ])
cant1 <- prod_estab$perdida_prod[prods_princ]
cant2 <- prod_estab$prom_men[prods_princ]
unidad1 <- prod_estab$u_perdida_prod[prods_princ]
unidad2 <- prod_estab$u_prom_men[prods_princ]
kg1 <- ifelse(unidad1 == 1, cant1, cant1*1000)
kg2 <- ifelse(unidad2 == 1, cant2, cant2*1000)
porcent_kg <- kg1/kg2*100

datos1 <- data.frame(Producto,porcent_kg)


library(plotly)

fig1 <- plot_ly(data = datos1, x = ~Producto, y = ~porcent_kg, type = "box")
fig1 %>% layout(title = "Cantidad de producto perdido al mes (kg)")



sort(table(prod_estab$Productos_genericos), decreasing = T)[1:25]
as.numeric(prod_estab$din_perd[prods_princ])


sum(as.numeric(prod_estab$din_perd[prod_estab$din_perd > 0]))






prods_princ <- which((prod_estab$Productos_genericos == "Quesos" | 
                        prod_estab$Productos_generico == "Pollo" |
                        prod_estab$Productos_generico == "Comida preparada" | 
                        prod_estab$Productos_generico == "Carnes frías y embutidos" |
                        prod_estab$Productos_generico == "Carne de res" | 
                        prod_estab$Productos_generico == "Jitomate" |
                        prod_estab$Productos_generico == "Tomate" | 
                        prod_estab$Productos_generico == "Cebolla" |
                        prod_estab$Productos_generico == "Carne de cerdo" | 
                        prod_estab$Productos_generico == "Papa" | 
                        prod_estab$Productos_generico == "Tortillas" |
                        prod_estab$Productos_generico == "Huevo" | 
                        prod_estab$Productos_generico == "Limón" | 
                        prod_estab$Productos_generico == "Lechuga" |
                        prod_estab$Productos_generico == "Aguacate" | 
                        prod_estab$Productos_generico == "Naranja" |
                        prod_estab$Productos_generico == "Pescados y mariscos") &
                       prod_estab$u_comestible %in% c(1,2) &
                       prod_estab$u_perdida_prod %in% c(1,2)) 

Producto <- as.factor(prod_estab$Productos_genericos[prods_princ])
cant1 <- as.numeric(prod_estab$comestible[prods_princ])
cant2 <- prod_estab$perdida_prod[prods_princ]
unidad1 <- prod_estab$u_comestible[prods_princ]
unidad2 <- prod_estab$u_perdida_prod[prods_princ]
kg1 <- ifelse(unidad1 == 1, cant1, cant1*1000)
kg2 <- ifelse(unidad2 == 1, cant2, cant2*1000)
Porcentaje <- kg1/kg2*100


library(plotly)

fig1 <- plot_ly(data = datos1, x = ~Producto, y = ~Porcentaje, type = "box")
fig1 %>% layout(title = "Prcentaje de producto perdido que aún era comestible")












