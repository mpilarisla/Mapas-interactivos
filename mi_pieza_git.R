
library(readr)
library(leaflet)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(dplyr)
library(geojson)
library(ggmap)
library(magrittr)
library(maps)
library(maptools)
library(rgeos)
library(sp)
library(ggplot2)
library(ggrepel)
library(geojsonio)
library(plotly)
library(leaflet.minicharts)
library(htmlwidgets)
library(leaflet.providers)


setwd("C:/Users/Pilar/Desktop/Proyectos/Mi Pieza Enac/Mi_pieza_2023")


shape<-st_read("20220524_info_publica.shp")
mipiezacsv<- read_csv("mi-pieza-2022-12-29.csv")

colnames(shape)[2]<-"id_barrio"
colnames(mipiezacsv)[4]<-"id_barrio"
colnames(shape)[3]<-"Barrio"
beneficiarias<-subset(mipiezacsv, estado=="seleccionada")
anotadas<-subset(mipiezacsv, estado=="inscripta")
frecuencia<-data.frame(table(beneficiarias$id_barrio))
names(frecuencia)[names(frecuencia) == 'Var1'] <- 'id_barrio'
frecuencia$id_barrio <- as.numeric(as.character(frecuencia$id_barrio))
shape<-full_join(shape,frecuencia,by="id_barrio")
anotadas_barrio<-data.frame(table(anotadas$id_barrio))
names(anotadas_barrio)[names(anotadas_barrio) == 'Var1'] <- 'id_barrio'
names(anotadas_barrio)[names(anotadas_barrio) == 'Freq'] <- 'anotadas.barrio'
anotadas_barrio$id_barrio<- as.numeric(as.character(anotadas_barrio$id_barrio))
shape<-full_join(shape, anotadas_barrio, by="id_barrio")
names(shape)[names(shape) == 'Freq'] <- 'cant.mipieza.total'

##por sorteo##
puntosrenabap<- read_csv("puntosrenabap.csv")
names(puntosrenabap)[names(puntosrenabap) == 'renabap_id'] <- 'id_barrio'
beneficiarias1<-subset(beneficiarias, sorteo=="1")
frecuenciaS1<-data.frame(table(beneficiarias1$id_barrio))
names(frecuenciaS1)[names(frecuenciaS1) == 'Var1'] <- 'id_barrio'
frecuenciaS1$id_barrio <- as.numeric(as.character(frecuenciaS1$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS1,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo1'
puntosrenabap1<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo1")]
beneficiarias2<-subset(beneficiarias, sorteo=="2")
frecuenciaS2<-data.frame(table(beneficiarias2$id_barrio))
names(frecuenciaS2)[names(frecuenciaS2) == 'Var1'] <- 'id_barrio'
frecuenciaS2$id_barrio <- as.numeric(as.character(frecuenciaS2$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS2,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo2'
puntosrenabap2<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo2")]
beneficiarias3<-subset(beneficiarias, sorteo=="3")
frecuenciaS3<-data.frame(table(beneficiarias3$id_barrio))
names(frecuenciaS3)[names(frecuenciaS3) == 'Var1'] <- 'id_barrio'
frecuenciaS3$id_barrio <- as.numeric(as.character(frecuenciaS3$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS3,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo3'
puntosrenabap3<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo3")]
beneficiarias4<-subset(beneficiarias, sorteo=="4")
frecuenciaS4<-data.frame(table(beneficiarias4$id_barrio))
names(frecuenciaS4)[names(frecuenciaS4) == 'Var1'] <- 'id_barrio'
frecuenciaS4$id_barrio <- as.numeric(as.character(frecuenciaS4$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS4,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo4'
puntosrenabap4<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo4")]
beneficiarias5<-subset(beneficiarias, sorteo=="5")
frecuenciaS5<-data.frame(table(beneficiarias5$id_barrio))
names(frecuenciaS5)[names(frecuenciaS5) == 'Var1'] <- 'id_barrio'
frecuenciaS5$id_barrio <- as.numeric(as.character(frecuenciaS5$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS5,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo5'
puntosrenabap5<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo5")]
beneficiarias6<-subset(beneficiarias, sorteo=="6")
frecuenciaS6<-data.frame(table(beneficiarias6$id_barrio))
names(frecuenciaS6)[names(frecuenciaS6) == 'Var1'] <- 'id_barrio'
frecuenciaS6$id_barrio <- as.numeric(as.character(frecuenciaS6$id_barrio))
puntosrenabap<-full_join(puntosrenabap,frecuenciaS6,by="id_barrio")
names(puntosrenabap)[names(puntosrenabap) == 'Freq'] <- 'Sorteo6'
puntosrenabap6<- puntosrenabap[,c("id_barrio","nombre", "x", "y", "Sorteo6")]

#pot
proyectos_sisu<- read_csv("proyectos_fisu-2023-09-20.csv")
proyectos_sisu<- proyectos_sisu[,c("id", "renabap_id","barrio", "tipo_unidad_ejecutora", "tipo_proyecto", "nombre_proyecto", "monto_total")]
datosesppot<- shape[,c("id_barrio","geometry")]
proyectos_sisu$renabap_id <- as.numeric(as.character(proyectos_sisu$renabap_id), rm.na= TRUE)
names(datosesppot)[names(datosesppot) == 'id_barrio'] <- 'renabap_id'
shapepot<- full_join(proyectos_sisu, datosesppot, by="renabap_id")
shapepot<-na.omit(shapepot)


shapepot$tipo_unidad_ejecutora[shapepot$tipo_unidad_ejecutora == 'OrganizaciÃ³n'] <- 'Organización'

mapashape<-st_read("20220524_info_publica.shp")
shape_spat<-mapashape %>% as_Spatial()


rangos<-c(1,20,40,80,100,200,500,1000,Inf)
pal<- colorBin(palette = "YlOrRd", domain = shape$cant.mipieza.total, bins = rangos)
pal2<- colorFactor(palette = "Blues",
                   levels = c("Municipio", "Coop. de servicios", "Provincia", "Organización", "SISU"))
shapepot <- sf::st_as_sf(shapepot)

leaflet(shape_spat) %>%
  addSearchOSM() %>%
  addProviderTiles(
    "CartoDB.DarkMatter",
    group = "CartoDB.DarkMatter"
  ) %>%
  addProviderTiles(
    "OpenStreetMap",
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  addTiles() %>%  
  addPolygons(data = shape,
              fill = FALSE,
              weight= 1,
              opacity = 1,
              color = "Black",
              dashArray = "3") %>%
  addCircleMarkers(data = puntosrenabap1,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = 'yellow',
                   label= ~nombre,
                   popup =~paste(Sorteo1),
                   group= "Sorteo 1") %>%
  addCircleMarkers(data = puntosrenabap2,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = 'orange',
                   label= ~nombre,
                   popup =~paste(Sorteo2),
                   group= "Sorteo 2") %>%
  addCircleMarkers(data = puntosrenabap3,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = 'red',
                   label= ~nombre,
                   popup =~paste(Sorteo3),
                   group= "Sorteo 3") %>%
  addCircleMarkers(data = puntosrenabap4,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = 'purple',
                   label= ~nombre,
                   popup =~paste(Sorteo4),
                   group= "Sorteo 4") %>%
  addCircleMarkers(data = puntosrenabap5,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = 'blue',
                   label= ~nombre,
                   popup =~paste(Sorteo5),
                   group= "Sorteo 5") %>%
  addCircleMarkers(data = puntosrenabap6,
                   lng = ~x,
                   lat = ~y,
                   radius = 4,
                   color = '#27408B',
                   label= ~nombre,
                   popup =~paste(Sorteo6),
                   group= "Sorteo 6") %>%
  addPolygons(data = shape,fillColor = ~pal(shape$cant.mipieza.total),
              fillOpacity = 0.7,
              weight= 1,
              opacity = 1,
              color = "Black",
              dashArray = "3",
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label= ~Barrio,
              popup = ~paste(Barrio,
                             "<br>Familias estimadas:", cantidad_f,
                             "<br>Cantidad de seleccionadas:", cant.mipieza.total,
                             "<br>Inscriptas no seleccionadas:", anotadas.barrio),
              group = "Mi Pieza total" )%>%
  addPolygons(
    data = shapepot,
    fillColor = ~pal2(shapepot$tipo_unidad_ejecutora),
    fillOpacity = 0.7,
    weight = 1,
    opacity = 1,
    color = "Black",
    dashArray = "3",
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~tipo_proyecto,
    popup = ~paste(
      tipo_proyecto,
      "<br>Nombre del barrio:", barrio,
      "<br>Nombre del proyecto:", paste(nombre_proyecto),
      "<br>Tipo de unidad ejecutora:", paste(tipo_unidad_ejecutora),
      "<br>Monto total del proyecto:", ifelse(
        !is.na(monto_total),
        paste("$", gsub(" ", "", format(monto_total, big.mark = ".", decimal.mark = ","))),
        "No disponible"
      )
    ),
    group = "Proyectos SISU"
  ) %>%
  addLegend(pal = pal, values = ~rangos, opacity = 0.7, position = "bottomright", group = "Mi Pieza total")%>%
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Esri.WorldImagery", "CartoDB.DarkMatter"
    ),
    overlayGroups = c("Sorteo 1",
                      "Sorteo 2",
                      "Sorteo 3",
                      "Sorteo 4",
                      "Sorteo 5",
                      "Sorteo 6",
                      "Mi Pieza total",
                      "Proyectos SISU")) %>%
  hideGroup("Proyectos SISU")  %>%
  hideGroup("Sorteo 1") %>%
  hideGroup("Sorteo 2") %>%
  hideGroup("Sorteo 3") %>%
  hideGroup("Sorteo 4") %>%
  hideGroup("Sorteo 5") %>%
  hideGroup("Sorteo 6")  %>%
addTiles(attribution = 'Arq. María del Pilar Isla - Año: 2023 - Fuente: Elaboración propia en base a SISU (2022)')