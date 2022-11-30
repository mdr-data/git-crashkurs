library(ndjson)
library(tidyverse)

# shapefiles
library(sp)
library(sf)
library(rgdal)
library(rgeos)

ac_data <- stream_in('~/Desktop/MDR Themen/Barrierefreiheit/barrierefreiheit/PlaceInfosWithOSM.ndjson', cls = c("dt", "tbl"))
# colnames(ac_data)

filtered_data <- ac_data %>% select(geometry.coordinates.0, geometry.coordinates.1, 
                          properties.category, 
                          properties.accessibility.accessibleWith.wheelchair)
data <- filtered_data %>% rename(long = geometry.coordinates.0,
                         lat = geometry.coordinates.1,
                         category = properties.category,
                         accessibility.wheelchair = properties.accessibility.accessibleWith.wheelchair)

# Rechteck um ST filtern
# Norden: lat 53.079059, Süden: lat 50.897711, Westen: long 10.462170, Osten: long 13.252697
rechteck_data <- data %>% filter(long < 13.252697 & long > 10.462170 & lat < 53.079059 & lat > 50.897711)

rechteck_data <- rechteck_data %>% drop_na(accessibility.wheelchair)

# shapefile einlesen
st_map <- readOGR('dvg_sachsen_anhalt/DVG_Sachsen-Anhalt_Shape_ETRS89_UTM32_2022-06-30/Bundesland.shp')

# SpatailPoints für Orte
coords <- data.frame(long = rechteck_data$long, lat = rechteck_data$lat)
prj <- CRS('+proj=longlat')
rechteck_sp <- SpatialPoints(coords, proj4string = prj)

# Karte auf gleiches Koordinatensystem setzen
st_map <- spTransform(st_map, prj)

# Wo überschneiden sich die Daten?
over <- over(rechteck_sp, st_map)

# https://stackoverflow.com/questions/24174042/matching-georeferenced-data-with-shape-file-in-r
data_st <- cbind.data.frame(rechteck_data, bundesland=over(rechteck_sp, st_map))

bundesland=over(rechteck_sp, st_map)

# Kategorien
ac_kategorien <- stream_in('~/Desktop/MDR Themen/Barrierefreiheit/barrierefreiheit/Categories.ndjson', cls = c("dt", "tbl"))

final <- merge(x = data_st, y = ac_kategorien, by.x = 'category', by.y = '_id', all.x = TRUE) %>%
  transmute(long = long, 
            lat = lat,
            bundesland = bundesland.GN_KLAR,
            kategorie = translations._id.de,
            accessibility.wheelchair = accessibility.wheelchair) %>%
  na.omit()

# speichern
write.csv(final, "~/Desktop/MDR Themen/Barrierefreiheit/barrierefreiheit/ST_wheelchair.csv")



# Kreise
kreise_map <- readOGR('dvg_sachsen_anhalt/DVG_Sachsen-Anhalt_Shape_ETRS89_UTM32_2022-06-30/Landkreise_und_kreisfreie_Staedte.shp')

# Karte auf gleiches Koordinatensystem setzen
kreise_map <- spTransform(kreise_map, prj)

# Wo überschneiden sich die Daten?
over_kreise <- over(rechteck_sp, kreise_map)

data_kreise <- cbind.data.frame(rechteck_data, kreise=over(rechteck_sp, kreise_map)) %>%
  transmute(long = long, 
            lat = lat,
            kreis = kreise.GN_KLAR,
            VSN = kreise.VSN_KLAR,
            kategorie = category,
            accessibility.wheelchair = accessibility.wheelchair) %>%
  na.omit()

kreise_final <- merge(x = data_kreise, y = ac_kategorien, by.x = 'kategorie', by.y = '_id', all.x = TRUE) %>%
  transmute(long = long, 
            lat = lat,
            kreis = kreis,
            VSN = VSN,
            kategorie = translations._id.de,
            accessibility.wheelchair = accessibility.wheelchair)

write_csv(kreise_final, "~/Desktop/MDR Themen/Barrierefreiheit/barrierefreiheit/kreise_wheelchair.csv")




# zusätzliche Variablen

filtered_data2 <- ac_data %>% select(geometry.coordinates.0, geometry.coordinates.1, 
                                    properties.category, 
                                    properties.accessibility.accessibleWith.wheelchair,
                                    properties.accessibility.offersActivitiesFor.hearingImpaired,
                                    properties.accessibility.offersActivitiesFor.visuallyImpaired)

data2 <- filtered_data2 %>% rename(long = geometry.coordinates.0,
                                 lat = geometry.coordinates.1,
                                 category = properties.category,
                                 accessibility.wheelchair = properties.accessibility.accessibleWith.wheelchair,
                                 offersActivitiesFor.hearingImpaired = properties.accessibility.offersActivitiesFor.hearingImpaired,
                                 offersActivitiesFor.visuallyImpaired = properties.accessibility.offersActivitiesFor.visuallyImpaired)

rechteck_data2 <- data2 %>% filter(long < 13.252697 & long > 10.462170 & lat < 53.079059 & lat > 50.897711)

# rechteck_data2 <- rechteck_data2 %>% drop_na(accessibility.wheelchair)

# SpatailPoints für Orte
coords <- data.frame(long = rechteck_data2$long, lat = rechteck_data2$lat)
prj <- CRS('+proj=longlat')
rechteck_sp2 <- SpatialPoints(coords, proj4string = prj)

# Wo überschneiden sich die Daten?
over <- over(rechteck_sp2, st_map)

# https://stackoverflow.com/questions/24174042/matching-georeferenced-data-with-shape-file-in-r
data_st2 <- cbind.data.frame(rechteck_data2, bundesland=over(rechteck_sp2, st_map))

# Kategorien

final2 <- merge(x = data_st2, y = ac_kategorien, by.x = 'category', by.y = '_id', all.x = TRUE) %>%
  transmute(long = long, 
            lat = lat,
            bundesland = bundesland.GN_KLAR,
            kategorie = translations._id.de,
            accessibility.wheelchair = accessibility.wheelchair,
            accessibility.hearingImpairment =  accessibility.hearingImpairment,
            accessibility.visualImpairment = accessibility.visualImpairment)

final2 <- final2 %>% drop_na(bundesland)

# speichern
write.csv(final, "~/Desktop/MDR Themen/Barrierefreiheit/barrierefreiheit/ST_wheelchair.csv")





