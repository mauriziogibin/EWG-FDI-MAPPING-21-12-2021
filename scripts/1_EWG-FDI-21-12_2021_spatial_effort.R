#-------------------------------------------------------------------------------
#
# Script to clean, analyse and map the spatial effort and spatial landings
# datasets of the FDI EWG21-12 20210913 - 20210917
# Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every 
# contribution is highlighted.
# Contact: maurizio.gibin@gmail.com
#
# Date: 2021-09-13 - 2021-09-17
#
#
#-------------------------------------------------------------------------------
###################
# Maciej Adamowicz
# 14.09.2018
###################
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list=ls())
#- Settings paths

cDIR = '~/work/EWG-FDI-21-12'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
fshzn            <- paste0(cDIR, "/fishing_zones/")
outPath          <- paste0(cDIR, "/output/")   # output

setwd(csqF)
load(file = "grids.RData")
# FDI DATA ----
setwd(dataF)

####################
#Maciej 19.09.2019 #
####################
load(file="fdi_Table.I.RData")

fdi <- fdi %>%
  group_by(country, year, quarter, gear_typeN, specon_tech, sub_region, rectangle_type,
           rectangle_lat, rectangle_lon, confidential,valid) %>%
  summarise(totfishdays=sum(totfishdays,na.rm=T))
fdi<-ungroup(fdi)

# Please use data.table! In the landings script I achieve the same result 
# using DT in less time. I left this but I suggest to use DT in the future
# especially when the number of rows ia considerable, like Table H.

#Loading the file with subregions assigned to fishing zones
setwd(fshzn)
fishing_zones           <- fread("fishing_zones_2021.csv", stringsAsFactors = F)
setwd(dataF)
#Assign fishing zones to the fdi data
fdi <- left_join(fdi,fishing_zones,by="sub_region")
fdi<-data.table(fdi)
fwrite(fdi[sub_region=="NK",.(nrows=.N),by=.(country,year,confidential,totfishdays,valid)],paste0(outPath,"Table.I.missing.subregion.csv"))

#Remove rows with sub_region = NK and remove BSAs
fdi<-fdi[!sub_region %in% c("NK","BSA")]
#Check if all rows have a fishing zone assigned
unique(fdi[is.na(fishing_zone),.(sub_region)])

#Remove  incorrect data
fdi<-fdi[valid=="Y"]
unique(fdi$rectangle_type)
fdi[is.na(rectangle_type),rectangle_type:="05*05"]
fdi.rectangle.na.csq<-fdi[is.na(rectangle_type)]

#Create id for each lon/lat combination in the fdi data
fdi <- mutate(fdi,
              rect_id = paste(as.character(rectangle_lon),as.character(rectangle_lat),sep = '/'))
#Create id containing lon and lat of the centroid for each ICES rectangle
icesr <- mutate(icesr,rect_id=paste(as.character(ices_x),as.character(ices_y),sep = '/'))

#Join fdi data with ices rectangles dataset
fdi <- left_join(fdi,icesr,by="rect_id")
fdi <- select(fdi,country:icesname)

#Keep the the data with the ICES rectangles assigned in a separate dataset
fdi.ices<-filter(fdi,!is.na(icesname))
sum(fdi.ices$totfishdays)

#Join the fdi.ices dataset with c-squares dataset. Warning! Fishing days will be doubled.
fdi.ices<-left_join(fdi.ices,csq05,by="icesname") 
#Divide fishing days by 2 (each ICES rectangle has 2 c-squares)
fdi.ices<-mutate(fdi.ices,totfishdays=totfishdays/2)
#Check if the total fishing days remained the same
sum(fdi.ices$totfishdays)
fdi.ices<-select(fdi.ices,country:icesname,cscode)

#Keep the the data with the ICES rectangles NOT assigned in a separate dataset 
fdi.not.ices<-filter(fdi,is.na(icesname))
fdi.not.ices.05.1<-filter(fdi.not.ices,rectangle_type=="05*1")
#Handle 0.5x1 rectangles outside ICES area
#Create the ids containing lon and lat of the centre left and centre right of the csquare
sum(fdi.not.ices.05.1$totfishdays)
csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(csq_y),sep='/'))#rect_id = centre/left
fdi.not.ices.05.1.left<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>% 
  mutate(totfishdays=totfishdays/2)

csq05<-mutate(csq05,
              rect_id=paste(as.character(e_csq),as.character(csq_y),sep='/'))#rect_id = centre/right
fdi.not.ices.05.1.right<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>% 
  mutate(totfishdays=totfishdays/2)
fdi.not.ices.05.1<-rbind(fdi.not.ices.05.1.left,fdi.not.ices.05.1.right)
fdi.not.ices.05.1<-select(fdi.not.ices.05.1,country:icesname.x,cscode)
fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,icesname=icesname.x)
sum(fdi.not.ices.05.1$totfishdays)

print(paste0("fdi.not.ices.05.1 number of rows without cscode: ",nrow(filter(fdi.not.ices.05.1,is.na(cscode)))))
temp<-filter(fdi.not.ices.05.1,is.na(cscode)) %>% 
  select(country:icesname)
fdi.not.ices.05.1<-filter(fdi.not.ices.05.1,!is.na(cscode))

# Check what rectangle types are present in the rest of the data
fdi.not.ices %>% 
  filter(rectangle_type!="05*1") %>% 
  group_by(rectangle_type) %>% 
  summarise(n=n())

# The rest of the data has 05*05, 1*1 and 5*5 rectangle_type

# Handle 05*05 rectangles
fdi.csq.05.05 <- filter(fdi.not.ices,rectangle_type=="05*05") %>% 
  rename(csq_c_id=rect_id)

# Create the id containing lon and lat of the centroid for each c-square
csq05<-mutate(csq05,
              csq_c_id=paste(as.character(csq_x),as.character(csq_y),sep='/'))


# Join the fdi.csq.c dataset with c-squares dataset.
fdi.csq.05.05<-left_join(fdi.csq.05.05,csq05,by="csq_c_id")
fdi.csq.05.05<-select(fdi.csq.05.05,country:icesname.x,cscode)

# Check if there are any rows without csquare assigned
print(paste0("fdi.csq.05.05 number of rows without cscode: ",nrow(filter(fdi.csq.05.05,is.na(cscode)))))

# Handle 1*1 rectangles
fdi.csq.1.1 <- filter(fdi.not.ices,rectangle_type=="1*1") %>% 
  rename(csq_c_id=rect_id)

csquares.1.1 <- select(fdi.csq.1.1,csq_c_id,rectangle_lon,rectangle_lat) %>% 
  mutate(key=1) %>% 
  distinct()
grid.for.1.1 <- data.frame(lon_diff = c(0,0.5,0.5,0),
                           lat_diff = c(0,0,0.5,0.5),
                           key= c(1))
csquares.1.1 <- inner_join(csquares.1.1,grid.for.1.1,by="key") %>% 
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>% 
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.csq.1.1$totfishdays)
fdi.csq.1.1 <- fdi.csq.1.1 %>% 
  inner_join(csquares.1.1,by="csq_c_id") %>% 
  mutate(totfishdays=totfishdays/4,
         rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
sum(fdi.csq.1.1$totfishdays)

# create bottom-left id of csquare
csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
fdi.csq.1.1<-left_join(fdi.csq.1.1,csq05,by="rect_id")
fdi.csq.1.1<-select(fdi.csq.1.1,country:fishing_zone,rect_id,icesname.x,cscode)
print(paste0("fdi.csq.1.1 number of rows without cscode: ",nrow(filter(fdi.csq.1.1,is.na(cscode)))))

# Handle 5*5 rectangles
fdi.csq.5.5 <- filter(fdi.not.ices,rectangle_type=="5*5") %>% 
  rename(csq_c_id=rect_id)

csquares.5.5 <- select(fdi.csq.5.5,csq_c_id,rectangle_lon,rectangle_lat) %>% 
  mutate(key=1) %>% 
  distinct()

grid.for.5.5 <- inner_join(data.frame(lon_diff=seq(-2, by=0.5, length.out = 10), key=1),
                          data.frame(lat_diff=seq(-2, by=0.5, length.out = 10), key=1), 
                          by="key")

csquares.5.5 <- inner_join(csquares.5.5,grid.for.5.5,by="key") %>% 
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>% 
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.csq.5.5$totfishdays)
fdi.csq.5.5 <- fdi.csq.5.5 %>% 
  inner_join(csquares.5.5,by="csq_c_id") %>% 
  mutate(totfishdays=totfishdays/100,
         rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
sum(fdi.csq.5.5$totfishdays)

# create bottom-left id of csquare
csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
fdi.csq.5.5<-left_join(fdi.csq.5.5,csq05,by="rect_id")
fdi.csq.5.5<-select(fdi.csq.5.5,country:fishing_zone,rect_id,icesname.x,cscode)
print(paste0("fdi.csq.5.5 number of rows without cscode: ",nrow(filter(fdi.csq.5.5,is.na(cscode)))))

fdi.ices<-rename(fdi.ices,geo_id=rect_id)
fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,geo_id=rect_id)
fdi.csq.05.05<-rename(fdi.csq.05.05,geo_id=csq_c_id,icesname=icesname.x)
fdi.csq.1.1<-rename(fdi.csq.1.1,geo_id=rect_id,icesname=icesname.x)
fdi.csq.5.5<-rename(fdi.csq.5.5,geo_id=rect_id,icesname=icesname.x)

result<-fdi.ices %>% 
  rbind(fdi.csq.05.05) %>% 
  rbind(fdi.not.ices.05.1) %>% 
  rbind(fdi.csq.1.1) %>% 
  rbind(fdi.csq.5.5) %>% 
  as.data.frame()

print(paste0("number of rows without cscode: ",nrow(filter(result,is.na(cscode)))))

result <- as.data.table(result)
result <-
  result[, sum(totfishdays,na.rm=T), by = .(
    country,
    year,
    quarter,
    gear_typeN,
    specon_tech,
    sub_region,
    fishing_zone,
    icesname,
    confidential,
    cscode
  )]
result[,`:=`(totfishdays = V1,
          V1 = NULL)]


result<-left_join(result,csq05,by="cscode") %>% 
  select(country:totfishdays,geometry) %>% 
  rename(icesname=icesname.x)


print(paste0("Is total effort correct? : ",round(sum(result$totfishdays),1)==round(sum(fdi$totfishdays),1)))

result_sf<-st_sf(result)
setwd(outPath)
st_write(result_sf,layer="spatial_effort.shp",dsn=".",driver="ESRI Shapefile", delete_layer = TRUE)
save(result,file='spatial_effort.RData')

setwd(outPath)
dir.create("effort")
dir.create("effort/areas")
dir.create("effort/errors")
dir.create("effort/gears")
dir.create("effort/specons")
#library(filesstrings)
fList <- list.files(path='.',patter=glob2rx('*I*.csv'))
file.copy(fList,'./effort/',overwrite = T)
file.remove(fList)
