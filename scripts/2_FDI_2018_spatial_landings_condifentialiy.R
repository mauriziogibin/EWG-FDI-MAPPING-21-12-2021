#-------------------------------------------------------------------------------
#
# Script to analise confidentialiy TABLE I of FDI data 
#
# By: Maurizio Gibin
# Code by: Maurizio Gibin
# Contact: maurizio.gibin@ec.europa.eu
#
# Date: 2017-09-11
#
# 
#-------------------------------------------------------------------------------
# library(maptools)
library(data.table)
library(sf)
library(Hmisc)
library(ggplot2)

#- Clear workspace
rm(list=ls())

setwd(dir = "~/work/FDI2018/")
options(scipen = 999)
options(digits = 9)

#- Settings paths
codePath         <- "~/work/FDI2018/scripts/"         # R scripts location
dataF            <- "~/work/FDI2018/ftp/"            # data folder
csqF             <- "~/work/FDI2018/csquares/"        # csquares location
icesrF           <- "~/work/FDI2018/ices_rects/" 
outPath          <- "~/work/FDI2018/output/"           # output

# FDI DATA ----
setwd(outPath)
# Loading the spatial landings data from ftp
fdiSF <- st_read("spatial_landings.shp",stringsAsFactors = F)
names(fdiSF) <- c("country","year","quarter","gear_typeN","specon_tech",
                  "sub_region","fishing_zone","icesname","confidential","cscode",
                  "landings","geometry")
# creating the data.table
fdi <- as.data.table(`st_geometry<-`(fdiSF,NULL))
# Omitting BSA area for mapping
BSArows            <- nrow(fdi[sub_region == "BSA",])
fdi                <- fdi[! sub_region == "BSA",]
# Omitting the confidential ones
cat('Number and Proportion of confidential rows on the total of rows')
nrow(fdi[confidential=="Y"])
round((nrow(fdi[confidential=="Y"])/nrow(fdi))*100,2)
fdiTableHConfByCountry <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                 confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                 landVol = .SD[confidential=="Y",sum(landings)],
                                 landProp = round(.SD[confidential=="Y",sum(landings)]/(.SD[,sum(landings)])*100,2)),
                              by= country]
fdiTableHConfByCountryYear <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                 confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                 landVol = .SD[confidential=="Y",sum(landings)],
                                 landProp = round(.SD[confidential=="Y",sum(landings)]/(.SD[,sum(landings)])*100,2)),
                              by= c("country","year")]
# When people are lazy or extremely smart ihihi
fdi[,major := ifelse(sub_region %like% 'GSA', sub_region,substr(sub_region,1,2)),]

fdiTableHConfByCountryYearSup <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                     confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                     landVol = .SD[confidential=="Y",sum(landings)],
                                     landProp = round(.SD[confidential=="Y",sum(landings)]/(.SD[,sum(landings)])*100,2)),
                                  by= c("country","year","major")]

fdiTableHConfByCountry <- fdiTableHConfByCountry[!conf==0,] 
fdiTableHConfByCountryYear <- fdiTableHConfByCountryYear[!conf==0,]
fdiTableHConfByCountryYearSup <- fdiTableHConfByCountryYearSup[!conf==0,]
setwd(outPath)
fwrite(fdiTableHConfByCountry, "fdiTableHConfByCountry.csv")
fwrite(fdiTableHConfByCountryYear, "fdiTableHConfByCountryYear.csv")
fwrite(fdiTableHConfByCountryYearSup, "fdiTableHConfByCountryYearSup.csv") 
# Now we will omit the confidential and do an analysis on the number of records by csquare.
# We are doing this for future dissemination. Even if the confidential records are omitted,
# It can happen that some records can still be affected by confidentiality, especially in some fishing zones.
# Now I have to calculate how many records per csquare 
recordsCSQ <- fdi[!confidential =='Y', .(recordspercsq = nrow(.SD)), by = cscode]
# Number of csquares that have at least one record (not one vessel, but one strata)
nrow(recordsCSQ[recordspercsq ==1,])
round(nrow(recordsCSQ[recordspercsq ==1,])/nrow(recordsCSQ)*100,2)
# Number of csquares that have less thank three record (not vessels, but strata)
nrow(recordsCSQ[recordspercsq < 3,])
round(nrow(recordsCSQ[recordspercsq < 3,])/nrow(recordsCSQ)*100,2)
# Number of csquares by country
recordsCSQ <- fdi[!confidential =='Y', .(recordspercsq = nrow(.SD)), by = c("cscode",'country')]# Loading the data from Maciew
# Omitting the confidential records
fdiNC <- fdi[!confidential =='Y',]
# Calculating the number of rows with less than three records
fdiNC[,rowsxcsq :=nrow(.SD), by = c('cscode','country')]
fdiTableHConfByCsq <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                             confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                             landVol = .SD[rowsxcsq <3,sum(landings)],
                             landProp = round(.SD[rowsxcsq <3,sum(landings)]/(.SD[,sum(landings)])*100,2)),
                          by= c("country")]
fdiTableHConfByCsq <- fdiTableHConfByCsq[!conf==0,]
fdiTableHConfByCsqYear <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                               confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                               landVol = .SD[rowsxcsq <3,sum(landings)],
                               landProp = round(.SD[rowsxcsq <3,sum(landings)]/(.SD[,sum(landings)])*100,2)),
                            by= c("country",'year')]
fdiTableHConfByCsqYear <- fdiTableHConfByCsqYear[!conf==0,]
fdiTableHConfByCsqYearSup <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                                   confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                                   landVol = .SD[rowsxcsq <3,sum(landings)],
                                   landProp = round(.SD[rowsxcsq <3,sum(landings)]/(.SD[,sum(landings)])*100,2)),
                                by= c("country",'year','major')]
fdiTableHConfByCsqYearSup <- fdiTableHConfByCsqYearSup[!conf==0,]
# Saving
setwd(outPath)
fwrite(fdiTableHConfByCsq, "fdiTableHConfByCsq.csv")
fwrite(fdiTableHConfByCsqYear, "fdiTableHConfByCsqYear.csv")
fwrite(fdiTableHConfByCsqYearSup, "fdiTableHConfByCsqYearSup.csv") 
