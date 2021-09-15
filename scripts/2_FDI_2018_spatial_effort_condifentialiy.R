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

setwd(dir = "~/work/FDI2018/")
options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list=ls())
#- Settings paths
codePath         <- "~/work/FDI2018/scripts/"         # R scripts location
dataF            <- "~/work/FDI2018/ftp/"            # data folder
csqF             <- "~/work/FDI2018/csquares/"        # csquares location
icesrF           <- "~/work/FDI2018/ices_rects/" 
outPath          <- "~/work/FDI2018/output/"           # output

# FDI DATA ----
setwd(outPath)
# Loading the spatial effort data from ftp
# fdi            <- fread("tableI.csv", stringsAsFactors = F)
fdiSF <- st_read("spatial_effort.shp",stringsAsFactors = F)
names(fdiSF) <- c("country","year","quarter","gear_typeN","specon_tech",
                  "sub_region","fishing_zone","icesname","confidential","cscode",
                  "fishing_days","geometry")
# creating the data.table
fdi <- as.data.table(`st_geometry<-`(fdiSF,NULL))
# Omitting BSA area for mapping
BSArows            <- nrow(fdi[sub_region == "BSA",])
fdi                <- fdi[! sub_region == "BSA",]
# Omitting the confidential ones
cat('Number and Proportion of confidential rows on the total of rows')
nrow(fdi[confidential=="Y"])
round((nrow(fdi[confidential=="Y"])/nrow(fdi))*100,2)
fdiTableIConfByCountry <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                 confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                 fdVol = .SD[confidential=="Y",sum(fishing_days)],
                                 fdProp = round(.SD[confidential=="Y",sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                              by= country]
fdiTableIConfByCountryYear <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                 confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                 fdVol = .SD[confidential=="Y",sum(fishing_days)],
                                 fdProp = round(.SD[confidential=="Y",sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                              by= c("country","year")]
# When people are lazy or extremely smart ihihi
fdi[,major := ifelse(sub_region %like% 'GSA', sub_region,substr(sub_region,1,2)),]

fdiTableIConfByCountryYearSup <- fdi[,.(conf =  nrow(.SD[confidential=="Y",]),
                                     confProp = round((nrow(.SD[confidential=="Y",])/(nrow(.SD)))*100,2),
                                     fdVol = .SD[confidential=="Y",sum(fishing_days)],
                                     fdProp = round(.SD[confidential=="Y",sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                                  by= c("country","year","major")]

fdiTableIConfByCountry <- fdiTableIConfByCountry[!conf==0,] 
fdiTableIConfByCountryYear <- fdiTableIConfByCountryYear[!conf==0,]
fdiTableIConfByCountryYearSup <- fdiTableIConfByCountryYearSup[!conf==0,]
setwd(outPath)
fwrite(fdiTableIConfByCountry, "fdiTableIConfByCountry.csv")
fwrite(fdiTableIConfByCountryYear, "fdiTableIConfByCountryYear.csv")
fwrite(fdiTableIConfByCountryYearSup, "fdiTableIConfByCountryYearSup.csv") 
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
fdiTableIConfByCsq <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                             confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                             fdVol = .SD[rowsxcsq <3,sum(fishing_days)],
                             fdProp = round(.SD[rowsxcsq <3,sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                          by= c("country")]
fdiTableIConfByCsq <- fdiTableIConfByCsq[!conf==0,]
fdiTableIConfByCsqYear <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                               confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                               fdVol = .SD[rowsxcsq <3,sum(fishing_days)],
                               fdProp = round(.SD[rowsxcsq <3,sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                            by= c("country",'year')]
fdiTableIConfByCsqYear <- fdiTableIConfByCsqYear[!conf==0,]
fdiTableIConfByCsqYearSup <- fdiNC[,.(conf =  nrow(.SD[rowsxcsq <3,]),
                                   confProp = round((nrow(.SD[rowsxcsq <3,])/(nrow(.SD)))*100,2),
                                   fdVol = .SD[rowsxcsq <3,sum(fishing_days)],
                                   fdProp = round(.SD[rowsxcsq <3,sum(fishing_days)]/(.SD[,sum(fishing_days)])*100,2)),
                                by= c("country",'year','major')]
fdiTableIConfByCsqYearSup <- fdiTableIConfByCsqYearSup[!conf==0,]
# Saving
setwd(outPath)
fwrite(fdiTableIConfByCsq, "fdiTableIConfByCsq.csv")
fwrite(fdiTableIConfByCsqYear, "fdiTableIConfByCsqYear.csv")
fwrite(fdiTableIConfByCsqYearSup, "fdiTableIConfByCsqYearSup.csv") 
