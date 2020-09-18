#-------------------------------------------------------------------------------
#
# Script to clean, analyse and map the spatial effort and spatial landings
# datasets of the FDI EWG18-11 20190910 - 20180914
# Tor 3 team : Maciej, Maksims, Maurizio, Tommaso (3MT). Every 
# contribution is highlighted.
# Contact: maurizio.gibin@ec.europa.eu
#
# Date: 2018-09-10 - 2018-09-14
#
#
#-------------------------------------------------------------------------------
#########
#Maurizio#
#########
library(data.table)
library(xlsx)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list=ls())

cDIR = '~/work/EWG-FDI-20-10'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
outPath          <- paste0(cDIR, "/output/")   # output

# FDI DATA ----
setwd(outPath)
# 
# We need to select the nuber of fields to keep and also made a recap on the number of rows and landings/effort we loose.
# TABLE I Reporting ----
setwd(paste0(outPath,'landings/'))
tH <- list.files(path = '.', pattern = glob2rx("*table.H*.csv"))

table.H.errors <- lapply(tH,fread)
names(table.H.errors) <- tH
# The third table is the  errors in unit weight that is already summarised,
# while the others are not.
errors.unit.weight <- table.H.errors[[1]]
table.H.errors <- table.H.errors[-1]
errors.unit.weight <-
  errors.unit.weight[, list(totwghtlandg = sum(totwghtlandg),
                           nrows = .N),
                    by = .(country, year)]

errors.unit.weight[,totwghtlandg:=round(totwghtlandg,0)]
names(errors.unit.weight) <- c("Country", "Year", "Total Landings", "Number of rows")

wb<-createWorkbook(type="xlsx")

CellStyle(wb, dataFormat=NULL, alignment=NULL,
          border=NULL, fill=NULL, font=NULL)
TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, 
                                   color = "000000" , isBold=TRUE)
SUB_TITLE_STYLE <- CellStyle(wb) + 
  Font(wb,  heightInPoints=12,
       isItalic=TRUE, isBold=FALSE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 


errors.total <- fread('fdi_TABLE_H_errors.csv')
errors.total <- errors.total[,.(country,year,totwghtlandg)]
errors.total <- errors.total[, list(totwghtlandg = sum(totwghtlandg),
                                    nrows = nrow(.SD)),
                             by = .(country, year)]
errors.total[,totwghtlandg := round(totwghtlandg,0)]
errors.total <- setorder(errors.total,country)
names(errors.total) <- c("Country", "Year", "Total Landings", "Number of rows")

# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
sheet <- createSheet(wb, sheetName = "Table H Errors")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table H Errors",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Recap on the number of rows in Table H with errors",
              titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(errors.total, sheet, startRow=5, startColumn=1,
             row.names = F,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(errors.total)), colWidth=16)

# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
sheet <- createSheet(wb, sheetName = "Table H Errors in Unit Weight")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table H Errors in Unit Weight",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Recap on the number of rows in Table H with incorrect unit weight",
              titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(errors.unit.weight, sheet, startRow=5, startColumn=1,
             row.names = F,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(errors.unit.weight)), colWidth=16)
# Add a plot into a worksheet
# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
missing.subregion <- table.H.errors[[2]]
table.H.errors <- table.H.errors[-2]
missing.subregion <-
  missing.subregion[, list(totwghtlandg = sum(totwghtlandg),
                            nrows = .N),
                     by = .(country, year)]

missing.subregion[,totwghtlandg:=round(totwghtlandg,0)]
names(missing.subregion) <- c("Country", "Year", "Total Landings", "Number of rows")

sheet <- createSheet(wb, sheetName = "Table H Missing Subregion")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table H Missing Subregion",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Recap on the number of rows in Table H with unknown Subregion",
              titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(missing.subregion, sheet, startRow=5, startColumn=1,
             row.names = F,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(missing.subregion)), colWidth=16)
# Add a plot into a worksheet

#
# The missing subregion table does not have a total of fishing effort or landing indicated
# For the errors table we consider country number of rows affected by the issue and then total effort lost.

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# table <- table.I.errors[1]

addTables <- function(table){
  names(table) <- gsub('\\.',' ',names(table))
  names(table) <- gsub(' csv','',names(table))
  names(table) <- simpleCap(names(table))
  sheetName    <- names(table) 
  table <- rbindlist(table)
  table[,totwghtlandg:=round(totwghtlandg,0)]
  names(table) <-  c("Country", "Year", "Total Landings", "Number of rows")
  sheet <- createSheet(wb, sheetName = sheetName)
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title=sheetName,
                titleStyle = TITLE_STYLE)
  # Add sub title
  xlsx.addTitle(sheet, rowIndex=3, 
                title="Recap on the number of rows and landings by country and year",
                titleStyle = SUB_TITLE_STYLE)
  # Add a table into a worksheet
  #++++++++++++++++++++++++++++++++++++
  addDataFrame(table, sheet, startRow=5, startColumn=1,
               row.names = F,
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  # Change column width
  setColumnWidth(sheet, colIndex=c(1:ncol(table)), colWidth=16)
}

# table.H.errors <- Map(cbind,table.H.errors,valid ="N")
table.H.errors <- lapply(table.H.errors,function(x){return(x[,.(country,year,totwghtlandg)])})
table.H.tables <- lapply(table.H.errors,function(x){return(x <- x[,list(totwghtlandg = sum(totwghtlandg),
                                                                        nrows = nrow(.SD)),
                                                                  by = .(country,year)])})

lapply(1:length(table.H.tables),function(x){addTables(table.H.tables[x])})

saveWorkbook(wb, "Table.H.checks.Tor.3.2.xlsx")
