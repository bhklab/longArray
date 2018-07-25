#######
# Pre-Amble: The goal is to work with the table in drug_combo_merck by 
# subsetting using the drug and cell line names. Implementing `[,]` 
# interface for 



library("S4Vectors")
library("data.table")

dc <- read.csv("drug_combo_merck.csv", as.is=TRUE)


## Make some unique keys
dc$combination_name <- make.names(dc$combination_name)
dc$combination_name <- gsub(dc$combination_name, pat="...", rep="_", fixed=TRUE)
dc$combination_name <- paste(dc$combination_name, dc$cell_line, dc$BatchID, sep="_")

## Split into different tables
dc <- cbind(dc["combination_name"],dc)
dc.info <- dc[,c(1,3,4,6,2)]
dc.raw <- dc[,-c(2,3,4,6,8)]
dc.info <- dc.info[!duplicated(dc.info),]

dc.info <- as.data.table(dc.info)
dc.raw <- as.data.table(dc.raw)

dc.info <- dc.info[,c(1,3,4,2,5), with=FALSE]

## Define the row ids
dc.row.ids <- dc.info[,c(1,2,3)]

# Define the col ids
dc.col.ids <- dc.info[,c(1,4)]

# remove the row and column ids from the data
dc.info <- dc.info[,-c(2,3,4)]

source("R/longArray.R")
dcTest <- longArray(row.ids = dc.row.ids, 
					col.ids = dc.col.ids, 
					data = list(info=dc.info, raw=dc.raw))

# save(dcTest, file="dcTest.RData")

# example 
dcTest #Prints object

#Subsetting works using drug names and cell names
dcTest[c("5-FU","Bortezomib", "Erlotinib"), "A2058"]

## Tables inside the object are subsetted by PK after the "rows" and "columns" are subsetted

# Contains only the experiment info for experiments that intersect those cell names and drug names
dcTest[c("5-FU","Bortezomib", "Erlotinib"), "A2058"]$info # "row and columns" prepended 


# Contains only the experiment data for experiments that intersect those cell names and drug 
dcTest[c("5-FU","Bortezomib", "Erlotinib"), "A2058"]$raw # "row and columns" prepended




library(SummarizedExperiment)

dcSE <- SummarizedExperiment(dcTest)

dcSE[1:2,1:2] # Subsetting works

## dimnames<- is not implemented so withDimnames needs to be false
assay(dcSE[1:2,1:2], 1, withDimnames=FALSE)$raw






