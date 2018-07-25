library("S4Vectors")
library("data.table")

dc <- read.csv("drug_combo_merck.csv")

dc$combination_name <- make.names(dc$combination_name)
dc$combination_name <- gsub(dc$combination_name, pat="...", rep="_", fixed=TRUE)
dc$combination_name <- paste(dc$combination_name, dc$cell_line, dc$BatchID, sep="_")

dc <- cbind(dc["combination_name"],dc)
dc.info <- dc[,c(1,3,4,6,2)]
dc.raw <- dc[,-c(2,3,4,6,8)]
dc.info <- dc.info[!duplicated(dc.info),]

dc.info <- as.data.table(dc.info)
dc.raw <- as.data.table(dc.raw)

dc.info <- dc.info[,c(1,3,4,2,5), with=FALSE]


dc.row.ids <- dc.info[,c(1,2,3)]
dc.col.ids <- dc.info[,c(1,4)]
dc.info <- dc.info[,-c(2,3,4)]


dcTest <- longArray(row.ids = dc.row.ids, 
					col.ids = dc.col.ids, 
					data = list(info=dc.info, raw=dc.raw))

save(dcTest, file="dcTest.RData")
dcTest[c("5-FU","Bortezomib", "Erlotinib"), "A2058"]$info
dcTest[c("5-FU","Bortezomib", "Erlotinib"), "A2058"]$raw













