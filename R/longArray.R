require(S4Vectors)
require(SummarizedExperiment)

setClassUnion("dt.df", c("data.table", "data.frame"))

.longArray <- setClass("longArray", slots = list(col.ids = "dt.df",
                                                 row.ids = "dt.df",
                                                 listData = "list",
                                                 storage.type = "character"),
                                                 contains = c("List"))


.longArray.DT <- setClass("longArray.DT", contains = "longArray")


longArray <- function(row.ids, col.ids, data, ...){
    dot.args <- list(...)

    if("data.table" %in% class(row.ids)){
        setkey(row.ids)
        
    }
    if("data.table" %in% class(col.ids)){
        setkey(col.ids)   
    }

    !any(!col.ids[,1] == row.ids[,1]) || stop("Column and Row defining tables have different number of PKs")


    colnames(row.ids)[1] <- "id"
    colnames(col.ids)[1] <- "id"
    # if(!"list" %in% class(data)){
    #     data <- list(data)
    # }
    if("data.table" %in% class(data[[1]])){
        storage.type <- "data.table"

        ## I am using the wrong idiom below::
        data <- lapply(data, function(xx) {
            colnames(xx)[1] <- "id"; return(xx)
        })
        if(!all(sapply(data, function(xx) xx[,all(id %in% row.ids[,id])]))) stop("Error")
        for(dd in data){
            setkeyv(dd, "id")
        }
        return(.longArray.DT(col.ids = col.ids, row.ids = row.ids, listData = data, storage.type = storage.type))
    } else {
        storage.type <- "data.frame"
        stop("Not Implemented")
    }
}

setMethod(`[`, "longArray.DT", function(x, i, j, ..., drop=if (missing(i)) TRUE else length(cols) == 1){
    ## how to do this efficiently? From tests, it seems that %in% is the fastest?
    ## Faster than "on = " ??? Seems not after more testing
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop
    if(missing(i)){
        i <- rownames(x)
    }

    if(missing(j)){
        if(Narg == 2) {
            j <- i
            i <- rownames(x)
        } else {
            j <- colnames(x)
        }
    }

    if(is.numeric(i)){
        warning("Numeric i index was passed, indexing into rownames")
        i <- rownames(x)[i]
    }
    if(is.numeric(j)){
        warning("Numeric j index was passed, indexing into colnames")
        j <- colnames(x)[j]
    }
    if(is.logical(i)){
        i <- rownames(x)[i]
    }
    if(is.logical(j)){
        j <- colnames(x)[j]
    }

    if(anyDuplicated(i)){
        warning("Duplicate row ids were passed to object, taking unique ids")
        i <- unique(i)
    } 
    if(anyDuplicated(j)){
        warning("Duplicate column ids were passed to object, taking unique ids")
        j <- unique(j)
    }

    row.ids <- x@row.ids
    iCols <- colnames(x@row.ids)[-1]
    for (iCol in iCols){
        row.ids <- row.ids[list(i), on=iCol, nomatch=0]
    }
    rowIds <- row.ids[,id]


    col.ids <- x@col.ids[rowIds,]
    jCols <- colnames(x@col.ids)[-1]
    for (jCol in jCols){
        col.ids <- col.ids[list(j), on=jCol, nomatch=0]
    }

    ids <- col.ids[,id]

    newXData <- lapply(x@listData, function(x) return(x[ids,]))
    return(longArray(col.ids = col.ids[ids,on="id"], row.ids = row.ids[ids,on="id"], data = newXData))
})


## XXX: Breaks when listData[[i]] has no columns except PK!!!
setMethod("[[", c("longArray", "ANY", "missing"),
    function(x, i, j, ...)
{   
    ids <- x@listData[[i]][,id]
    return(cbind(x@row.ids[ids,],x@col.ids[ids,-1],x@listData[[i]][,-1]))
})

# setReplaceMethod("[[", c("SummarizedExperiment", "ANY", "missing"),
#     function(x, i, j, ..., value)
# {
#     colData(x)[[i, ...]] <- value
#     x
# })

.DollarNames.SummarizedExperiment <- function(x, pattern = "")
    grep(pattern, names(x@listData), value=TRUE)

setMethod("$", "longArray",
    function(x, name)
{
    x[[name]]
})

# setReplaceMethod("$", "SummarizedExperiment",
#     function(x, name, value)
# {
#     colData(x)[[name]] <- value
#     x
# })



# setGeneric("toTable", function(x) standardGeneric("toTable"))
# setMethod("toTable", "longArray.DT", function(x){
#     # return(x@data[x@info, on = colnames(x@data)[1]])
#     return(x@info[x@data, on = colnames(x@data)[1]])

# }) 

## TODO:: FIXME:: this is inefficient 
setMethod("dim", "longArray.DT", function(x){
    return(c(length(unique(unlist(x@row.ids[, -1]))), 
             length(unique(unlist(x@col.ids[, -1])))))
})


setMethod("dimnames", "longArray.DT", function(x){
    return(list(unique(unlist(x@row.ids[, -1])), 
                unique(unlist(x@col.ids[, -1]))))
})

setMethod("show", signature=signature(object="longArray"), 
    function(object) {
        dms <- dim(object)
        cat("A longArray object with the following dimensions: \n")
        cat(dms[1],"\"rows\" across ", length(object@row.ids) - 1," variable(s) \n")
        cat(dms[2],"\"columns\" across ", length(object@col.ids) - 1," variable(s) \n")
})




# sens.info[rep(list(rCI), 2), on=c("cellid", "drugid"), nomatch=0]