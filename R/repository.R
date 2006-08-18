rds2repos <- function(from) {
    library(tools)
   
    if(!is(from, "filehashRDS"))
        stop("'from' should be a 'filehashRDS' object")
    newname <- paste(from@name, "repos", sep = ".")
    dir.create(newname)

    datadir <- file.path(newname, "Data")
    dir.create(datadir)

    dbDir <- from@dbDir
    keys <- dbList(from)
    files <- file.path(dbDir, keys)
    md5 <- md5sum(files)
    meta <- data.frame(key = keys, md5sum = md5, dir = "Data")

    con <- gzfile(file.path(newname, "reposInfo.gz"), "wb")
    tryCatch({
        write.dcf(meta, file = con)
    }, finally = close(con))
    

    for(filename in files) 
        file.copy(filename, datadir)
    TRUE
}

