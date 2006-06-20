rds2repos <- function(from) {
    newname <- paste(from@name, "repos", sep = ".")
    dir.create(newname)

    dbDir <- from@dbDir
    keys <- dbList(from)
    files <- file.path(dbDir, keys)
    md5 <- md5sum(files)
    meta <- data.frame(key = keys, md5sum = md5)

    con <- gzfile(file.path(newname, "reposInfo.gz"), "wb")
    tryCatch({
        write.dcf(meta, file = con)
    }, finally = close(con))
    
    datadir <- file.path(newname, "Data")
    dir.create(datadir)
    
    for(filename in files) 
        file.copy(filename, datadir)
    TRUE
}
