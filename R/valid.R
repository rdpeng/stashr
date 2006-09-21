setValidity("filehashRemote",
            function(object) {
                if(length(object@name) == 0)
                    return("object needs a 'name'")
                if(!validURL(object@url))
                    return("object needs a 'url' of type 'http://'")
                TRUE
            })

validURL <- function(URL) {
    if(length(grep("^http://", URL, fixed = TRUE, perl = TRUE)) > 0)
        TRUE
    else
        FALSE
}
