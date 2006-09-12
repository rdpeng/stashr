setValidity("filehashRemote",
            function(object) {
                if(length(object@name) == 0)
                    return("'filehashRemote' object needs a 'name'")
                if(length(grep("^http://", object@url, fixed = TRUE, perl = TRUE)) == 0)
                    return("'filehashRemote' object needs a 'url' of type 'http://'")
                TRUE
            })

setValidity("filehashLocal",
            function(object) {
                if(length(object@name) == 0)
                    return("'filehashLocal' object needs a 'name'")
                if(length(grep("^file://", object@url, fixed = TRUE, perl = TRUE)) == 0)
                    return("'filehashLocal' object needs a 'url' of type 'file://'")
                TRUE
            })
