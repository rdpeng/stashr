setValidity("filehashRemote",
            function(object) {
                if(length(object@name) == 0)
                    return("'filehashRemote' object needs a 'name'")
                TRUE
            })
