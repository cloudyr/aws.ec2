create_keypair <- function(name, ...) {
    query <- list(Action = "CreateKeyPair")
    if(nchar(name) > 255)
        stop("'name' must be <= 255 characters")
    query$KeyName <- name
    r <- ec2HTTP(query = query, ...)
    return(r)
}

delete_keypair <- function(name, ...) {
    query <- list(Action = "DeleteKeyPair")
    if(nchar(name) > 255)
        stop("'name' must be <= 255 characters")
    query$KeyName <- name
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_keypairs <- function(name, filter, ...) {
    query <- list(Action = "DescribeKeyPairs")
    if(!missing(name)) {
        name <- as.list(name)
        names(name) <- paste0("KeyName.", 1:length(name))
        query <- c(query, name)
    }
    if(!missing(filter)) {
        vfilter <- c("fingerprint", "key-name")
        if(any(!names(filter) %in% vfilter))
            stop("'filter' must be in: ", paste0(vfilter, collapse = ", "))
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

import_keypair <- function(name, publickey, ...) {
    query <- list(Action = "ImportKeyPair")
    if(nchar(name) > 255)
        stop("'name' must be <= 255 characters")
    query$KeyName <- name
    query$PublicKeyMaterial <- base64encode(charToRaw(publickey))
    r <- ec2HTTP(query = query, ...)
    return(r)    
}
