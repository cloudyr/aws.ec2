#' @rdname keypairs
#' @title EC2 Keypairs
#' @description Get, create, delete, and import EC2 keypairs
#' @template keypair
#' @param filter \dots
#' @param publickey \dots
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' k <- create_keypair("test_keypair")
#' get_keypairs()
#' delete_keypair(k)
#' }
#' @export
get_keypairs <- function(keypair, filter, ...) {
    query <- list(Action = "DescribeKeyPairs")
    if (!missing(keypair)) {
        if (inherits(keypair, "ec2_keypair")) {
            keypair <- list(get_keypairname(keypair))
        } else if (is.character(keypair)) {
            keypair <- as.list(get_keypairname(keypair))
        } else {
            keypair <- lapply(keypair, get_keypairname)
        }
        names(keypair) <- paste0("KeyName.", 1:length(keypair))
        query <- c(query, keypair)
    }
    if (!missing(filter)) {
        vfilter <- c("fingerprint", "key-name")
        if (any(!names(filter) %in% vfilter)) {
            stop("'filter' must be in: ", paste0(vfilter, collapse = ", "))
        }
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$keySet, `class<-`, "ec2_keypair"))
}

#' @rdname keypairs
#' @export
create_keypair <- function(keypair, ...) {
    query <- list(Action = "CreateKeyPair")
    keypair <- get_keypairname(keypair)
    if (nchar(keypair) > 255) {
        stop("'keypair' must be <= 255 characters")
    }
    query$KeyName <- keypair
    r <- ec2HTTP(query = query, ...)
    return(structure(r, class = "ec2_keypair"))
}

#' @rdname keypairs
#' @export
delete_keypair <- function(keypair, ...) {
    query <- list(Action = "DeleteKeyPair")
    keypair <- get_keypairname(keypair)
    if (nchar(keypair) > 255) {
        stop("'keypair' must be <= 255 characters")
    }
    query$KeyName <- keypair
    r <- ec2HTTP(query = query, ...)
    return(r$return)
}

#' @rdname keypairs
#' @importFrom base64enc base64encode
#' @export
import_keypair <- function(keypair, publickey, ...) {
    query <- list(Action = "ImportKeyPair")
    if (nchar(keypair) > 255) {
        stop("'keypair' must be <= 255 characters")
    }
    query$KeyName <- keypair
    query$PublicKeyMaterial <- base64encode(charToRaw(publickey))
    r <- ec2HTTP(query = query, ...)
    return(r)    
}

get_keypairname <- function(x) {
    if (is.character(x)) {
        return(x)
    } else if (inherits(x, "ec2_keypair")) {
        return(x$keyName[[1]])
    }
}
