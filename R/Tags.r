create_tags <- function(resource, tag, ...) {
    query <- list(Action = "CreateTags")
    if(!missing(resource)) {
        resource <- as.list(resource)
        names(resource) <- paste0("ResourceId.", 1:length(resource))
        query <- c(query, resource)
    }
    if(!missing(tag)) {
        query <- c(query, .makelist(tag, type = "Tag"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

delete_tags <- function(resource, tag, ...) {
    query <- list(Action = "DeleteTags")
    if(!missing(resource)) {
        resource <- as.list(resource)
        names(resource) <- paste0("ResourceId.", 1:length(resource))
        query <- c(query, resource)
    }
    if(!missing(tag)) {
        query <- c(query, .makelist(tag, type = "Tag"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_tags <- function(filter, n, token, ...) {
    query <- list(Action = "DeleteTags")
    if(!missing(filter)) {
        filter <- as.list(filter)
        names(filter) <- paste0("Filter.", 1:length(filter))
        query <- c(query, filter)
    }
    if(!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if(!missing(token))
        query$NextToken <- token
    r <- ec2HTTP(query = query, ...)
    return(r)
}
