#' @rdname tags
#' @title Tags
#' @description Tag an EC2 Resource
#' @param resource A character vector specifying one or more resource IDs, typically EC2 instance IDs.
#' @param tag A named character string of key-value pairs of tag names and their corresponding values.
#' @template filter
#' @param n \dots
#' @param page \dots
#' @template dots
#' @return A logical.
#' @seealso \code{\link{associate_ip}}
#' @export
create_tags <- function(resource, tag, ...) {
    query <- list(Action = "CreateTags")
    resource <- as.list(resource)
    names(resource) <- paste0("ResourceId.", 1:length(resource))
    query <- c(query, resource)
    query <- c(query, .makelist(tag, type = "Tag"))
    r <- ec2HTTP(query = query, ...)
    return(r$return[[1]])
}

#' @rdname tags
#' @export
delete_tags <- function(resource, tag, ...) {
    query <- list(Action = "DeleteTags")
    resource <- as.list(resource)
    names(resource) <- paste0("ResourceId.", 1:length(resource))
    query <- c(query, resource)
    if (!missing(tag)) {
        query <- c(query, .makelist(tag, type = "Tag"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r$return[[1]])
}

#' @rdname tags
#' @export
describe_tags <- function(filter, n, page, ...) {
    query <- list(Action = "DescribeTags")
    if (!missing(filter)) {
        filter <- as.list(filter)
        names(filter) <- paste0("Filter.", 1:length(filter))
        query <- c(query, filter)
    }
    if (!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if (!missing(token)) {
        query$NextToken <- token
    }
    r <- ec2HTTP(query = query, ...)
    return(r$tagSet)
}
