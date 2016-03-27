#' @rdname tags
#' @title Tags
#' @description Tag an EC2 Resource
#' @param resource A character vector specifying one or more resource IDs, typically EC2 instance IDs.
#' @param tag A named character string of key-value pairs of tag names and their corresponding values. For \code{delete_tags}, the value can be an empty string (in which case, the tag is delete regardless of value) or a specific value (in which case the tag is only deleted if it matches the value).
#' @template filter
#' @param n \dots
#' @param page \dots
#' @template dots
#' @return For \code{create_tags} and \code{delete_tags}, a logical. Otherwise, a list of tags.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTags.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTags.html}
#' @examples
#' \dontrun{
#' create_tags("i-b79cfd34", list(foo = "bar"))
#' describe_tags()
#' delete_tags("i-b79cfd34", list("foo" = "notbar"))
#' delete_tags("i-b79cfd34", list("foo" = "bar"))
#' }
#' @seealso \code{\link{describe_instances}}, \code{\link{get_instance_attr}}, \code{\link{associate_ip}}
#' @export
create_tags <- function(resource, tag, ...) {
    query <- list(Action = "CreateTags")
    resource <- as.list(resource)
    names(resource) <- paste0("ResourceId.", 1:length(resource))
    query <- c(query, resource)
    query <- c(query, .makelist(tag, type = "Tag"))
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
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
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname tags
#' @export
describe_tags <- function(filter, n, page, ...) {
    query <- list(Action = "DescribeTags")
    if (!missing(filter)) {
        filter <- as.list(filter)
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if (!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if (!missing(page)) {
        query$NextToken <- page
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$tagSet, function(z) {
        structure(flatten_list(z), class = "ec2_tag")
    })))
}
