#' @title Describe IP(s)
#' @description Get information about one or more IP addresses.
#' @template ip 
#' @template filter
#' @template dots
#' @return A list
#' @export
describe_ips <- function(ip, filter, ...) {
    query <- list(Action = "DescribeAddresses")
    if (!missing(ip)) {
        if (inherits(ip, "ec2_ip")) {
            if (ip$domain == "vpc") {
                query$AllocationId <- ip$allocationId
            } else if (ip$domain == "standard") {
                query$PublicIp <- ip$publicIp
            } else {
                stop("'ip' is not a recognized domain")
            }
        } else if (is.list(ip)) {
            if ("allocationId" %in% names(ip)) {
                query$AllocationId <- ip$allocationId
            } else if ("publicIp" %in% names(ip)) {
                query$PublicIp <- ip$publicIp
            } else {
                stop("'ip' is not a recognized domain")
            }
        } else if (is.character(ip)) {
            if (!grepl("\\.", ip)) {
                query$AllocationId <- ip
            } else {
                query$PublicIp <- ip
            }
        } else {
            stop("'ip' must be an allocationId, a publicIp, or an object of class 'ec2_ip'")
        }
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$addressesSet, function(z) {
        structure(flatten_list(z), class = "ec2_ip")
    })))
}
