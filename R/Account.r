#' @title Account Attributes
#' @description Retrieve Account Attributes
#' @param attribute Optionally a character string specifying one or more of: \dQuote{supported-platforms}, \dQuote{vpc-max-security-groups-per-interface}, \dQuote{max-elastic-ips}, \dQuote{max-instances}, \dQuote{vpc-max-elastic-ips}, \dQuote{default-vpc}. If missing, all are returned.
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @export
account_attrs <- function(attribute, ...) {
    query <- list(Action = "DescribeAccountAttributes")
    if (!missing(attribute)) {
        attribute <- as.list(attribute)
        names(attribute) <- paste0("AttributeName.", 1:length(attribute))
        query <- c(query, attribute)
    }
    r <- ec2HTTP(query = query, ...)
    out <- lapply(r$accountAttributeSet, function(z) {
        unname(unlist(z$attributeValueSet))
    })
    names(out) <- lapply(r$accountAttributeSet, function(z) z[["attributeName"]][[1]])
    return(out)
}

#' @title Describe Zones
#' @description Retrieve Zone Details
#' @param zone Optionally a character string specifying the name of one or more AWS zones. If missing, details about all are returned.
#' @param filter \dots
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
describe_zones <- function(zone, filter, ...) {
    query <- list(Action = "DescribeAvailabilityZones")
    if (!missing(zone)) {
        zone <- as.list(zone)
        names(zone) <- paste0("ZoneName.", 1:length(zone))
        query <- c(query, zone)
    }
    if (!missing(filter)) {
        vfilter <- c("message", "region-name", "state", "zone-name")
        if (any(!names(filter) %in% vfilter)) {
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        }
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$availabilityZoneInfo, `class<-`, "ec2_zone"))
}

#' @title Describe Regions
#' @description Retrieve Region Details
#' @param region Optionally a character string specifying the name of one or more AWS regions. If missing, details about all are returned.
#' @param filter \dots
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
describe_regions <- function(region, filter, ...) {
    query <- list(Action = "DescribeRegions")
    if (!missing(region)) {
        region <- as.list(region)
        names(region) <- paste0("RegionName.", 1:length(region))
        query <- c(query, region)
    }
    if (!missing(filter)) {
        vfilter <- c("endpoint", "region-name")
        if (any(!names(filter) %in% vfilter)) {
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        }
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$regionInfo, `class<-`, "ec2_region"))
}
