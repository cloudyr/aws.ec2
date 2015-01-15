account_attrs <- function(attribute, ...) {
    query <- list(Action = "DescribeAccountAttributes")
    if(!missing(attribute)) {
        attribute <- as.list(attribute)
        names(attribute) <- paste0("AttributeName.", 1:length(attribute))
        query <- c(query, attribute)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_zones <- function(zone, filter, ...) {
    query <- list(Action = "DescribeAvailabilityZones")
    if(!missing(zone)) {
        zone <- as.list(zone)
        names(zone) <- paste0("ZoneName.", 1:length(zone))
        query <- c(query, zone)
    }
    if(!missing(filter)) {
        vfilter <- c("message", "region-name", "state", "zone-name")
        if(any(!names(filter) %in% vfilter))
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_regions <- function(region, filter, ...) {
    query <- list(Action = "DescribeRegions")
    if(!missing(region)) {
        region <- as.list(region)
        names(region) <- paste0("RegionName.", 1:length(region))
        query <- c(query, region)
    }
    if(!missing(filter)) {
        vfilter <- c("endpoint", "region-name")
        if(any(!names(filter) %in% vfilter))
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
