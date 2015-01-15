create_gateway <- function(asn, ip, type = "ipsec.1", ...) {
    query <- list(Action = "CreateCustomerGateway",
                  BgpAsn = asn,
                  IpAddress = ip,
                  type = type)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

delete_gateway <- function(gateway, ...) {
    query <- list(Action = "DeleteCustomerGateway",
                  CustomerGatewayId = gateway)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_gateways <- function(gateway, filter, ...) {
    query <- list(Action = "DescribeCustomerGateway")
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if(!missing(gateway)) {
        gateway <- as.list(gateway)
        names(gateway) <- paste0("CustomerGatewayId.", 1:length(gateway))
        query <- c(query, gateway)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
