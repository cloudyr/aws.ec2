#' @rdname gateway_create
#' @title Create/Delete Gateways
#' @description Create and delete Network Gateways for a VPC
#' @template gateway
#' @references
#' \url{http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateInternetGateway.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteInternetGateway.html}
#' @seealso \code{\link{create_gateway}}, \code{\link{describe_gateways}}
#' @export
create_gateway <- function(...) {
    query <- list(Action = "CreateInternetGateway")
    r <- ec2HTTP(query = query, ...)
    return(structure(flatten_list(r$internetGateway), class = "ec2_internet_gateway", requestId = r$requestId))
}

#' @rdname gateway_create
#' @export
delete_gateway <- function(gateway, ...) {
    query <- list(Action = "DeleteInternetGateway",
                  InternetGatewayId = gateway)
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname gateway_attach
#' @title Attach/detach Gateways
#' @description Attach or Detacth Network Gateways for a VPC
#' @template gateway
#' @template vpc
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AttachInternetGateway.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DetachInternetGateway.html}
#' @seealso \code{\link{create_gateway}}, \code{\link{describe_gateways}}
#' @export
attach_gateway <- function(gateway, vpc, ...) {
    query <- list(Action = "AttachInternetGateway",
                  InternetGatewayId = get_gatewayid(gateway),
                  VpcId = get_vpcid(vpc))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname gateway_attach
#' @export
detach_gateway <- function(gateway, vpc, ...) {
    query <- list(Action = "DetachInternetGateway",
                  InternetGatewayId = get_gatewayid(gateway),
                  VpcId = get_vpcid(vpc))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @title describe_gateways
#' @description Describe Network Gateways
#' @template gateway
#' @template filter
#' @template dots
#' @return A list.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInternetGateways.html}
#' @seealso \code{\link{create_gateway}}, \code{\link{attach_gateway}}
#' @export
describe_gateways <- function(gateway, filter, ...) {
    query <- list(Action = "DescribeInternetGateways")
    if (!missing(gateway)) {
        if (inherits(gateway, "ec2_internet_gateway")) {
            gateway <- list(get_instanceid(gateway))
        } else if (is.character(gateway)) {
            gateway <- as.list(get_gatewayid(gateway))
        } else {
            gateway <- lapply(gateway, get_gatewayid)
        }
        names(gateway) <- paste0("InternetGatewayId.", 1:length(gateway))
        query <- c(query, gateway)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(structure(r$customerGatewaySet, requestId = r$requestId[[1]]))
}


# utils

get_gatewayid <- function(x) {
    if (inherits(x, "ec2_internet_gateway")) {
        x$internetGatewayId
    } else {
        x
    }
}
