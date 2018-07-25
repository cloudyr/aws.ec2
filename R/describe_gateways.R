#' @title describe_gateways
#' @description Describe Network Gateways
#' @template gateway
#' @template filter
#' @template dots
#' @return A list.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInternetGateways.html>
#' @seealso [create_gateway()], [attach_gateway()]
#' @keywords security
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
