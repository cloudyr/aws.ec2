#' @rdname gateway_attach
#' @title Attach/detach Gateways
#' @description Attach or Detacth Network Gateways for a VPC
#' @template gateway
#' @template vpc
#' @template dots
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AttachInternetGateway.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DetachInternetGateway.html>
#' @seealso [create_gateway()], [describe_gateways()]
#' @keywords security
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
