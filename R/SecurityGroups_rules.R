#' @rdname sg_ingress
#' @title Security Group Ingress
#' @description Authorize/Revoke Security Group Ingress
#' @template sgroup
#' @param cidr A one- or two-element character vector specifying an IP or range of IP addresses. The default is your current machine's public IP as returned by <http://checkip.amazonaws.com/>.
#' @param port A one- or two-element integer vector, specifying a port or port range.
#' @param protocol A character string specifying a protocol. A value of \dQuote{-1} (the default) means all protocols.
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' s <- describe_subnets()
#' g <- create_sgroup("test_group", "new security group", vpc = s[[1]])
#' authorize_ingress(g, port = 80, protocol = "tcp")
#' 
#' # cleanup
#' revoke_ingress(g, port = 80, protocol = "tcp")
#' }
#' @keywords security
#' @seealso [create_sgroup()]
#' @export
authorize_ingress <- function(sgroup, cidr = paste0(my_ip(),"/32"), port, protocol = "-1", ...) {
    query <- list(Action = "AuthorizeSecurityGroupIngress")
    query$GroupId <- get_sgid(sgroup)
    if (!missing(cidr)) {
        query$IpPermissions.1.IpRanges.1.CidrIp <- head(cidr, 1)
        if (length(cidr) == 2) {
            query$IpPermissions.1.IpRanges.2.CidrIp <- tail(cidr, 1)        
        }
    }
    if (!missing(port)) {
        query$IpPermissions.1.FromPort <- head(port, 1)
        query$IpPermissions.1.ToPort <- tail(port, 1)
    }
    if (!missing(protocol)) {
        query$IpPermissions.1.IpProtocol <- protocol
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @importFrom utils head tail
#' @rdname sg_ingress
#' @export
revoke_ingress <- function(sgroup, cidr = paste0(my_ip(),"/32"), port, protocol = "-1", ...) {
    query <- list(Action = "RevokeSecurityGroupIngress")
    query$GroupId <- get_sgid(sgroup)
    if (!missing(cidr)) {
        query$IpPermissions.1.IpRanges.1.CidrIp <- head(cidr, 1)
        if (length(cidr) == 2) {
            query$IpPermissions.1.IpRanges.2.CidrIp <- tail(cidr, 1)        
        }
    }
    if (!missing(port)) {
        query$IpPermissions.1.FromPort <- head(port, 1)
        query$IpPermissions.1.ToPort <- tail(port, 1)
    }
    if (!missing(protocol)) {
        query$IpPermissions.1.IpProtocol <- protocol
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


authorize_egress <- function() {}

revoke_egress <- function() {}

