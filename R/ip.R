#' @rdname allocate_ip
#' @title Allocate/Release IP Addresses
#' @description Allocate or release VPC or standard IP Address
#' @details This function is used to allocate IP addresses either to EC2 classic (if it is available on your account; see [account_attrs()]) or on a Virtual Private Cloud (VPC). The default for new AWS EC2 accounts is only to be able to create VPC configurations. Due to limitations in the IPv4 universe, users are typically restricted to 5 IP addresses, which can dynamically be allocated to instances via a VPC. Use [associate_ip()]/[disassociate_ip()] to link an IP address to a specific instance.
#' @param domain Optionally, a character string specifying \dQuote{vpc} or \dQuote{standard}.
#' @template ip 
#' @template dots
#' @return For `allocate_ip`, a list containing the IP address. For `release_ip`, a logical.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AllocateAddress.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReleaseAddress.html>
#' @examples
#' \dontrun{
#' # create a classic/"standard" IP address
#' a1 <- allocate_ip("standard")
#' release_ip(a1$publicIp)
#'
#' # create a VPC IP address
#' a2 <- allocate_ip("vpc")
#' release_ip(a2$allocationId)
#' }
#' @seealso [associate_ip()], [describe_ips()], [release_ip()], [make_ip_vpc()]/[make_ip_classic()]
#' @export
allocate_ip <- function(domain = c("vpc", "standard"), ...) {
    query <- list(Action = "AllocateAddress")
    if (!is.null(domain)) {
        query$Domain <- match.arg(domain)
    } 
    r <- ec2HTTP(query = query, ...)
    if (r$domain == "standard" ) {
        out <- list(publicIp = r$publicIp[[1]], domain = r$domain[[1]])
    } else if(r$domain == "vpc") {
        out <- list(publicIp = r$publicIp[[1]], allocationId = r$allocationId[[1]], domain = r$domain[[1]])
    } else {
        stop("Unrecognized domain. Not allocating IP address.")
    }
    return(structure(out, 
                     class = "ec2_ip", 
                     requestId = r$requestId[[1]]))
    
}

#' @rdname allocate_ip
#' @export
release_ip <- function(ip, ...) {
    query <- list(Action = "ReleaseAddress")
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
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}


# utils
print.ec2_ip <- function(x, ...) {
    cat("publicIp:     ", x$publicIp, "\n")
    cat("domain:       ", x$domain, "\n")
    if (x$domain == "vpc") {
        cat("allocationId: ", x$allocationId, "\n")
    }
    invisible(x)
}
