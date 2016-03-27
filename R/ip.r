#' @rdname allocate_ip
#' @title Allocate/Release IP Address
#' @description Allocate or release VPC or standard IP Address
#' @param domain Optionally, a character string specifying \dQuote{vpc} or \dQuote{standard}.
#' @template ip 
#' @template dots
#' @return For \code{allocate_ip}, a list containing the IP address. For \code{release_ip}, a logical.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AllocateAddress.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReleaseAddress.html}
#' @examples
#' \dontrun{
#' a <- allocate_ip("standard")
#' release_ip(public = a$publicIp)
#' }
#' @seealso \code{\link{associate_ip}}, \code{\link{describe_ip}}, \code{\link{release_ip}}
#' @export
allocate_ip <- function(domain = c("vpc", "standard"), ...) {
    query <- list(Action = "AllocateAddress")
    if (!missing(domain)) {
        domain <- match.arg(domain)
        query$Domain <- domain
    }
    r <- ec2HTTP(query = query, ...)
    if ("publicIp" %in% names(r)) {
        out <- list(publicIp = r$publicIp[[1]], domain = r$domain[[1]])
    } else {
        out <- list(allocationId = r$allocationId[[1]], domain = r$domain[[1]])
    }
    return(structure(out, 
                     class = "ec2_ip", 
                     requestId = r$requestId[[1]]))
    
}

#' @rdname allocate_ip
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

assign_private_ip <- function(netinterface, n, private, allow, ...) {
    query <- list(Action = "ReleaseAddress", NetworkInterfaceId = netinterface)
    if(!missing(n))
        query$SecondaryPrivateIpAddressCount <- n
    else {
        private <- as.list(private)
        names(private) <- paste0("PrivateIpAddress.", 1:length(private))
        query <- c(query, private)
    }
    if(!missing(allow))
        query$AllowReassociation <- tolower(as.character(allow))
    r <- ec2HTTP(query = query, ...)
    return(r)

}

#' @rdname associate_ip
#' @title (Dis)Associate IP
#' @description Associate/Disassociate IP with Instance
#' @template instance
#' @template ip 
#' @param private For a VPC \code{ip}, the primary or secondary private IP address to associate with the Elastic IP address. If no private IP address is specified, the Elastic IP address is associated with the primary private IP address.
#' @param netinterface For a VCP \code{ip}, the ID of the network interface. If the instance has more than one network interface, you must specify a network interface ID.
#' @param allow \dots
#' @template dots
#' @return A list.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AssociateAddress.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DisassociateAddress.html}
#' @seealso \code{\link{allocate_ip}}, \code{\link{describe_ip}}, \code{\link{release_ip}}
#' @export
associate_ip <- 
function(instance, 
         ip, 
         private, 
         netinterface, 
         allow,
         ...) {
    query <- list(Action = "AssociateAddress", InstanceId = get_instanceid(instance))
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
    
    if (!missing(private)) {
        query$PrivateIpAddress <- private
    }
    if (!missing(netinterface)) {
        query$NetworkInterfaceId <- netinterface
    }
    if (!missing(allow)) {
        query$AllowReassociation <- tolower(as.character(allow))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname associate_ip
#' @export
disassociate_ip <- function(ip, ...) {
    query <- list(Action = "DisassociateAddress")
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
    return(r)
}

#' @title Describe IP
#' @description Get IP information
#' @template ip 
#' @template filter
#' @template dots
#' @return A list
#' @export
describe_ip <- function(ip, filter, ...) {
    query <- list(Action = "DescribeAddresses")
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
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$addressesSet, function(z) {
        structure(flatten_list(z), class = "ec2_ip")
    })))
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
