#' @rdname allocate_ip
#' @title Allocate/Release IP Addresses
#' @description Allocate or release VPC or standard IP Address
#' @details This function is used to allocate IP addresses either to EC2 classic (if it is available on your account; see \code{\link{account_attrs}}) or on a Virtual Private Cloud (VPC). The default for new AWS EC2 accounts is only to be able to create VPC configurations. Due to limitations in the IPv4 universe, users are typically restricted to 5 IP addresses, which can dynamically be allocated to instances via a VPC. Use \code{\link{associate_ip}}/\code{\link{disassociate_ip}} to link an IP address to a specific instance.
#' @param domain Optionally, a character string specifying \dQuote{vpc} or \dQuote{standard}.
#' @template ip 
#' @template dots
#' @return For \code{allocate_ip}, a list containing the IP address. For \code{release_ip}, a logical.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AllocateAddress.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReleaseAddress.html}
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
#' @seealso \code{\link{associate_ip}}, \code{\link{describe_ips}}, \code{\link{release_ip}}, \code{\link{make_ip_vpc}}/\code{\link{make_ip_classic}}
#' @export
allocate_ip <- function(domain = c("vpc", "standard"), ...) {
    query <- list(Action = "AllocateAddress")
    if (!missing(domain)) {
        domain <- match.arg(domain)
        query$Domain <- domain
    } 
    r <- ec2HTTP(query = query, ...)
    if (r$domain == "standard" ) {
        out <- list(publicIp = r$publicIp[[1]], domain = r$domain[[1]])
    } else if(r$domain == "vpc") {
        out <- list(publicIp = r$publicIp[[1]], allocationId = r$allocationId[[1]], domain = r$domain[[1]])
    } else {
        print("Unrecognized domain. NOT ALLOCATING ADDRESS")
        return(NULL)
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
#' @seealso \code{\link{allocate_ip}}, \code{\link{describe_ips}}, \code{\link{release_ip}}
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


#' @rdname convert_ip
#' @title IP conversion
#' @description Convert IP between Classic/EC2
#' @details IP addresses can only be used within EC2 classic or a Virtual Private Cloud. These functions allow you to convert an IP address between the two systems.
#' @template ip
#' @template dots
#' @return A list.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RestoreAddressToClassic.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RestoreAddressToClassic.html}
#' @export
make_ip_vpc <- function(ip, ...) {
    query <- list(Action = "MoveAddressToVpc")
    if (inherits(ip, "ec2_ip")) {
        query$PublicIp <- ip[["publicIp"]]
    } else if (is.list(ip)) {
        if ("publicIp" %in% names(ip)) {
            query$PublicIp <- ip[["publicIp"]]
        } else {
            stop("'ip' is not a recognized domain")
        }
    } else if (is.character(ip)) {
        if (grepl("\\.", ip)) {
            query$PublicIp <- ip
        }
    } else {
        stop("'ip' must be a publicIp, or an object of class 'ec2_ip'")
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname convert_ip
#' @export
make_ip_classic <- function(ip, ...) {
    query <- list(Action = "RestoreAddressToClassic")
    if (inherits(ip, "ec2_ip")) {
        query$PublicIp <- ip[["publicIp"]]
    } else if (is.list(ip)) {
        if ("publicIp" %in% names(ip)) {
            query$PublicIp <- ip[["publicIp"]]
        } else {
            stop("'ip' is not a recognized domain")
        }
    } else if (is.character(ip)) {
        if (grepl("\\.", ip)) {
            query$PublicIp <- ip
        }
    } else {
        stop("'ip' must be a publicIp, or an object of class 'ec2_ip'")
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
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

#' @rdname sg_ingress
#' @export
my_ip <- function() {
    readLines("http://checkip.amazonaws.com/")
}
