#' @rdname allocate_ip
#' @title Allocate/Release IP Address
#' @description Allocate or release VPC or standard IP Address
#' @param domain Optionally, a character string specifying \dQuote{vpc} or \dQuote{standard}.
#' @template allocation
#' @param public This is the public IP address for a \dQuote{classic} EC2 instance. For VPC, use \code{allocation} instead.
#' @template dots
#' @return For \code{allocate_ip}, a list containing the IP address. For \code{release_ip}, a logical.
#' @seealso \code{\link{associate_ip}}
#' @export
allocate_ip <- function(domain, ...) {
    query <- list(Action = "AllocateAddress")
    if (!missing(domain)) {
        vdomain <- c("vpc", "standard")
        if (!domain %in% vdomain) {
            stop("'domain' must be one of: ", paste0(vdomain, collapse = ", "))
        }
        query$Domain <- domain
    }
    r <- ec2HTTP(query = query, ...)
    return(structure(r, class = "ec2_ip"))
}

#' @rdname allocate_ip
release_ip <- function(allocation, public, ...) {
    query <- list(Action = "ReleaseAddress")
    if (!missing(allocation)) {
        query$AllocationId <- allocation
    }
    if (!missing(public)) {
        query$PublicIp <- public
    }
    r <- ec2HTTP(query = query, ...)
    return(r$return)
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
#' @param allocation \dots
#' @param public \dots
#' @param private \dots
#' @param netinterface \dots
#' @param allow \dots
#' @template dots
#' @return A list.
#' @export
associate_ip <- 
function(instance, 
         allocation, 
         public, 
         private, 
         netinterface, 
         allow,
         ...) {
    query <- list(Action = "AssociateAddress", InstanceId = get_instanceid(instance))
    if (!missing(allocation)) {
        query$AllocationId <- allocation
    }
    if (!missing(public)) {
        query$PublicIp <- public
    }
    if (!missing(public)) {
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
disassociate_ip <- function(allocation, public, ...) {
    query <- list(Action = "DisassociateAddress")
    if (!missing(allocation)) {
        query$AllocationId <- allocation
    }
    if (!missing(public)) {
        query$PublicIp <- public
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @title Describe IP
#' @description Get IP information
#' @template allocation
#' @param public \dots
#' @template filter
#' @template dots
#' @return A list
#' @export
describe_ip <- function(allocation, public, filter, ...) {
    query <- list(Action = "DescribeAddresses")
    if (!missing(allocation)) {
        allocation <- as.list(allocation)
        names(allocation) <- paste0("AllocationId.", 1:length(allocation))
        query <- c(query, allocation)
    }
    if (!missing(public)) {
        public <- as.list(public)
        names(public) <- paste0("PublicIp.", 1:length(public))
        query <- c(query, public)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$addressesSet, `class<-`, "ec2_ip"))
}
