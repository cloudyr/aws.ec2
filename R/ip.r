allocate_ip <- function(domain, ...) {
    query <- list(Action = "AllocateAddress")
    if(!missing(domain)) {
        vdomain <- c("vpc", "standard")
        if(domain %in% vdomain)
            stop("'domain' must be one of: ", paste0(vdomain, collapse = ", "))
        query$Domain <- domain
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

release_ip <- function(allocation, public, ...) {
    query <- list(Action = "ReleaseAddress")
    if(!missing(allocation))
        query$AllocationId <- allocation
    if(!missing(public))
        query$PublicIp <- public
    r <- ec2HTTP(query = query, ...)
    return(r)
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

associate_ip <- 
function(instance, 
         allocation, 
         public, 
         private, 
         netinterface, 
         allow,
         ...) {
    query <- list(Action = "AssociateAddress", InstanceId = instance)
    if(!missing(allocation))
        query$AllocationId <- allocation
    if(!missing(public))
        query$PublicIp <- public
    if(!missing(public))
        query$PrivateIpAddress <- private
    if(!missing(netinterface))
        query$NetworkInterfaceId <- netinterface
    if(!missing(allow))
        query$AllowReassociation <- tolower(as.character(allow))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

disassociate_ip <- function(allocation, public, ...) {
    query <- list(Action = "DisassociateAddress")
    if(!missing(allocation))
        query$AllocationId <- allocation
    if(!missing(public))
        query$PublicIp <- public
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_ip <- function(allocation, public, filter, ...) {
    if(!missing(allocation)) {
        allocation <- as.list(allocation)
        names(allocation) <- paste0("AllocationId.", 1:length(allocation))
        query <- c(query, allocation)
    }
    if(!missing(public)) {
        public <- as.list(public)
        names(public) <- paste0("PublicIp.", 1:length(public))
        query <- c(query, public)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
