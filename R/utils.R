
# The date and time at which the signature included in the request expires, in the format YYYY-MM-DDThh:mm:ssZ
.expires <- function(time){
    format.POSIXct(time, format = "%FT%TZ", tz = "UTC")
}

.tag_image <- function(tags = list()){
    
}

.tag_specification <- function(resource_type = "instance", tags = list()){
    z <- c(
        list(ResourceType = resource_type),
        .makelist(tags, type = "Tag")
    )
    names(z) <- paste0("TagSpecification.", 1, ".", names(z))
    z
}



.makelist <- function(list, type = "Filter") {
    .name <- switch(type,
                    Filter = ".Name",
                    Tag = ".Key",
                    ".Key"
                    )
    tmp <- as.list(c(names(list), list))
    names(tmp) <- c(paste0(type, ".", seq_along(list), .name),
                    paste0(type, ".", seq_along(list), ".Value.1"))
    return(tmp)
}

flatten_list <- function(x) {
    if (is.list(x)) {
        if ((class(x) != "list") || (length(class(x)) > 1)) {
            return(x)
        } else {
            if (length(x) == 0) return("")
            if (length(x) == 1) return(flatten_list(x[[1]]))
            return(lapply(x, flatten_list))
        }
    } else {
        return(x)
    }
}

# Get ID functions
get_subnetid <- function(x) {
    if (is.character(x)) {
        return(x)
    } else if (inherits(x, "ec2_subnet")) {
        return(x$subnetId[[1]])
    }
}

get_associd <- function(x) {
    if (inherits(x, "ec2_network_association")) {
        return(x$networkAclAssociationId)
    } else {
        return(x)
    }
}

get_vpcid <- function(x) {
    if (inherits(x, "ec2_vpc") || inherits(x, "ec2_subnet")) {
        return(x$vpcId)
    } else {
        return(x)
    }
}

get_sgid <- function(x) {
    if (inherits(x, "ec2_security_group")) {
        return(x$groupId[[1]])
    } else if (is.character(x)) {
        return(x)
    }     
}

get_sgname <- function(x) {
    if (inherits(x, "ec2_security_group")) {
        return(x$groupName[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
}

get_pgname <- function(x) {
    if (inherits(x, "ec2_placement_group")) {
        return(x$groupName[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
}

get_keypairname <- function(x) {
    if (is.character(x)) {
        return(x)
    } else if (inherits(x, "ec2_keypair")) {
        return(x$keyName[[1]])
    }
}

get_instanceid <- function(x) {
    if (is.character(x)) {
        return(x)
    }
    if (inherits(x, "ec2_instance")) {
        return(x$instanceId[[1]])
    } 
    if (inherits(x, "ec2_reservation")) {
        return(vapply(x$instancesSet, get_instanceid, FUN.VALUE = character(1), USE.NAMES = FALSE))
    } 
    if (inherits(x, "ec2_reservation_set")) {
        return(vapply(x, get_instanceid, FUN.VALUE = character(1), USE.NAMES = FALSE))
    }
    if (is.list(x)) {
        return(vapply(x, get_instanceid, FUN.VALUE = character(1), USE.NAMES = FALSE))
    }
    NA
}

get_imageid <- function(x) {
    if (inherits(x, "ec2_image")) {
        return(x$imageId[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
}

get_gatewayid <- function(x) {
    if (inherits(x, "ec2_internet_gateway")) {
        return(x$internetGatewayId)
    } else {
        return(x)
    }
}

get_networkaclid <- function(x) {
    if (inherits(x, "ec2_network_acl")) {
        return(x$networkAclId[[1]])
    } else {
        return(x)
    }
}

get_snapshotid <- function(x) {
    if (inherits(x, "ec2_ebs_snapshot")) {
        return(x$snapshotId)
    } else {
        return(x)
    }
}

get_volumeid <- function(x) {
    if (inherits(x, "ec2_ebs_volume")) {
        return(x$volumeId)
    } else {
        return(x)
    }
}
