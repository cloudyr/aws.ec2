.makelist <- function(list, type = "Filter") {
    tmp <- as.list(c(names(list), list))
    names(tmp) <- c(paste0(type, ".", 1:length(list), ".Key"),
                    paste0(type, ".", 1:length(list), ".Value"))
    return(tmp)
}

flatten_list <- function(x) {
    if (is.list(x)) {
        if ((class(x) != "list") || (length(class(x)) > 1)) {
            return(x)
        } else {
            if (length(x) == 1) {
                return(flatten_list(x[[1]]))
            } else {
                return(lapply(x, flatten_list))
            }
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
    if (inherits(x, "ec2_instance")) {
        return(x$instanceId[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
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
