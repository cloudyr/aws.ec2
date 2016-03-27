#' @rdname security_groups
#' @title Security Groups
#' @description Describe, create, and delete Security Groups
#' @param id A character string specifying a security group ID.
#' @param name A character string (max 255 characters) specifying a security group name.
#' @param description A character string specifying a security group description.
#' @param vpc A character string specifying a VPC Id (required for a VPC).
#' @template filter
#' @template dots
#' @return For \code{describe_sgroups} and \code{create_sgroup}, a list of objects of class \dQuote{ec2_security_group}. For \code{delete_sgroup}, a logical.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSecurityGroups.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteSecurityGroup.html}
#' @examples
#' \dontrun{
#' describe_sgroups()
#' sg <- create_sgroup("test_group", "example security group")
#' delete_sgroup(sg)
#' }
#' @export
describe_sgroups <- function(id, name, filter, ...) {
    query <- list(Action = "DescribeSecurityGroups")
    if (!missing(id)) {
        if (inherits(id, "ec2_security_group")) {
            id <- list(get_sgid(id))
        } else if (is.character(id)) {
            id <- as.list(get_sgid(id))
        } else {
            id <- lapply(id, get_sgid)
        }
        names(id) <- paste0("GroupId.", 1:length(id))
        query <- c(query, id)
    }
    if (!missing(name)) {
        if (inherits(name, "ec2_security_group")) {
            name <- list(get_sgname(name))
        } else if (is.character(name)) {
            name <- as.list(get_sgname(name))
        } else {
            name <- lapply(name, get_sgname)
        }
        names(name) <- paste0("GroupName.", 1:length(name))
        query <- c(query, name)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$securityGroupInfo, function(z) {
        structure(flatten_list(z), class = "ec2_security_group")
    })))
}

#' @rdname security_groups
#' @export
create_sgroup <- function(name, description, vpc, ...) {
    query <- list(Action = "CreateSecurityGroup", 
                  GroupName = name,
                  GroupDescription = description)
    if (!missing(vpc)) {
        query$VpcId <- get_vpcid(vpc)
    }
    r <- ec2HTTP(query = query, ...)
    return(structure(list(r$groupId), class = "ec2_security_group"))
}

#' @rdname security_groups
#' @export
delete_sgroup <- function(name, id, ...) {
    query <- list(Action = "DeleteSecurityGroup")
    if (!missing(id)) {
        if (inherits(id, "ec2_security_group")) {
            id <- list(get_sgid(id))
        } else if (is.character(id)) {
            id <- as.list(get_sgid(id))
        } else {
            id <- lapply(id, get_sgid)
        }
        names(id) <- paste0("GroupId.", 1:length(id))
        query <- c(query, id)
    }
    if (!missing(name)) {
        if (inherits(name, "ec2_security_group")) {
            name <- list(get_sgname(name))
        } else if (is.character(name)) {
            name <- as.list(get_sgname(name))
        } else {
            name <- lapply(name, get_sgname)
        }
        names(name) <- paste0("GroupName.", 1:length(name))
        query <- c(query, name)
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

authorize_ingress <- function() {}

revoke_ingress <- function() {}

authorize_egress <- function() {}

revoke_egress <- function() {}


print.ec2_security_group <- function(x, ...) {
    cat("ownerId:          ", x$ownerId, "\n")
    cat("groupId:          ", x$groupId, "\n")
    cat("groupName:        ", x$groupName, "\n")
    cat("groupDescription: ", x$groupDescription, "\n")
    cat("vpcId:            ", x$vpcId , "\n")
    cat("ipPermissions:    ", length(x$ipPermissions), "\n")
    invisible(x)
}


# utils
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
