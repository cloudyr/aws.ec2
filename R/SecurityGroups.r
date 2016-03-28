#' @rdname security_groups
#' @title Security Groups
#' @description Describe, create, and delete Security Groups
#' @details Security groups provide a layer of security for a Virtual Private Cloud (VPC) for an EC2 instance or set of instances. These can be used in tandem with or in lieu of network Access Control Lists (ACLs) (see \code{\link{describe_network_acls}}). Any given instance can be in multiple security groups, which can be confusing.
#' @template sgroup
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
#' # create a generic security group
#' sg1 <- create_sgroup("test_group", "example security group")
#' delete_sgroup(sg1)
#'
#' # create a security group within a VPC
#' ## setup the VPC
#' vpc <- allocate_ip("vpc")
#' vpc <- describe_ips(vpc)[[1]]
#' sg2 <- create_sgroup("test_group2", "new security group", vpc = vpc)
#' }
#' @seealso \code{\link{authorize_ingress}}
#' @keywords security
#' @export
describe_sgroups <- function(sgroup, name, filter, ...) {
    query <- list(Action = "DescribeSecurityGroups")
    if (!missing(sgroup)) {
        if (inherits(sgroup, "ec2_security_group")) {
            sgroup <- list(get_sgid(sgroup))
        } else if (is.character(sgroup)) {
            sgroup <- as.list(get_sgid(sgroup))
        } else {
            sgroup <- lapply(sgroup, get_sgid)
        }
        names(sgroup) <- paste0("GroupId.", 1:length(sgroup))
        query <- c(query, sgroup)
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
    out <- list(groupId = r$groupId[[1]], groupName = name, groupDescription = description)
    return(structure(out, class = "ec2_security_group"))
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

print.ec2_security_group <- function(x, ...) {
    cat("ownerId:          ", x$ownerId, "\n")
    cat("groupId:          ", x$groupId, "\n")
    cat("groupName:        ", x$groupName, "\n")
    cat("groupDescription: ", x$groupDescription, "\n")
    cat("vpcId:            ", x$vpcId , "\n")
    cat("ipPermissions:       ", length(x$ipPermissions), "\n")
    cat("ipPermissionsEgress: ", length(x$ipPermissionsEgress), "\n")
    invisible(x)
}
