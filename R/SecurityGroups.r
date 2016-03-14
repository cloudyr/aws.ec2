#' @rdname security_groups
#' @title Security Groups
#' @description Describe, create, and delete Security Groups
#' @param name A character string (max 255 characters) specifying a security group name.
#' @param id A character string specifying a security group ID.
#' @param description A character string specifying a security group description.
#' @param vpc A character string specifying a VPC Id (required for a VPC).
#' @template filter
#' @template dots
#' @return For \code{describe_sgroups}, a list of objects of class \dQuote{ec2_security_group}.
#' @export
describe_sgroups <- function(name, id, filter, ...) {
    query <- list(Action = "DescribeSecurityGroups")
    if (!missing(name)) {
        name <- as.list(name)
        names(name) <- paste0("GroupName.", 1:length(name))
        query <- c(query, name)
    }
    if (!missing(id)) {
        id <- as.list(id)
        names(id) <- paste0("GroupId.", 1:length(id))
        query <- c(query, id)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$securityGroupInfo, `class<-`, "ec2_security_group"))
}

#' @rdname security_groups
#' @export
create_sgroup <- function(name, description, vpc, ...) {
    query <- list(Action = "CreateSecurityGroup", 
                  GroupName = name,
                  GroupDescription = description)
    if(!missing(vpc))
        query$VpcId <- vpc
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname security_groups
#' @export
delete_sgroup <- function(name, id, ...) {
    query <- list(Action = "DeleteSecurityGroup")
    if(!missing(name))
        query$GroupName <- name
    else
        query$GroupId <- id
    r <- ec2HTTP(query = query, ...)
    return(r)
}

authorize_ingress <- function() {}

revoke_ingress <- function() {}

authorize_egress <- function() {}

revoke_egress <- function() {}

