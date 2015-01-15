create_security_group <- function(name, description, vpc, ...) {
    query <- list(Action = "CreateSecurityGroup", 
                  GroupName = name,
                  GroupDescription = description)
    if(!missing(vpc))
        query$VpcId <- vpc
    r <- ec2HTTP(query = query, ...)
    return(r)
}

delete_security_group <- function(name, id, ...) {
    query <- list(Action = "DeleteSecurityGroup")
    if(!missing(name))
        query$GroupName <- name
    else
        query$GroupId <- id
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_security_groups <- function(name, id, filter, ...) {
    query <- list(Action = "DescribeSecurityGroups")
    if(!missing(name)) {
        name <- as.list(name)
        names(name) <- paste0("GroupName.", 1:length(name))
        query <- c(query, name)
    } else {
        id <- as.list(id)
        names(id) <- paste0("GroupName.", 1:length(id))
        query <- c(query, id)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}


authorize_ingress <- function() {}

revoke_ingress <- function() {}

authorize_egress <- function() {}

revoke_egress <- function() {}

