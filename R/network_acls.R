# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkAcl.html}
# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteNetworkAcl.html}
create_netacl <- function(vpc, ...) {}
delete_netacl <- function(acl, ...) {}

# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkAclEntry.html}
# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReplaceNetworkAclEntry.html}
# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteNetworkAclEntry.html}
create_netacl_entry <- function(acl, ...) {}
replace_netacl_entry <- function(acl, ...) {}
delete_netacl_entry <- function(acl, rule, egress, ...) {}

# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReplaceNetworkAclAssociation.html}
associate_netacl <- function(association, acl, ...) {}


#' @title describe_network_acls
#' @description Describe Network ACL(s)
#' @template filter
#' @template dots
#' @return A list of objects of class \dQuote{ec2_network_acl}.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeNetworkAcls.html}
#' @seealso \code{\link{describe_subnets}}, \code{\link{describe_ips}}
#' @export
describe_netacls <- function(acl, filter, ...) {
    query <- list(Action = "DescribeNetworkAcls")
    if (!missing(acl)) {
        if (inherits(acl, "ec2_subnet")) {
            acl <- list(get_networkaclid(acl))
        } else if (is.character(acl)) {
            acl <- as.list(get_networkaclid(acl))
        } else {
            acl <- lapply(acl, get_networkaclid)
        }
        names(acl) <- paste0("NetworkAclId.", seq_along(acl))
        query <- c(query, acl)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$networkAclSet, function(x) {
        z <- flatten_list(x)
        z$entrySet <- unname(lapply(z$entrySet, `class<-`, "ec2_network_entry"))
        z$associationSet <- unname(lapply(z$associationSet, `class<-`, "ec2_network_association"))
        structure(z, class = "ec2_network_acl")
    })))
}


# utils

print.ec2_network_acl <- function(x, ...) {
    cat("networkAclId:     ", x$networkAclId, "\n")
    cat("vpcId:            ", x$vpcId, "\n")
    cat("default:          ", x$default, "\n")
    cat("Access Rules:     ", length(x$entrySet), "\n")
    cat("VCP Associations: ", length(x$associationSet), "\n")
    invisible(x)
}

get_networkaclid <- function(x) {
    if (inherits(x, "ec2_network_acl")) {
        return(x$networkAclId)
    } else {
        return(x)
    }
}
