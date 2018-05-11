#' @rdname acls
#' @title EC2 Network ACLs
#' @description Create/Delete Network ACL for VPC
#' @details Access Control Lists (ACLs) for a Virtual Private Cloud (VPC) provide a layer of security for an EC2 instance or set of instances. These can be used in tandem with or in lieu of Security Groups (see [describe_sgroups()]).
#' @template vpc
#' @template acl
#' @template dots
#' @return For `create_netacl`, a list of class \dQuote{ec2_acl}. For `delete_netacl`, a logical indicating whether the operation succeeded.
#' @references
#' <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Security.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkAcl.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteNetworkAcl.html>
#' @examples
#' \dontrun{
#' # create a VPC
#' v <- create_vpc(cidr = "10.0.0.0/16")
#' describe_vpcs()
#' 
#' # create a Network ACL for the VPC
#' acl <- create_netacl(v)
#'
#' # cleanup
#' delete_acl(acl)
#' delete_vpc(v)
#' }
#' @seealso [describe_netacls()]
#' @keywords security
#' @export
create_netacl <- function(vpc, ...) {
    query <- list(Action = "CreateNetworkAcl")
    query$VpcId <- get_vpcid(vpc)
    r <- ec2HTTP(query = query, ...)
    return(structure(flatten_list(r$networkAcl), class = "ec2_acl"))
}

#' @rdname acls
#' @export
delete_netacl <- function(acl, ...) {
    query <- list(Action = "DeleteNetworkAcl")
    query$NetworkAclId <- get_networkaclid(acl)
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname acl_rules
#' @title ACL Rules
#' @description Set/Replace/Delete ACL Rules
#' @details Network ACL rules control inbound and outbound traffic. An ACL is, by default, created without any rules. `create_netacl_rule` adds a new rule. Each Network ACL rule consists of a a unique identifying number, an access protocol, an action (\dQuote{allow} or \dQuote{deny}), a direction (ingress or egress), a CIDR block, and a port range. Rules cannot be modified, but they can be deleted and replaced. `replace_netacl_rule` replaces an existing rule. `delete_netacl_rule` deletes an existing rule.
#' @template acl
#' @param rule A positive integer between 1 and 32766 to identify the rule. AWS advises staggering rule numbers (e.g., by 10s).
#' @template cidr
#' @param port A one- or two-element integer vector, specifying a port or port range.
#' @param protocol A character string specifying a protocol. A value of \dQuote{-1} (the default) means all protocols.
#' @param action A character vector specifying one of \dQuote{allow} (the default) or \dQuote{deny}.
#' @param direction A character vector specifying one of \dQuote{ingress} (the default) or `egress`.
#' @template dots
#' @return A logical indicating whether the operation succeeded.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkAclEntry.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReplaceNetworkAclEntry.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteNetworkAclEntry.html>
#' @examples
#' \dontrun{
#' # create a VPC
#' v <- create_vpc(cidr = "10.0.0.0/16")
#' describe_vpcs()
#' 
#' # create a Network ACL for the VPC
#' acl <- create_netacl(v)
#' 
#' # add a rule
#' create_netacl_rule(acl, rule = 1000, cidr = "10.0.0.0/16", port = "80")
#' 
#' # retrieve the ACL (see rule is added)
#' describe_netacls(acl)
#' 
#' # replace the rule
#' replace_netacl_rule(acl, rule = 1000, cidr = "10.0.0.0/0", port = "32")
#' 
#' # cleanup
#' delete_netacl_rule(acl, 1000, protocol = "-1")
#' delete_netacl(acl)
#' delete_vpc(v)
#' }
#' @keywords security
#' @export
create_netacl_rule <- 
function(acl, rule, cidr, port, protocol = "-1", 
         action = c("allow", "deny"), 
         direction = c("ingress", "egress"), ...) {
    query <- list(Action = "CreateNetworkAclEntry")
    query$NetworkAclId <- get_networkaclid(acl)
    query$RuleNumber <- rule
    query$CidrBlock <- cidr
    query$PortRange.From <- head(port, 1)
    query$PortRange.To <- tail(port, 1)
    query$Protocol <- protocol
    query$RuleAction <- match.arg(action)
    direction <- match.arg(direction)
    if (direction == "ingress") {
        query$Egress <- FALSE
    } else {
        query$Egress <- TRUE
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname acl_rules
#' @export
replace_netacl_rule <- 
function(acl, rule, cidr, port, protocol = "-1", 
         action = c("allow", "deny"), 
         direction = c("ingress", "egress"), ...) {
    query <- list(Action = "ReplaceNetworkAclEntry")
    query$NetworkAclId <- get_networkaclid(acl)
    query$RuleNumber <- rule
    query$CidrBlock <- cidr
    query$PortRange.From <- head(port, 1)
    query$PortRange.To <- tail(port, 1)
    query$Protocol <- protocol
    query$RuleAction <- match.arg(action)
    direction <- match.arg(direction)
    if (direction == "ingress") {
        query$Egress <- FALSE
    } else {
        query$Egress <- TRUE
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname acl_rules
#' @export
delete_netacl_rule <- function(acl, rule, protocol = "-1", direction = c("ingress", "egress"), ...) {
    query <- list(Action = "DeleteNetworkAclEntry")
    query$NetworkAclId <- get_networkaclid(acl)
    query$RuleNumber <- rule
    direction <- match.arg(direction)
    if (direction == "ingress") {
        query$Egress <- FALSE
    } else {
        query$Egress <- TRUE
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}


#' @title Associate ACL w/Subnet
#' @description Associate a Network ACL with a Subnet
#' @details This function modifies the ACL associated with a Subnet. By default when you create a Subnet, it is automatically associated with the default network ACL. This can be used to change that association.
#' @template acl
#' @param association A character string specifying a Network ACL Association, or an object of class \dQuote{ec2_network_association} (possibly returned by [describe_netacls()]). This should be the association that you want to replace.
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ReplaceNetworkAclAssociation.html>
#' @keywords security
#' @seealso [create_netacl()], [create_subnet()]
#' @export
associate_netacl <- function(acl, association, ...) {
    query <- list(Action = "ReplaceNetworkAclAssociation")
    query$NetworkAclId <- get_networkaclid(acl)
    query$AssociationId <- get_associd(association)
    r <- ec2HTTP(query = query, ...)
    return(r)
}


#' @title describe_network_acls
#' @description Describe Network ACL(s)
#' @details Access Control Lists (ACLs) for a Virtual Private Cloud (VPC) provide a layer of security for an EC2 instance or set of instances. These can be used in tandem with or in lieu of Security Groups (see [describe_sgroups()]).
#' @template acl
#' @template filter
#' @template dots
#' @return A list of objects of class \dQuote{ec2_network_acl}.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeNetworkAcls.html>
#' @seealso [describe_subnets()], [describe_ips()]
#' @keywords security
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

print.ec2_network_acl <- function(x, ...) {
    cat("networkAclId:     ", x$networkAclId, "\n")
    cat("vpcId:            ", x$vpcId, "\n")
    cat("default:          ", x$default, "\n")
    cat("Access Rules:     ", length(x$entrySet), "\n")
    cat("VCP Associations: ", length(x$associationSet), "\n")
    cat("Tags:", if (length(x$tagSet)) paste(x$tagSet, collapse = ", ") else "<NA>", "\n")
    invisible(x)
}
