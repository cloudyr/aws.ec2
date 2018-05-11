#' @rdname vpcs
#' @title create_vpc
#' @description Create/delete VPCs
#' @template cidr
#' @param tenancy A character string specifying either \dQuote{default} or \dQuote{dedicated} (single-tenant) hardware for instances launched within the VPC.
#' @template vpc
#' @template dots
#' @return For `create_vpc`, a list of class \dQuote{ec2_vpc}. For `delete_vpc`, a logical indicating whether the operation succeeded.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateVpc.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteVpc.html>
#' @examples
#' \dontrun{
#' v <- create_vpc(cidr = "10.0.0.0/16")
#' describe_vpcs()
#' 
#' # delete VPC
#' delete_vpc(v)
#' }
#' @seealso [describe_vpcs()], [get_vpc_attr()]
#' @keywords security
#' @export
create_vpc <- function(cidr, tenancy = c("default", "dedicated"), ...) {
    query <- list(Action = "CreateVpc", CidrBlock = cidr)
    query$InstanceTenancy <- match.arg(tenancy)
    r <- ec2HTTP(query = query, ...)
    return(structure(flatten_list(r$vpc), class = "ec2_vpc"))
}

#' @rdname vpcs
#' @export
delete_vpc <- function(vpc, ...) {
    query <- list(Action = "DeleteVpc", VpcId = get_vpcid(vpc))
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @title Describe VPC(s)
#' @description List/describe VPC(s)
#' @template vpc
#' @template filter
#' @template dots
#' @return A list.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVpcs.html>
#' @examples
#' \dontrun{
#' describe_vpcs()
#' }
#' @seealso [create_vpc()], [get_vpc_attr()]
#' @keywords security
#' @export
describe_vpcs <- function(vpc, filter, ...) {
    query <- list(Action = "DescribeVpcs")
    if (!missing(vpc)) {
        if (inherits(vpc, "ec2_vpc")) {
            vpc <- list(get_vpcid(vpc))
        } else if (is.character(vpc)) {
            vpc <- as.list(get_vpcid(vpc))
        } else {
            vpc <- lapply(vpc, get_vpcid)
        }
        names(vpc) <- paste0("VpcId.", 1:length(vpc))
        query <- c(query, vpc)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$vpcSet, function(x) {
        structure(flatten_list(x), class = "ec2_vpc")
    })))
}

#' @rdname vpc_attrs
#' @title VPC Attributes
#' @description Get and set VPC attributes
#' @template vpc
#' @param dns A logical indicating whether DNS resolution is supported for the VPC. A request can only specify only `dns` xor `hostnames`.
#' @param hostnames A logical indicating whether instances launched in the VPC get DNS hostnames. A request can only specify only `dns` xor `hostnames`.
#' @param attribute A character string specifying one of \dQuote{enableDnsSupport} or \dQuote{enableDnsHostnames}.
#' @template dots
#' @return For `get_vpc_attr`, a logical indicating the value of the attribute. For `set_vpc_attr`, a logical indicating whether the operation succeeded.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVpcAttribute.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVpcAttribute.html>
#' @examples
#' \dontrun{
#' # create VPC
#' v <- create_vpc("10.0.0.0/16")
#' get_vpc_attr(v, "enableDnsSupport")
#' set_vpc_attr(v, dns = FALSE)
#' 
#' # cleanup
#' delete_vpc(v)
#' }
#' @seealso [describe_vpcs()], [create_vpc()]
#' @keywords security
#' @export
set_vpc_attr <- function(vpc, dns, hostnames, ...) {
    query <- list(Action = "ModifyVpcAttribute", 
                  VpcId = get_vpcid(vpc))
    if (!missing(dns)) {
        query$EnableDnsSupport.Value <- tolower(as.character(dns))
    } else if (!missing(hostnames)) {
        query$EnableDnsHostnames.Value <- tolower(as.character(hostnames))
    } else {
        stop("Must specify either 'dns' or 'hostnames'")
    }
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}

#' @rdname vpc_attrs
#' @export
get_vpc_attr <- function(vpc, attribute = c("enableDnsSupport", "enableDnsHostnames"), ...) {
    query <- list(Action = "DescribeVpcAttribute", VpcId = get_vpcid(vpc))
    query$Attribute <- attribute <- match.arg(attribute)
    r <- ec2HTTP(query = query, ...)
    return(as.logical(r[[attribute]][["value"]][[1]]))
}



print.ec2_vpc <- function(x, ...) {
    cat("vpcId:           ", x$vpcId, "\n")
    cat("state:           ", x$state, "\n")
    cat("cidrBlock:       ", x$cidrBlock, "\n")
    cat("dhcpOptionsId:   ", x$dhcpOptionsId, "\n")
    cat("instanceTenancy: ", x$instanceTenancy, "\n")
    cat("isDefault:       ", x$isDefault, "\n")
    cat("Tags:\n")
    if (is.null(x$tagSet)) {
        cat(" - NULL")
    } else {
        if (is.null(x$tagSet$item)) {
            ts <- x$tagSet
            cat(" -", ts$key, ":", ts$value, "\n")
        } else {
            lapply(x$tagSet, function(ts) {
                cat(" -", ts$key, ":", ts$value, "\n")
            })
        }
    }
    invisible(x)
}
