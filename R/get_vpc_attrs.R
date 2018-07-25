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
