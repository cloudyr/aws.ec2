#' @rdname vpcs
#' @title create_vpc
#' @description Create/delete VPCs
#' @template cidr
#' @param tenancy A character string specifying either \dQuote{default} or \dQuote{dedicated} (single-tenant) hardware for instances launched within the VPC.
#' @template vpc
#' @template dots
#' @return For `create_vpc`, a list of class \dQuote{ec2_vpc}. For `delete_vpc`, a logical indicating whether the operation succeeded.
#' @references
#' <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html>
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
create_vpc <-
function(
  cidr,
  tenancy = c("default", "dedicated"),
  ...
) {
    query <- list(Action = "CreateVpc", CidrBlock = cidr)
    query$InstanceTenancy <- match.arg(tenancy)
    r <- ec2HTTP(query = query, ...)
    return(structure(flatten_list(r$vpc), class = "ec2_vpc"))
}

#' @rdname vpcs
#' @export
delete_vpc <-
function(
  vpc,
  ...
) {
    query <- list(Action = "DeleteVpc", VpcId = get_vpcid(vpc))
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
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
