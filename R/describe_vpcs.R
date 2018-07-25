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
