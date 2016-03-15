#' @rdname subnets
#' @title Subnets
#' @description Get, create, and delete subnets
#' @template subnet
#' @template filter
#' @param vpc \dots
#' @param cidr \dots
#' @param zone \dots
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' get_image_attr("ami-7f9dc615", "description")
#' }
#' @export
describe_subnets <- function(subnet, filter, ...) {
    query <- list(Action = "DescribeSubnets")
    if (!missing(subnet)) {
        if (inherits(subnet, "ec2_keypair")) {
            subnet <- list(get_subnetid(subnet))
        } else if (is.character(subnet)) {
            subnet <- as.list(get_subnetid(subnet))
        } else {
            subnet <- lapply(subnet, get_subnetid)
        }
        names(subnet) <- paste0("SubnetId.", seq_along(subnet))
        query <- c(query, subnet)
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$subnetSet, `class<-`, "ec2_subnet"))
}

#' @rdname subnets
#' @export
create_subnet <- function(vpc, cidr, zone, ...) {
    query <- list(Action = "CreateSubnet", CidrBlock = cidr)
    query$VpcId <- get_vpcid(vpc)
    if (!missing(zone)) {
        query$AvailabilityZone <- zone
    }
    r <- ec2HTTP(query = query, ...)
    return(structure(r, class = "ec2_subnet"))
}

#' @rdname subnets
#' @export
delete_subnet <- function(subnet, ...) {
    query <- list(Action = "DeleteSubnet")
    query$SubnetId <- get_subnetid(subnet)
    r <- ec2HTTP(query = query, ...)
    return(r$return)
}

get_subnetid <- function(x) {
    if (is.character(x)) {
        return(x)
    } else if (inherits(x, "ec2_subnet")) {
        return(x$subnetId[[1]])
    }
}

# set_subnet_attr

get_vpcid <- function(x) {
    x
}
