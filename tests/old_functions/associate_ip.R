#' @rdname associate_ip
#' @title (Dis)Associate IP
#' @description Associate/Disassociate IP with Instance
#' @template instance
#' @template ip
#' @param private For a VPC \code{ip}, the primary or secondary private IP
#'   address to associate with the Elastic IP address. If no private IP address
#'   is specified, the Elastic IP address is associated with the primary private
#'   IP address.
#' @param netinterface For a VCP \code{ip}, the ID of the network interface. If
#'   the instance has more than one network interface, you must specify a
#'   network interface ID.
#' @param allow \dots
#' @template dots
#' @return A list.
#' @references
#' \href{https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html}{Networking:
#' Elastic IP addresses}
#' \href{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AssociateAddress.html}{API
#' Reference: AssociateAddress}
#' \href{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DisassociateAddress.html}{API
#' Reference: DisassociateAddress}
#' @seealso \code{\link{allocate_ip}}, \code{\link{describe_ips}},
#'   \code{\link{release_ip}}
#' @export
associate_ip <- function(instance,
                         ip,
                         private = NULL,
                         netinterface = NULL,
                         allow = FALSE,
                         ...) {
  query <- list(
    Action = "AssociateAddress", 
    InstanceId = get_instanceid(instance)
  )
  if (inherits(ip, "ec2_ip")) {
    if (ip$domain == "vpc") {
      query$AllocationId <- ip$allocationId
    } else if (ip$domain == "standard") {
      query$PublicIp <- ip$publicIp
    } else {
      stop("'ip' is not a recognized domain")
    }
  } else if (is.list(ip)) {
    if ("allocationId" %in% names(ip)) {
      query$AllocationId <- ip$allocationId
    } else if ("publicIp" %in% names(ip)) {
      query$PublicIp <- ip$publicIp
    } else {
      stop("'ip' is not a recognized domain")
    }
  } else if (is.character(ip)) {
    if (!grepl("\\.", ip)) {
      query$AllocationId <- ip
    } else {
      query$PublicIp <- ip
    }
  } else {
    stop(
      "'ip' must be an allocationId, a publicIp,",
      " or an object of class 'ec2_ip'"
    )
  }

  if (!is.null(private)) {
    query$PrivateIpAddress <- private
  }
  if (!is.null(netinterface)) {
    query$NetworkInterfaceId <- netinterface
  }
  if (!isTRUE(allow)) {
    query$AllowReassociation <- TRUE
  }
  r <- ec2HTTP(query = query, ...)
  return(if (r$return[[1L]] == "true") TRUE else FALSE)
}

#' @rdname associate_ip
#' @export
disassociate_ip <- function(ip, ...) {
  query <- list(Action = "DisassociateAddress")
  if (inherits(ip, "ec2_ip")) {
    if (ip$domain == "vpc") {
      query$AllocationId <- ip$allocationId
    } else if (ip$domain == "standard") {
      query$PublicIp <- ip$publicIp
    } else {
      stop("'ip' is not a recognized domain")
    }
  } else if (is.list(ip)) {
    if ("allocationId" %in% names(ip)) {
      query$AllocationId <- ip$allocationId
    } else if ("publicIp" %in% names(ip)) {
      query$PublicIp <- ip$publicIp
    } else {
      stop("'ip' is not a recognized domain")
    }
  } else if (is.character(ip)) {
    if (!grepl("\\.", ip)) {
      query$AllocationId <- ip
    } else {
      query$PublicIp <- ip
    }
  } else {
    stop(
      "'ip' must be an allocationId, a publicIp,",
      " or an object of class 'ec2_ip'"
    )
  }
  r <- ec2HTTP(query = query, ...)
  return(r)
}
