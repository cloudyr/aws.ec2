#' @rdname convert_ip
#' @title IP conversion
#' @description Convert IP between Classic/EC2
#' @details IP addresses can only be used within EC2 classic or a Virtual Private Cloud. These functions allow you to convert an IP address between the two systems.
#' @template ip
#' @template dots
#' @return A list.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RestoreAddressToClassic.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RestoreAddressToClassic.html>
#' @export
make_ip_vpc <- function(ip, ...) {
    query <- list(Action = "MoveAddressToVpc")
    if (inherits(ip, "ec2_ip")) {
        query$PublicIp <- ip[["publicIp"]]
    } else if (is.list(ip)) {
        if ("publicIp" %in% names(ip)) {
            query$PublicIp <- ip[["publicIp"]]
        } else {
            stop("'ip' is not a recognized domain")
        }
    } else if (is.character(ip)) {
        if (grepl("\\.", ip)) {
            query$PublicIp <- ip
        }
    } else {
        stop("'ip' must be a publicIp, or an object of class 'ec2_ip'")
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname convert_ip
#' @export
make_ip_classic <- function(ip, ...) {
    query <- list(Action = "RestoreAddressToClassic")
    if (inherits(ip, "ec2_ip")) {
        query$PublicIp <- ip[["publicIp"]]
    } else if (is.list(ip)) {
        if ("publicIp" %in% names(ip)) {
            query$PublicIp <- ip[["publicIp"]]
        } else {
            stop("'ip' is not a recognized domain")
        }
    } else if (is.character(ip)) {
        if (grepl("\\.", ip)) {
            query$PublicIp <- ip
        }
    } else {
        stop("'ip' must be a publicIp, or an object of class 'ec2_ip'")
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
