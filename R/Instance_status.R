#' @rdname describe_instances
#' @title EC2 Instance Status
#' @description Describe or check status of an EC2 instance
#' @template instance
#' @template filter
#' @param runningonly \dots
#' @param n \dots
#' @template token
#' @template dots
#' @return For `describe_instances`, a list containing several lists of elements, including one or more \dQuote{instancesSet} elements that lists various instances. For `instance_status`, a possibly empty list.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstanceStatus.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' describe_instances(i[[1]])
#' instance_status(i[[1]])
#'
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @keywords instances
#' @export
describe_instances <- function(instance, filter, n, token, ...) {
    query <- list(Action = "DescribeInstances")
    if (!missing(instance) && length(instance) > 0) {
        if (inherits(instance, "ec2_instance")) {
            instance <- list(get_instanceid(instance))
        } else if (is.character(instance)) {
            instance <- as.list(get_instanceid(instance))
        } else {
            instance <- lapply(instance, get_instanceid)
        }
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if (!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if (!missing(token)) {
        query$NextToken <- token
    }
    r <- ec2HTTP(query = query, ...)
    out <- unname(lapply(r$reservationSet, function(x) {
        x$instancesSet <- unname(lapply(x$instancesSet, function(x) {
            structure(flatten_list(x), class = "ec2_instance")
        }))
        `class<-`(x, "ec2_reservation")
    }))
    class(out) <-  "ec2_reservation_set"
    return(out)
}

#' @rdname describe_instances
#' @export
instance_status <- function(instance, filter, runningonly, n, token, ...) {
    query <- list(Action = "DescribeInstanceStatus")
    if (!missing(instance)) {
        if (inherits(instance, "ec2_instance")) {
            instance <- list(get_instanceid(instance))
        } else if (is.character(instance)) {
            instance <- as.list(get_instanceid(instance))
        } else {
            instance <- lapply(instance, get_instanceid)
        }
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if (!missing(n)) {
        if (n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if (!missing(token)) {
        query$NextToken <- token
    }
    if (!missing(runningonly)) {
        query$IncludeAllInstances <- !runningonly
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$instanceStatusSet, `class<-`, "ec2_instance_status"))
}

print.ec2_instance_status <- function(x, ...) {
    cat(sprintf("instanceId:       %s\n", x$instanceId[[1L]]))
    cat(sprintf("availabilityZone: %s\n", x$availabilityZone[[1L]]))
    cat(sprintf("instanceState:    %s (%s)\n", x$instanceState$code[[1L]], x$instanceState$name[[1L]]))
    cat(sprintf("systemStatus:     %s\n", x$systemStatus$status[[1L]]))
    cat(sprintf("instanceStatus:   %s\n", x$instanceStatus$status[[1L]]))
    invisible(x)
}

#' @rdname describe_instances
#' @export
get_instance_public_ip <- function(instance, ...) {
    instance_list <- describe_instances(instance, ...)
    if (length(instance) > 1L) {
        out <- unlist(lapply(instance_list, function(x) {
            address <- x$instancesSet[[1L]]$ipAddress
            if (is.null(address)) {
                NA_character_
            } else {
                address
            }
        }))
    } else {
        out <- instance_list[[1L]]$instancesSet[[1L]]$ipAddress
        if (is.null(out)) {
            out <- NA_character_
        }
    }
    names(out) <- instance
    return(out)
}

#' @rdname describe_instances
#' @export
get_instance_private_ip <- function(instance, ...) {
    instance_list <- describe_instances(instance, ...)
    if (length(instance) > 1L) {
        out <- unlist(lapply(instance_list, function(x) x$instancesSet[[1L]]$privateIpAddress))
    } else {
        out <- instance_list[[1L]]$instancesSet[[1L]]$privateIpAddress
        if (is.null(out)) {
            out <- NA_character_
        }
    }
    names(out) <- instance
    return(out)
}
