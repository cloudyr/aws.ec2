#' @rdname start_instances
#' @title Start, Stop, Reboot, or Terminate EC2 Instance
#' @description Stop a running instance, terminate a stopped instance, start a stopped instance, or reboot an instance.
#' @template instance
#' @param force \dots
#' @param info \dots
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_StartInstances.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_StopInstances.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TerminateInstances.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RebootInstances.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' stop_instances(i[[1]])
#' start_instances(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [describe_instances()], [run_instances()]
#' @keywords instances
#' @export
start_instances <- function(instance, info, ...) {
    query <- list(Action = "StartInstances")
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
    if (!missing(info)) {
        query$AdditionalInfo <- info
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$instancesSet, function(z) {
        structure(flatten_list(z), class = "ec2_instance")
    })))
}

#' @rdname start_instances
#' @export
stop_instances <- function(instance, force, ...) {
    query <- list(Action = "StopInstances")
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
    if (!missing(force)) {
        query$Force <- tolower(as.character(force))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$instancesSet, function(z) {
        structure(flatten_list(z), class = "ec2_instance")
    })))
}

#' @rdname start_instances
#' @export
terminate_instances <- function(instance, ...) {
    query <- list(Action = "TerminateInstances")
    if(!missing(instance)) {
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
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$instancesSet, function(z) {
        structure(flatten_list(z), class = "ec2_instance")
    })))
}

#' @rdname start_instances
#' @export
reboot_instances <- function(instance, ...) {
    query <- list(Action = "RebootInstances")
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
    r <- ec2HTTP(query = query, ...)
    return(r$return)
}
