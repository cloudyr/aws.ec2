#' @rdname monitor_instance
#' @title EC2 Instance Monitoring
#' @description Set EC2 instance monitoring in CloudWatch on or off.
#' @template instance
#' @template dots
#' @return A list.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_MonitorInstances.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_UnmonitorInstances.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' monitor_instances(i[[1]])
#' unmonitor_instances(i[[1]])
#' 
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [describe_instances()]
#' @keywords instances
#' @export
monitor_instances <- function(instance, ...) {
    query <- list(Action = "MonitorInstances")
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
    return(unname(lapply(r$instancesSet, flatten_list)))
}

#' @rdname monitor_instance
#' @export
unmonitor_instances <- function(instance, ...) {
    query <- list(Action = "UnmonitorInstances")
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
    return(unname(lapply(r$instancesSet, flatten_list)))
}
