#' @title Run an EC2 Instance
#' @description Run/launch a new EC2 Instance
#' @template image
#' @param type A character string specifying the type of EC2 instance to use. See \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html} for details of types and available options.
#' @param min An integer specifying a minimum number of instances to launch. Defaults to 1.
#' @param max An integer specifying a minimum number of instances to launch. Defaults to \code{min}.
#' @template keypair 
#' @template subnet
#' @template sgroup 
#' @param userdata Optionally, a character string specifying a script to run during launch.
#' @param shutdown A character string specifying either \dQuote{stop} or \dQuote{terminate}, to control the behavior of a shutdown action taken from within the instance.
#' @template token
#' @template dots
#' @return A list
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EC2_GetStarted.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html}
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#' s <- describe_subnets()
#' g <- create_sgroup("my_security_group", "a security group", vpc = s[[1]])
#' i <- run_instances(image = "ami-7f9dc615", 
#'                    type = "t2.micro", 
#'                    subnet = s[[1]], 
#'                    sgroup = g[[1]])
#'
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso \code{\link{describe_instances}}, \code{\link{start_instances}}, \code{\link{terminate_instances}}
#' @keywords instances
#' @export
run_instances <- 
function(image, type, min = 1, max = min, keypair, subnet, sgroup, 
         userdata, shutdown = c("stop", "terminate"), token, ...) {
    query <- list(Action = "RunInstances", 
                  ImageId = get_imageid(image),
                  InstanceType = type,
                  MinCount = min,
                  MaxCount = max)
    if (!missing(keypair)) {
        query$KeyName <- get_keypairname(keypair)
    }
    if (!missing(subnet)) {
        query$SubnetId <- get_subnetid(subnet)
    }
    if (!missing(sgroup)) {
        if (inherits(sgroup, "ec2_security_group")) {
            sgroup <- list(get_sgid(sgroup))
        } else if (is.character(sgroup)) {
            sgroup <- as.list(get_sgid(sgroup))
        } else {
            sgroup <- lapply(sgroup, get_sgid)
        }
        names(sgroup) <- paste0("SecurityGroupId.", 1:length(sgroup))
        query <- c(query, sgroup)
    }
    if (!missing(userdata)) {
        query$UserData <- userdata
    }
    if (!missing(shutdown)) {
        query$InstanceInitiatedShutdownBehavior <- match.arg(shutdown)
    }
    if (!missing(token)) {
        query$ClientToken <- token
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$instancesSet, `class<-`, "ec2_instance"))
}

#' @rdname start_instances
#' @title Start, Stop, Reboot, or Terminate EC2 Instance
#' @description Stop a running instance, terminate a stopped instance, start a stopped instance, or reboot an instance.
#' @template instance
#' @param force \dots
#' @param info \dots
#' @template dots
#' @return A list
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_StartInstances.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_StopInstances.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TerminateInstances.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RebootInstances.html}
#' @examples
#' \dontrun{
#' i <- run_instances()
#' stop_instances(i[[1]])
#' start_instances(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso \code{\link{describe_instances}}, \code{\link{run_instances}}
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

print.ec2_instance <- function(x, ...) {
    cat("instanceId: ", x$instanceId, "\n")
    cat("imageId: ", x$imageId, "\n")
    cat("instanceType: ", x$instanceType, "\n")
    cat("instanceState: ", paste0(x$instanceState$name, " (", x$instanceState$code, ")"), "\n")
    cat("subnetId: ", x$subnetId, "\n")
    cat("vpcId: ", x$vpcId, "\n")
    cat("ipAddress: ", x$ipAddress, "\n")
    invisible(x)
}
