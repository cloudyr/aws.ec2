#' @rdname instance_attr
#' @title EC2 Instance Attributes
#' @description Get, set, and reset attributes for an EC2 instance
#' @template instance
#' @param attribute For \code{get_instance_attr} and \code{reset_instance_attr}, a character string. For \code{set_instance_attr}, a named character string, where the name is an attribute name. Valid attribute names are: \dQuote{instanceType}, \dQuote{kernel}, \dQuote{ramdisk}, \dQuote{userData}, \dQuote{disableApiTermination}, \dQuote{instanceInitiatedShutdownBehavior}, \dQuote{rootDeviceName}, \dQuote{blockDeviceMapping}, \dQuote{productCodes}, \dQuote{sourceDestCheck}, \dQuote{groupSet}, \dQuote{ebsOptimized}, \dQuote{sriovNetSupport}.
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A character string
#' @examples
#' \dontrun{
#' i <- run_instance()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @importFrom base64enc base64decode
#' @export
get_instance_attr <- function(instance, attribute, ...) {
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    if (!attribute %in% val) {
        stop("'attribute' must be one of: ", paste0(val, collapse = ", "))
    }
    query <- list(Action = "DescribeInstanceAttribute", 
                  InstanceId = get_instanceid(instance),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname instance_attr
#' @export
set_instance_attr <- function(instance, attribute, ...) {
    query <- list(Action = "ModifyInstanceAttribute", InstanceId = get_instanceid(instance))
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    if (!unname(attribute) %in% val) {
        stop("'attribute' must be one of: ", paste0(val, collapse = ", "))
    }
    vl <- as.list(attribute)
    names(vl) <- paste0(names(vl), ".Value")
    query <- c(query, vl)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname instance_attr
#' @export
reset_instance_attr <- function(instance, attribute, ...) {
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    if (!attribute %in% val) {
        stop("'attribute' must be one of: ", paste0(val, collapse = ", "))
    }
    query <- list(Action = "ResetInstanceAttribute", 
                  InstanceId = get_instanceid(instance),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @title Get EC2 Instance Console Output
#' @description Retrieve console output for an EC2 instance as a character string
#' @template instance
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A character string
#' @examples
#' \dontrun{
#' i <- run_instance()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @importFrom base64enc base64decode
#' @export
get_console_output <- function(instance, ...) {
    query <- list(Action = "GetConsoleOutput", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    return(rawToChar(base64decode(o$output[[1]])))
}

#' @title Get EC2 Instance Password Data
#' @description Retrieve password data for an EC2 instance
#' @template instance
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' get_password_data(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @export
get_password_data <- function(instance, ...) {
    query <- list(Action = "GetPasswordData", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    return(r$passwordData)
}

#' @title Run an EC2 Instance
#' @description Run/launch a new EC2 Instance
#' @template image
#' @param type A character string specifying the type of EC2 instance to use. See \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html} for details of types and available options.
#' @param min An integer specifying a minimum number of instances to launch. Defaults to 1.
#' @param min An integer specifying a minimum number of instances to launch. Defaults to \code{min}.
#' @template keypair 
#' @template subnet
#' @param userdata Optionally, a character string specifying a script to run during launch.
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#' s <- describe_subnets()
#' i <- run_instances("ami-7f9dc615", type = "t2.micro", subnet = s[[1]])
#'
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @export
run_instances <- function(image, type, min = 1, max = min, keypair, subnet, userdata, ...) {
    query <- list(Action = "RunInstances", 
                  ImageId = get_imageid(image),
                  InstanceType = type,
                  MinCount = min,
                  MaxCount = max)
    if (!missing(keypair)) {
        query$KeyName <- keypair
    }
    if (!missing(subnet)) {
        query$SubnetId <- get_subnetid(subnet)
    }
    if (!missing(userdata)) {
        query$UserData <- userdata
    }
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$instancesSet, `class<-`, "ec2_instance"))
}

#' @rdname start_instances
#' @title Stop/Start EC2 Instance
#' @description Stop a running or start a stopped EC2 instance
#' @template instance
#' @param force \dots
#' @param info \dots
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' stop_instances(i[[1]])
#' start_instances(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
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
    return(r$instancesSet)
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
    return(r$instancesSet)
}

#' @title Terminate EC2 Instance
#' @description Terminate a stopped EC2 instance
#' @template instance
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
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
    return(r$instancesSet)
}

#' @rdname describe_instances
#' @title EC2 Instance Status
#' @description Describe or check status of an EC2 instance
#' @template instance
#' @template filter
#' @param runningonly \dots
#' @param n \dots
#' @template token
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' describe_instances(i[[1]])
#' instance_status(i[[1]])
#'
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @export
describe_instances <- function(instance, filter, n, token, ...) {
    query <- list(Action = "DescribeInstances")
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
    out <- lapply(r$reservationSet, function(x) {
        x$instancesSet <- lapply(x$instancesSet, `class<-`, "ec2_instance")
        x
    })
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

#' @rdname monitor_instance
#' @title EC2 Instance Monitoring
#' @description Set EC2 instance monitoring on or off
#' @template instance
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' monitor_instances(i[[1]])
#' unmonitor_instances(i[[1]])
#' 
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
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
    return(r$instancesSet)
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
    return(r$instancesSet)
}

#' @title Reboot EC2 Instance
#' @description Reboot a running EC2 instance
#' @template instance
#' @param ... Additional arguments passed to \code{\link{ec2HTTP}}.
#' @return A list
#' @examples
#' \dontrun{
#' i <- run_instance()
#' reboot_instances(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
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

get_instanceid <- function(x) {
    if (inherits(x, "ec2_instance")) {
        return(x$instanceId[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
}
