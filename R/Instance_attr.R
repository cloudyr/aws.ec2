#' @rdname instance_attr
#' @title EC2 Instance Attributes
#' @description Get, set, and reset attributes for an EC2 instance
#' @template instance
#' @param attribute For \code{get_instance_attr} and \code{reset_instance_attr}, a character string. For \code{set_instance_attr}, a named character string, where the name is an attribute name. Valid attribute names are: \dQuote{instanceType}, \dQuote{kernel}, \dQuote{ramdisk}, \dQuote{userData}, \dQuote{disableApiTermination}, \dQuote{instanceInitiatedShutdownBehavior}, \dQuote{rootDeviceName}, \dQuote{blockDeviceMapping}, \dQuote{productCodes}, \dQuote{sourceDestCheck}, \dQuote{groupSet}, \dQuote{ebsOptimized}, \dQuote{sriovNetSupport}.
#' @template dots
#' @return A list
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstanceAttribute.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstanceAttribute.html}
#' @examples
#' \dontrun{
#' i <- run_instances()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso \code{\link{describe_instances}}, \code{\link{create_tags}}
#' @importFrom base64enc base64decode
#' @keywords instances
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
    return(flatten_list(r)[[attribute]])
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
#' @template dots
#' @return A character string
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetConsoleOutput.html}
#' @examples
#' \dontrun{
#' i <- run_instances()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso \code{\link{get_instance_attr}}, \code{\link{describe_instances}}, \code{\link{get_password_data}}
#' @keywords instances
#' @importFrom base64enc base64decode
#' @export
get_console_output <- function(instance, ...) {
    query <- list(Action = "GetConsoleOutput", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    return(rawToChar(base64decode(r$output[[1]])))
}

#' @title Get EC2 Instance Password Data
#' @description Retrieve password data for an EC2 instance
#' @template instance
#' @template dots
#' @return A list
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetConsoleOutput.html}
#' @examples
#' \dontrun{
#' i <- run_instances()
#' get_password_data(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso \code{\link{describe_instances}}, \code{\link{get_console_output}}
#' @keywords instances
#' @export
get_password_data <- function(instance, ...) {
    query <- list(Action = "GetPasswordData", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    return(r$passwordData)
}

