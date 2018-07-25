#' @rdname instance_attr
#' @title EC2 Instance Attributes
#' @description Get, set, and reset attributes for an EC2 instance
#' @template instance
#' @param attribute For `get_instance_attr` and `reset_instance_attr`, a character string. For `set_instance_attr`, a named character string, where the name is an attribute name. Valid attribute names are: \dQuote{instanceType}, \dQuote{kernel}, \dQuote{ramdisk}, \dQuote{userData}, \dQuote{disableApiTermination}, \dQuote{instanceInitiatedShutdownBehavior}, \dQuote{rootDeviceName}, \dQuote{blockDeviceMapping}, \dQuote{productCodes}, \dQuote{sourceDestCheck}, \dQuote{groupSet}, \dQuote{ebsOptimized}, \dQuote{sriovNetSupport}.
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstanceAttribute.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstanceAttribute.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [describe_instances()], [create_tags()]
#' @importFrom base64enc base64decode
#' @keywords instances
#' @export
get_instance_attr <-
function(
  instance,
  attribute,
  ...
) {
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    attribute <- match.arg(attribute, val)
    query <- list(Action = "DescribeInstanceAttribute", 
                  InstanceId = get_instanceid(instance),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(flatten_list(r)[[attribute]])
}

#' @rdname instance_attr
#' @export
set_instance_attr <-
function(
  instance,
  attribute,
  ...
) {
    query <- list(Action = "ModifyInstanceAttribute", InstanceId = get_instanceid(instance))
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    attribute <- match.arg(attribute, val)
    vl <- as.list(attribute)
    names(vl) <- paste0(names(vl), ".Value")
    query <- c(query, vl)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname instance_attr
#' @export
reset_instance_attr <-
function(
  instance,
  attribute,
  ...
) {
    val <- c("instanceType", "kernel", "ramdisk", "userData", "disableApiTermination", "instanceInitiatedShutdownBehavior", "rootDeviceName", "blockDeviceMapping", "productCodes", "sourceDestCheck", "groupSet", "ebsOptimized", "sriovNetSupport")
    attribute <- match.arg(attribute, val)
    query <- list(Action = "ResetInstanceAttribute", 
                  InstanceId = get_instanceid(instance),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}
