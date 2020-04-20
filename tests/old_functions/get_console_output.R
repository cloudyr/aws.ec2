#' @title Get EC2 Instance Console Output
#' @description Retrieve console output for an EC2 instance as a character string
#' @template instance
#' @template dots
#' @return A character string
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetConsoleOutput.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' cat(get_console_output(), "\n")
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [get_instance_attr()], [describe_instances()], [get_password_data()]
#' @keywords instances
#' @importFrom base64enc base64decode
#' @export
get_console_output <- function(instance, ...) {
    query <- list(Action = "GetConsoleOutput", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    if (is.list(r)) {
        r
    } else {
        rawToChar(base64enc::base64decode(r$output[[1]]))
    }
}


