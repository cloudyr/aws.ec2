#' @title Get EC2 Instance Password Data
#' @description Retrieve password data for an EC2 instance
#' @template instance
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetConsoleOutput.html>
#' @examples
#' \dontrun{
#' i <- run_instances()
#' get_password_data(i[[1]])
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [describe_instances()], [get_console_output()]
#' @keywords instances
#' @export
get_password_data <- function(instance, ...) {
    query <- list(Action = "GetPasswordData", InstanceId = get_instanceid(instance))
    r <- ec2HTTP(query = query, ...)
    return(r$passwordData)
}

