#' @title Delete Launch Template
#' @description Delete Instance Template
#' @param name The name of the launch template to delete.
#' @param tempId The ID of the launch template to delete. Must specify
#' name or ID.
#' @template dots
#' @example 
#' \dontrun{
#' delete_template(name = "testTemp", desc = "newDesc", 
#'                 templateData = list(ImageId = "ami-1a2b3c4d", InstanceType = "t1.micro"),
#'                 clientToken = "123")
#' }
#' @keywords template
#' @references 
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html>
#' @export

delete_template <- function(
    id = NULL,
    name = NULL,
    ...
) {
    query <- list(Action = "DeleteLaunchTemplate")
    
    if(!is.null(id)) {
        names(id) <- "LaunchTemplateId"
        query <- c(query, id)
    }
    else if(!is.null(name)) {
        names(name) <- "LaunchTemplateName"
        query <- c(query, name)
    } else {
        stop("delete_template wasn't passed a name or ID")
    }
    
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r, function(z) {
        structure(flatten_list(z), class = "ec2_template_del")
    })))
}