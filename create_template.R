#' @title Create Launch Template
#' @description Create Instance Template, templates can be sent to run_instance()
#' @param name A name for the launch template, String, Minchar:3, Max:128
#' @param desc A description for the first version of the template.
#' @param templateData A named list of template data. See reference
#' @param clientToken Identifier to ensure idempotency.
#' @template dots
#' @example 
#' \dontrun{
#' create_template(name = "testTemp", desc = "newDesc", 
#'                 templateData = list(ImageId = "ami-1a2b3c4d", InstanceType = "t1.micro"),
#'                 clientToken = "123")
#' }
#' @keywords template
#' @references 
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html>
#' @export


create_template <-
function(
    name = NULL,
    desc = NULL,
    templateData = NULL,
    clientToken = NULL,
    ...
    
) {
    query <- list(Action = "CreateLaunchTemplate")
    if(!is.null(name)) {
        names(name) <- "LaunchTemplateName"
        query <- c(query, name)
    }
    if(!is.null(desc)) {
        names(desc) <- "VersionDescription"
        query <- c(query, desc)
    }
    if(!is.null(clientToken)) {
        names(clientToken) <- "ClientToken"
        query <- c(query, clientToken)
    }
    if(!is.null(templateData)) {
        names(templateData) <- paste0("LaunchTemplateData.", names(templateData))
        query <- c(query, templateData)
    }

    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r, function(z) {
        structure(flatten_list(z), class = "ec2_template")
    })))
}