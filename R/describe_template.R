#' @title Describe Launch Template
#' @description Describe Instance Template
#' @param tempName The name of the launch template to describe
#' @param tempId The ID of the launch template to delete. Must specify
#' name or ID.
#' @param filter One or more filters
#' @template dots
#' @example 
#' \dontrun{
#' create_template(tempName = "testTemplate", tempDesc = "newDesc", 
#'                 templateData = list(ImageId = "ami-1a2b3c4d", InstanceType = "t1.micro"),
#'                 clientToken = "123")
#' describe_template(tempName = "testTemplate")
#' delete_template(tempDesc = "testTemplate")
#' }
#' @keywords template
#' @references 
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Filter.html>
#' @export

describe_template <-
    function(
        tempName = NULL,
        tempId = NULL,
        filter = NULL,
        ...
    ){
        
        query <- list(Action = "DescribeLaunchTemplates")
        
        if(!is.null(tempId)) {
            names(tempId) <- "LaunchTemplateId.1"
            query <- c(query, tempId)
        }
        else if(!is.null(tempName)) {
            names(tempName) <- "LaunchTemplateName.1"
            query <- c(query, tempName)
        }
        else {
            stop("describe_template wasn't passed a name/Id")
        }
        if(!is.null(filter)) {
            names(filter) <- "Filter.1"
            query <- c(query, filter)
        }
        
        r <- ec2HTTP(query = query, ...)
        return(unname(lapply(r, function(z) {
            structure(flatten_list(z), class = "ec2_template_desc")
        })))
    }