.makelist <- function(list, type = "Filter") {
    tmp <- as.list(c(names(list), list))
    names(tmp) <- c(paste0(type,".",1:length(list),".Key"),
                    paste0(type,".",1:length(list),".Value"))
    return(tmp)
}

flatten_list <- function(x) {
    if (is.list(x)) {
        if ((class(x) != "list") || (length(class(x)) > 1)) {
            return(x)
        } else {
            if (length(x) == 1) {
                return(flatten_list(x[[1]]))
            } else {
                return(lapply(x, flatten_list))
            }
        }
    } else {
        return(x)
    }
}
