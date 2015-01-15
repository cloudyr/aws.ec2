.makelist <- function(list, type = "Filter") {
    tmp <- as.list(c(names(list), list))
    names(tmp) <- c(paste0(type,".",1:length(list),".Key"),
                    paste0(type,".",1:length(list),".Value"))
    return(tmp)
}
