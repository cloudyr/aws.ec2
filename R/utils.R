.make_syntactic <- function(x) {
    gsub("-", "_", x)
}

.match_api_parameters <- function(x) {
    gsub("_", "-", x)
}
