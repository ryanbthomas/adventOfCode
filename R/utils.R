pretty_print_vector <- function(x) {
    paste0("[", paste0(x, collapse = ", "), "]")
}

regexprmatch <- function(x, pattern) {
    regmatches(x, gregexpr(pattern = pattern, text = x))
}