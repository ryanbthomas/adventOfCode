read_input <- function(path, parsers) {
    if (!file.exists(path)) {
        stop(path, "does not exist.", call. = FALSE)
    }
    all_lines <- readLines(path, warn = FALSE)
    num_groups <- sum(all_lines == "") + 1
    
    results <- vector("list", num_groups)

    is_blank <- all_lines == ""
    idx <- cumsum(is_blank) + 1
    idx[is_blank] <- 0

    for (grp_idx in seq_len(num_groups)) {
        if (grp_idx <= length(parsers)) {
            f <- parsers[[grp_idx]]
        }
        results[[grp_idx]] <- f(all_lines[idx == grp_idx])
    }
    return(results)
    
}

comma_separated_line <- function(x) {
    items <- strsplit(x, split = ",")[[1]]
    as.integer(items)
}

space_separated_matrix <- function(x) {
    num_cols <- length(regmatches(x[1], gregexpr("(?<=[0-9])\\s+", x[1], perl = TRUE))[[1]]) + 1 
    
    single_line <- paste0(x, collapse = " ")
    str_values <- strsplit(single_line, split = " ")[[1]]
    int_values <- as.integer(str_values[!str_values == ""])
    matrix(int_values, ncol = num_cols, byrow = TRUE)
}