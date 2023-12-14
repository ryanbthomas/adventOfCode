base::load("common_functions.Rdata")

day_folder <- "2023/11"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    tmp <- gsub(pattern = "[^#]", replacement = "0", lines)
    tmp <- gsub(pattern = "#", replacement = "1", tmp)
    tmp <- strsplit(tmp, "")
    tmp <- lapply(tmp, \(x) matrix(as.integer(x), byrow = TRUE, nrow = 1))
    Reduce(rbind, tmp)
    
}
expand_universe <- function(map) {
    row_sums <- rowSums(map)
    breaks <- seq_len(ncol(map))[row_sums == 0]

    empty_row <- matrix(0, nrow = 1L, ncol = ncol(map))
    for (r in rev(breaks)) {
        idx <- seq_len(r)
        map <- rbind(map[idx, ], empty_row, map[-idx, ])
    }

    col_sums <- colSums(map)
    breaks <- seq_len(nrow(map))[col_sums == 0]

    empty_col <- matrix(0, nrow = nrow(map), ncol = 1L)
    for (r in rev(breaks)) {
        idx <- seq_len(r)
        map <- cbind(map[, idx], empty_col, map[, -idx])
    }
    
    map
}

calc_distances <- function(map, debug = FALSE) {
    galaxies <- which(map == 1L, arr.ind = TRUE)

    num_galaxies <- nrow(galaxies)
    dists <- vector("integer", num_galaxies * (num_galaxies - 1) / 2L)
    dist_cnt <- 0
    while(nrow(galaxies) > 1) {
        g <- galaxies[1, ]
        galaxies <- galaxies[-1, , drop = FALSE]

        tmp <- vapply(seq_len(nrow(galaxies)), \(x) sum(abs(g - galaxies[x, ] )), FUN.VALUE = 0L)
        
        if (debug) {
            for (i in seq_along(tmp)) {
                cli::cli_alert_info("{pretty_print_vector(g)} -> {pretty_print_vector(galaxies[i,])}: {tmp[i]}")
            }
        }
        dists[dist_cnt + seq_along(tmp)] <- tmp
        dist_cnt <- dist_cnt + length(tmp)
    }
    dists
}

calc_distances2 <- function(map, expansion_factor = 1L, debug = FALSE) {
    galaxies <- which(map == 1L, arr.ind = TRUE)
    
    row_sums <- rowSums(map)
    row_breaks <- seq_len(ncol(map))[row_sums == 0]
    col_sums <- colSums(map)
    col_breaks <- seq_len(nrow(map))[col_sums == 0]

    d <- function(x, y) galaxy_dist(x, y, row_breaks, col_breaks, expansion_factor = expansion_factor) 

    num_galaxies <- nrow(galaxies)
    dists <- vector("integer", num_galaxies * (num_galaxies - 1) / 2L)
    dist_cnt <- 0
    while(nrow(galaxies) > 1) {
        g <- galaxies[1, , drop = FALSE]
        galaxies <- galaxies[-1, , drop = FALSE]

        tmp <- vapply(seq_len(nrow(galaxies)), \(x) d(g, galaxies[x, , drop = FALSE]), FUN.VALUE = 0L)
        
        if (debug) {
            for (i in seq_along(tmp)) {
                cli::cli_alert_info("{pretty_print_vector(g)} -> {pretty_print_vector(galaxies[i,])}: {tmp[i]}")
            }
        }
        dists[dist_cnt + seq_along(tmp)] <- tmp
        dist_cnt <- dist_cnt + length(tmp)
    }
    dists
}

galaxy_dist <- function(g1, g2, empty_rows, empty_cols, expansion_factor = 1L) {
    #cli::cli_alert_info("{g1}: class {class(g1)}")
    min_row <- min(g1[1, 1], g2[1, 1])
    max_row <- max(g1[1, 1], g2[1, 1])
    num_rows <- sum(empty_rows >= min_row & empty_rows <= max_row)

    min_col <- min(g1[1, 2], g2[1, 2])
    max_col <- max(g1[1, 2], g2[1, 2])
    num_cols <- sum(empty_cols >= min_col & empty_cols <= max_col)

    sum(abs(g1 - g2)) + expansion_factor * (num_rows + num_cols)
    
}

sum_dists <- function(input) {
    map <- expand_universe(input)
    sum(calc_distances(map))
}
test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  sum_dists,
  test_input,
  374L,
  label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
  \(x) sum(calc_distances2(x)),
  test_input,
  374L,
  label = "Test 2"
)

test_example(
  \(x) sum(calc_distances2(x, expansion_factor = 9L)),
  test_input,
  1030L,
  label = "Test 3"
)

test_example(
  \(x) sum(calc_distances2(x, expansion_factor = 99L)),
  test_input,
  8410L,
  label = "Test 4"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

total_dist <- sum_dists(input)

cli::cli_alert_info("The Part 1 answer is {total_dist}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

all_dists <- calc_distances2(input, expansion_factor = 999999L)

total_dist <- sum(all_dists)

cli::cli_alert_info("The Part 2 answer is {total_dist}")
cat("\n")