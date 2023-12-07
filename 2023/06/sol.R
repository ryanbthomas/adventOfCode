base::load("common_functions.Rdata")

day_folder <- "2023/06"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    lapply(regexprmatch(lines, "[0-9]+"), as.integer) |> setNames(c("time", "record"))
}
input_parser02 <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    lines <- gsub(lines, pattern = " ", replacement = "")
    lapply(regexprmatch(lines, "[0-9]+"), bit64::as.integer64) |> setNames(c("time", "record"))
}


find_possible_range <- function(time, record, debug = FALSE) {

    if (!"integer64" %in% class(time)) {
        disc <- sqrt(time^2 - 4 * record)
        min_val <- ceiling((time - disc) / 2) + ifelse(((time - disc)/ 2) %% 2 ==0, 1, 0)
        max_val <- floor((time + disc) / 2) + ifelse(((time + disc)/ 2) %% 2 ==0, -1, 0)
        return(c(min_val, max_val))
    }

    disc <- time^bit64::as.integer64(2L) - bit64::as.integer64(4L) * record
    if(debug) {
        cli::cli_alert_info("disc is {disc}")
        dbl_disc <- as.numeric(disc)
        cli::cli_alert_info("sqrt(disc): {sqrt(disc)}")
    }

    disc <- sqrt(as.numeric(disc))
    #min_val <- ceiling((time - disc) / 2) + ifelse(((time - disc)/ 2) %% 2 ==0, 1, 0)
    #max_val <- floor((time + disc) / 2) + ifelse(((time + disc)/ 2) %% 2 ==0, -1, 0)
    min_val <- (time - disc) / 2
    max_val <- (time + disc) / 2
    
    c(min_val, max_val)
}

calc_margin_of_error <- function(input) {
    rngs <- lapply(seq_along(input[[1]]), function(y) find_possible_range(input[[1]][y], input[[2]][y]))
    rng_len <- vapply(rngs, \(x) diff(x) + 1, FUN.VALUE = 0)
    prod(rng_len)
}

test_input <- input_parser("example01")


cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
   function(x) {
    rngs <- lapply(seq_along(x[[1]]), function(y) find_possible_range(x[[1]][y], x[[2]][y]))
    vapply(rngs, \(x) diff(x) + 1, FUN.VALUE = 0)
   },
   test_input,
   c(4, 8, 9),
   label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_input2 <- input_parser02("example01")
test_example(
   function(x) {
    rng <- find_possible_range(x[["time"]], x[["record"]])
    diff(rng)  + 1
   },
   test_input2,
   71503,
   label = "Test 2"
)


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

margin <- calc_margin_of_error(input)

cli::cli_alert_info("The Part 1 answer is {margin}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")
input2 <- input_parser02("input")
rng <- find_possible_range(input2[["time"]], input2[["record"]])

margin2 <- diff(rng) + 1

cli::cli_alert_info("The Part 2 answer is {margin2}")
cat("\n")