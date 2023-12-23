base::load("common_functions.Rdata")

day_folder <- "2023/14"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))

    tmp <- strsplit(lines, "")
    ncols <- length(tmp[[1]])
    matrix(unlist(tmp), ncol = ncols, byrow = TRUE)
}

shift_ohs <- function(x) {
    if (x[1] == "#") {
        return(x)
    }
    #out <- rep(".", length(x))
    
    #n <- sum(x == "O")
    #out[seq_len(n)] <- rep("O", n)
    #out
    #sort(x, decreasing = TRUE)

    dots <- 0
    sz <- length(x) 
    out <- x
    i <- 1
    while (i + dots < sz) {
        if (out[i] == ".") {
            dots <- dots + 1
            out[i:sz] <- c(out[(i+1):sz], out[i])
            next
        }
        i <- i + 1
    }
    out
}

tilt_north <- function(input) {
    output <- matrix("", nrow = nrow(input), ncol = ncol(input))

    for (c in seq_len(ncol(input))) {
        col_str <- paste0(input[, c], collapse = "")
        pieces <- regexprmatch(col_str, pattern = "[O\\.]+|#+")[[1]]

        #idx <- grepl("#", pieces)
        
        #shifted_pieces <- pieces
        #if (!all(idx)) {    
        shifted_pieces <- unlist(lapply(pieces, \(x) shift_ohs(strsplit(x, "")[[1]])))
        #}
        output[, c] <- shifted_pieces

    }
    output
}

calc_load <- function(input) {
    #shifted_input <- tilt_north(input)

    num_rocks <- vapply(seq_len(nrow(input)), \(r) sum(input[r, ] == "O"), FUN.VALUE = 0)

    sum(num_rocks * seq(from = length(num_rocks), to = 1))       
}

rotate_clockwise <- function(m) t(m[seq(nrow(m), 1), ])

tilt_cycle <- function(input, num = 1) {
    for (i in seq_len(4*num)) {
        input <- tilt_north(input)
        input <- rotate_clockwise(input)
    }
    input
}

hash_config <- function(input) {
    openssl::sha2(paste0(as.vector(input), collapse = ""))
}

cycle_and_hash <- function(input, cycles = 10) {
    hashes <- vector("character", cycles + 1)
    hashes[1] <- hash_config(input)
    for (i in seq_len(cycles)) {
        input <- tilt_cycle(input)
        hashes[1 + i] <- hash_config(input)
    }
    hashes
}
print_map <- function(m) {
    for (r in seq_len(nrow(m))) {
        cat(paste0(m[r,], collapse = ""), "\n")
    }
    invisible(m)
}
find_cycle_params <- function(input) {
    e <- new.env()

    e$hashes <- character(0)
    
    new_hash <- hash_config(input)
    while(!new_hash %in% e$hashes) {
        e$hashes <- append(e$hashes, new_hash)
        input <- tilt_cycle(input)
        new_hash <- hash_config(input)
    }

    cycle_start <- which(e$hashes == new_hash)
    cycle_len <- length(e$hashes) - cycle_start + 1

    c(start = cycle_start, length = cycle_len)
}

test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  \(x) calc_load(tilt_north(x)),
  test_input,
  136,
  label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

cycle_params <- find_cycle_params(test_input)
offset <- (10^9 - cycle_params['start']) %% cycle_params['length']
num_cycles <- cycle_params['start'] + offset
test_example(
    \(x) calc_load(tilt_cycle(test_input, num_cycles)),
    test_input,
    64,
    label = "Test 2 1B cycles"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")
tic <- Sys.time()
total_load <- calc_load(tilt_north(input))
toc <- Sys.time()
elap_time <- toc - tic
cli::cli_alert_info("The Part 1 answer is {total_load}")
cli::cli_alert_success("The total elap time is {elap_time} {attr(elap_time, 'unit')}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")
tic <- Sys.time()
cycle_params <- find_cycle_params(input)

cli::cli_alert_success("cycle params - start: {cycle_params['start']} and length: {cycle_params['length']}")
offset <- (10^9 - cycle_params['start']) %% cycle_params['length']
num_cycles <- cycle_params['start'] + offset

total_load <- calc_load(tilt_cycle(input, num_cycles))
toc <- Sys.time()

elap_time <- toc - tic
cli::cli_alert_info("The Part 2 answer is {total_load}")
cli::cli_alert_success("The total elap time is {elap_time} {attr(elap_time, 'unit')}")
cat("\n")