base::load("common_functions.Rdata")

day_folder <- "2023/03"


input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))

    ncols <- nchar(lines[1])
    nrows <- length(lines)

    list(
        string = paste0(lines, collapse = ""),
        dims = c(row = nrows, col = ncols),
        index_to_rc = function(idx, len = attr(idx, "match.length")) {
            x <- idx
            if (length(len) > 1 && !all(len == 1)) {
                stop("problem", call. = FALSE)
            }
            if (length(len) == 1) {
                x <- seq.int(from = idx, to = idx + len - 1)    
            } 
            lapply(x, 
                function(x) {
                c(x %/% ncols + (x %% ncols > 0), (x - 1) %% ncols + 1)
                }
            )
        }
    )
}

encircle <- function(pt) {
    dirs <- list(
        c(-1, -1), c(-1, 0), c(-1, 1),
        c(0, -1), c(0, 1),
        c(1, -1), c(1, 0), c(1, 1)
        )

    #res <- vector("list", length(dirs) * length(pt))
    #for (i in seq_along(pt)) {
    #    res[seq_along(dirs) + (i - 1)] <- lapply(dirs, function(d) pt[[i]] + d)
    #}
    #unique(res)
    lapply(dirs, function(d) pt + d)
}

engine_part_nums <- function(input, debug = FALSE) {
    symbols <- gregexpr(pattern = "[^0-9.]", text = input$string)

    symbols_pos <- input$index_to_rc(symbols[[1]])
    valid_parts_pos <- unique(do.call(c, lapply(symbols_pos, encircle)))

    num_matches <- gregexpr(pattern = "[0-9]+", text = input$string)
    nums <- as.integer(regmatches(input$string, num_matches)[[1]])

    for(i in seq_along(num_matches[[1]])) {
        num_pos <- input$index_to_rc(num_matches[[1]][i], attr(num_matches[[1]], "match.length")[i])
        if (!any(num_pos %in% valid_parts_pos)) {
            nums[i] <- 0
        }
    }
    if (debug) return(nums)

    sum(nums)

}
engine_part_nums2<- function(input, debug = FALSE) {
    symbols <- gregexpr(pattern = "[^0-9.]", text = input$string)

    symbols_pos <- input$index_to_rc(symbols[[1]])
    #valid_parts_pos <- unique(do.call(c, lapply(symbols_pos, encircle)))

    num_matches <- gregexpr(pattern = "[0-9]+", text = input$string)
    nums <- as.integer(regmatches(input$string, num_matches)[[1]])

    for(i in seq_along(num_matches[[1]])) {
        num_pos <- input$index_to_rc(num_matches[[1]][i], attr(num_matches[[1]], "match.length")[i])
        num_circle <- unique(do.call(c, lapply(num_pos, encircle)))
        if (!any(symbols_pos %in% num_circle)) {
            nums[i] <- 0
        }
    }
    if (debug) return(nums)

    sum(nums)

}


sum_gear_ratios <- function(input, debug = FALSE) {
    gears <- gregexpr(pattern = "[*]", text = input$string)

    gears_pos <- input$index_to_rc(gears[[1]])
    #valid_parts_pos <- unique(do.call(c, lapply(gear_pos, encircle)))

    gear_ratio_inputs <- vector("list", length(gears_pos))

    num_matches <- gregexpr(pattern = "[0-9]+", text = input$string)
    nums <- as.integer(regmatches(input$string, num_matches)[[1]])

    for(i in seq_along(num_matches[[1]])) {
        num_pos <- input$index_to_rc(num_matches[[1]][i], attr(num_matches[[1]], "match.length")[i])
        num_circle <- unique(do.call(c, lapply(num_pos, encircle)))
        gear_idx <- gears_pos %in% num_circle
        if (any(gear_idx)) {
            gear_num <- seq_along(gears_pos)[gear_idx]
            gear_ratio_inputs[[gear_num]] <- c(gear_ratio_inputs[[gear_num]], nums[i])
        }
    }

    gear_ratios <- sapply(gear_ratio_inputs, function(gri) ifelse(length(gri)==2, prod(gri), 0))


    if (debug) return(gear_ratio_inputs)

    sum(gear_ratios)

}

test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
   engine_part_nums2,
   test_input,
   4361,
   label = "Test 1: engine part nums"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
   sum_gear_ratios,
   test_input,
   467835,
   label = "Test 2: sum gear ratios"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

result <- engine_part_nums2(input)

cli::cli_alert_info("The Part 1 answer is {result}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

result2 <- sum_gear_ratios(input)

cli::cli_alert_info("The Part 2 answer is {result2}")
cat("\n")