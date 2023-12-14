base::load("common_functions.Rdata")

day_folder <- "2023/12"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    tmp <- strsplit(lines, " ")

    f <- function(x) {
        list(
            record = c(strsplit(x[[1]], "")[[1]], "."), 
            check = as.integer(strsplit(x[[2]], ",")[[1]])
        )
    }
    lapply(tmp, f) 

}

valid_arrangement <- function(record, seg_length) {
    p <- \(...) paste0(..., collapse = "")
    pattern <- p("[\\?#]{", seg_length - 1, "}[\\.\\?]") 
    #cli::cli_alert_danger("checking validity of {p(record)} and {pattern}")
    grepl(pattern = pattern, p(record), perl = TRUE)
}

count_arrangments <- function(record, check, debug = FALSE) {

    if (length(record) == 0 || length(check) == 0) {
        #cli::cli_alert_info("Got to end, returning {as.integer(length(check) == 0)}")
        return(as.integer(length(check) == 0))
    }
    
    remaining_length <- sum(check[-1]) + length(check[-1])

    min_start <- 1L
    max_start <- length(record) - remaining_length - check[1]
    first_hash <- as.integer(regexpr(text = paste0(record, collapse = ""), pattern = "#"))
    if (first_hash > 0) {
        max_start <- min(first_hash, max_start)
    }

    num_ways <- 0L
    #cli::cli_alert_info("looking for seq of length {check[1]} in {min_start}, {max_start}")
    for (i in seq(min_start, max_start)) {
        # how many ways can check[1] work for segment i to i + seg_length -1
        # times how many ways can I count what's left
        seg_length <- check[1] + 1
        idx <- seq(i, i + seg_length - 1)
        if (debug) cli::cli_alert_info("idx: {idx}")
        r <- record[idx]
        if (debug) cli::cli_alert_warning("Trying {paste0(r, collapse = '')}, check: [{check[1]}]")
        if (r[1] == "." || !valid_arrangement(r, seg_length)) {
            next
        }

        if (length(check) == 1) {
            if (debug) cli::cli_alert_info("We're on the last check [{check}]. Adding 1 to {num_ways}")
            #cli::cli_alert_info("")
            num_ways <- num_ways + 1
            if (debug) cli::cli_alert_info(" => {num_ways}")
            
           next  
        } 

        if(debug) cli::cli_alert_danger("Do I make it here?")
        r2 <- record[-seq_len(idx[length(idx)])]
        #following_arrangments <- as.integer(length(check[-1]) == 0)
        
        #if (seg_length < length(record)) {
            if (debug) cli::cli_alert_success("it's valid; look at {paste0(r2, collapse = '')} and {check[-1]}")
            #browser()
            following_arrangments <- count_arrangments(r2, check[-1])

#        }
        if(debug) cli::cli_alert_info("Adding {following_arrangments} to {num_ways}")
        num_ways <- num_ways + following_arrangments
        if (debug) cli::cli_alert_info(" => {num_ways}")
    }

    num_ways
}

trim_record <- function(record, check) {
    front_fluff <- regexprmatch(record, "^[^\\?]+")[[1]]
    back_fluff <- regexprmatch(record, "[^\\?]+$")[[1]]

    if (length(front_fluff) == 0 && length(back_fluff) == 0) {
        return(list(record = record, check = check))
    }
    new_record_idx <- c(1, nchar(record))
    if (length(front_fluff) > 0) {
        known_busted <- strsplit(front_fluff, "\\.")[[1]]
        idx <- known_busted == ""
        known_busted <- known_busted[!idx]

        i <- 1L
        for (b in known_busted) {
            check[i] <- check[i] - nchar(b)
            stopifnot(check[i] >= 0)
            i <- i + 1
        }
        new_record_idx[1] <- nchar(front_fluff) + 1
    }

    if (length(back_fluff) > 0) {
        known_busted <- strsplit(back_fluff, "\\.")[[1]]
        idx <- known_busted == ""
        known_busted <- known_busted[!idx]

        i <- length(check) 
        for (b in rev(known_busted)) {
            check[i] <- check[i] - nchar(b)
            stopifnot(check[i] >= 0)
            i <- i - 1
        }
        new_record_idx[2] <- nchar(record) - nchar(back_fluff)
    }


    new_record <- substr(record, new_record_idx[1], new_record_idx[2]) 

    list(record = new_record, check = check[check > 0])
}

all_combos <- function(record) {
  idx <- record == "?"
  opts <- expand.grid(lapply(seq_len(sum(idx)), \(x) c("#", ".")), stringsAsFactors = FALSE)

  all_combos <- vector("list", length = nrow(opts))

  for (i in seq_along(all_combos)) {
    tmp <- record
    tmp[idx] <- unlist(opts[i, ])
    all_combos[[i]] <- tmp
  }
 vapply(all_combos, \(x) paste0(x, collapse = ""), FUN.VALUE = "")
}

validate_arrangment <- function(record, check) {
    grps <- lapply(strsplit(record, "\\."), \(x) nchar(x[!x == ""]))
    vapply(grps, \(x) identical(x, check), FUN.VALUE = TRUE)

}

count_slowly <- function(record, check) {
    a <- all_combos(record)
    v <- validate_arrangment(a, check)
    sum(v)
}

test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")

example01_answers <- c(1, 4, 1, 1, 4, 10)

for (i in seq_along(example01_answers)) {
    test_label <- paste0("Test ", i, ": ", paste0(c("'", test_input[[i]]$record, "'"), collapse = ""), " ", pretty_print_vector(test_input[[i]]$check))

    test_example(
    \(x) do.call(count_arrangments, x),
    test_input[[i]],
    example01_answers[i],
    label =  test_label   
    )
    
    test_example(
    \(x) do.call(count_slowly, x),
    test_input[[i]],
    as.integer(example01_answers[i]),
    label =  paste0("Count Slowly Confirmation ", i)   
    )
}

# more testing. Let's check all of the inputs with 8 '?' are fewer with count_slowly

for (i in seq_along(input)) {
    num_qs <- sum(input[[i]]$record == '?')
    cat(i, ": ", sep = "")
    if (num_qs > 8) {
        cat("skip", "\n")
        next
    }

    correct_answer <- count_slowly(input[[i]]$record, input[[i]]$check)
    fast_answer <- count_arrangments(input[[i]]$record, input[[i]]$check)

    if (fast_answer == correct_answer) {
        cat("OK", "\n")
    } else {
        cat("DEAD!\n")
        cli::cli_alert_danger("record: {input[[i]]$record} check: {input[[i]]$check}\nfast: {fast_answer} correct: {correct_answer}")
    
    }        

    
}

cat(cli::col_green("Part 2 Test Cases"), "\n")


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

arrangements <- unlist(lapply(input, \(x) do.call(count_arrangments, x)))
#slow_arrangements <- unlist(lapply(input, \(x) do.call(count_slowly, x)))

cli::cli_alert_info("The Part 1 answer is {sum(arrangements)}")
#cli::cli_alert_info("The SLOW answer is {sum(slow_arrangements)}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


cli::cli_alert_info("The Part 2 answer is ")
cat("\n")