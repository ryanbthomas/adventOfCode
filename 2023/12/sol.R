base::load("common_functions.Rdata")

day_folder <- "2023/12"
library(collections)

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    tmp <- strsplit(lines, " ")

    f <- function(x) {
        list(
            #record = c(strsplit(x[[1]], "")[[1]], "."), 
            record = strsplit(x[[1]], "")[[1]], 
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
    pretty_record <- paste0(record, collapse = "")
    if (debug) cli::cli_alert_info("{pretty_record}: starting with {pretty_print_vector(check)}")
    if (length(record) == 0 || length(check) == 0) {
        if(debug) cli::cli_alert_info("{pretty_record} Got to end, returning {as.integer(length(check) == 0)}")
        return(as.integer(length(check) == 0))
    }
    
    remaining_length <- sum(check[-1]) + length(check[-1])

    min_start <- 1L
    max_start <- length(record) - remaining_length - check[1]
    first_hash <- as.integer(regexpr(text = paste0(record, collapse = ""), pattern = "#"))
    if (first_hash > 0) {
        max_start <- min(first_hash, max_start)
    }
    
    if (length(check) == 1 && grepl(pretty_record, pattern = paste0("^[#]{", check, "}\\.+", collapse = ""))) {
        if (debug) cli::cli_alert_info("Only one way. Returning 1.")
        return(1)
    }
    
    # if (length(check) == 1 && grepl(paste0(record, collapse = ""),  pattern = paste0("^\\?{", check,", }\\.+")) {
    #     # if we have a string of '?'. Last will always b
    #     num_qs <- sum(record == "?")
    #     num_opts <- num_qs - check + 1
    #     if (debug) cli::cli_alert_info("{pretty_record} check: {check}; returning {num_opts}")
    #     return(num_opts)
    # }

    num_ways <- 0L
    if(debug) cli::cli_alert_info("{pretty_record} looking for seq of length {check[1]} in {min_start}, {max_start}")
    i <- min_start
    while (i <= max_start) {
    #for (i in seq(min_start, max_start)) {
        # how many ways can check[1] work for segment i to i + seg_length -1
        # times how many ways can I count what's left
        seg_length <- check[1] + 1
        idx <- seq(i, i + seg_length - 1)
        #if (debug) cli::cli_alert_info("idx: {idx}")
        r <- record[idx]
        if (debug) cli::cli_alert_warning("{pretty_record} Trying {paste0(r, collapse = '')}, check: [{check[1]}]")
        
        if (r[1] == "." || !valid_arrangement(r, seg_length)) {
            i <- i + 1
            next
        }
        
        if (length(check) == 1) {
            if (debug) cli::cli_alert_info("{pretty_record} We're on the last check [{check}]. Should we increment?")
            #cli::cli_alert_info("")
            r_check <- record[-seq_len(i + seg_length -1)]
            #if(debug) browser()
            to_add <- ifelse("#" %in% r_check, 0, 1)
            #check_r[idx] <- rep("#", seg_length)
            #to_add <- as.integer(sum(r_check == "#") == check)
            num_ways <- num_ways + to_add
            if (debug) cli::cli_alert_info("{pretty_record} Adding {num_ways - to_add} + {to_add}=> {num_ways}")
            i <- i + 1
            #i <- i + seg_length + 1
           next  
        } 
        r2 <- record[-seq_len(idx[length(idx)])]
        #following_arrangments <- as.integer(length(check[-1]) == 0)
        i <- i + 1       
        #if (seg_length < length(record)) {
            if (debug) cli::cli_alert_success("{pretty_record} it's valid; look at {paste0(r2, collapse = '')} and {check[-1]}")
            #browser()
            following_arrangments <- count_arrangments(r2, check[-1], debug)

#        }
        if(debug) cli::cli_alert_info("{pretty_record} Adding {following_arrangments} to {num_ways}")
        num_ways <- num_ways + following_arrangments
        if (debug) cli::cli_alert_info("{pretty_record} => {num_ways}")
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
    if(length(check) == 0) {
        num_hashes <- sum(record == "#")
        return(ifelse(num_hashes == 0, 1, 0))
    }

    a <- all_combos(record)
    v <- validate_arrangment(a, check)
    sum(v)
}

valid_a <- function(record, check) {
    if (nchar(record) < check[1]) {
        return(FALSE)
    }
    seg_size <- min(length(record), check[1])
    idx <- seq_len(seg_size)
    #rec <- paste0(record, collapse = "")
    rec <- record[idx]
    rec_str <- paste0(rec, collapse = "")
    next_r <- record[seg_size + 1]
    
    #if (length(check) == 1 && length(record) > seg_size && "#" %in% record[-idx]) {
    #    return(FALSE)
    #}
    result <- grepl(rec_str, pattern = paste0("^[^\\.]{", check, "}")) && (is.na(next_r[1]) || next_r[1] != "#")
    
    if (length(check) == 1 && length(record) > seg_size) {
        result <- result && '#' %in% record[-idx]
    }

    cli::cli_alert_info("valid?: {rec} followed by {next_r[1]} = {result}")
    result
}
is_valid_segment <- function(record, check) {
    pattern <- paste0("^[^\\.]{", check[1], "}")
    if (length(record) == check[1]) {
        rec_str <- paste0(record, collapse = "")
        return(grepl(rec_str, pattern = pattern))
    }

    # if we're here than record is longer than check
    rec_str <- paste0(record[seq_len(check[1] + 1)], collapse = "")
    pattern <- paste0(pattern, "[^#]")
    valid <- grepl(rec_str, pattern = pattern)

    if(length(check) > 1) {
        return(valid)
    }

    # if I'm here then record is longer than check and we're on the last segment
    # so there can't be any # to the right of the check[1] positions
    valid && sum(record[-seq_len(check[1])] == "#") == 0
}

hash_inputs <- function(record, check) {
    rec_str <- paste0(record, collapse = "")
    chk_str <- pretty_print_vector(check)
    paste0(rec_str, ";", chk_str)
}

ca <- function(record, check, lookup = NULL) {

    if (!is.null(lookup)) {
        e <- hash_inputs(record, check)
        if (lookup$has(e)) {
            return(lookup$get(e))
        }
    }

    if (length(record) == 0) return(0)

    first_hash <- as.integer(regexpr(pattern = "#", text = paste0(record, collapse ="")))

    min_record <- check[1]
    if(first_hash > 0) {
        min_record <- max(min_record, length(record) - first_hash + 1)
    }
    if (length(check) == 1) {
        num_ways <- 0
        #found_hash <- FALSE

        while(length(record) >= min_record) {
            #seg_size <- min(length(record), check[1])
            #idx <- seq_len(seg_size)
            #rec_seg <- paste0(record[idx], collapse = "")
            
            #record <- record[-idx]
            # in order to be valid on the last pass there can't be any following #s not accounted for.
            if( is_valid_segment(record, check) ) {
                num_ways <- num_ways + 1
            }
            record <- record[-1]

        }
        if(!is.null(lookup)) {
            lookup$set(e, num_ways)
        }        
        return(num_ways)
    }

    num_ways <- 0L
    while (length(record) >= min_record) {
        #seg_size <- min(length(record), check[1])
        idx <- seq_len(check[1])
        #rec_seg <- paste0(record[idx], collapse = "")
        #record <- record[-idx]
        if (is_valid_segment(record, check)) {
            num_ways <- num_ways + ca(record[-c(1, 1 + idx)], check[-1], lookup)
        }
        record <- record[-1]
    }
    if (!is.null(lookup)) {
        lookup$set(e, num_ways)
    }
    num_ways
}


dup_inputs <- function(inputs) {
    lapply(
        inputs, 
        \(x) list(
            record = strsplit(paste0(rep(paste0(x$record, collapse = ""), 5), collapse = "?"), "")[[1]],
            check = rep(x$check, 5)
            )
    )
}

test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")

example01_answers <- c(1, 4, 1, 1, 4, 10)

for (i in seq_along(example01_answers)) {
    test_label <- paste0("Test ", i, ": ", paste0(c("'", test_input[[i]]$record, "'"), collapse = ""), " ", pretty_print_vector(test_input[[i]]$check))

    test_example(
    \(x) do.call(ca, x),
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

#more testing. Let's check all of the inputs with 8 '?' are fewer with count_slowly
if (FALSE)
for (i in seq_along(input)) {
    num_qs <- sum(input[[i]]$record == '?')
    #cat(i, ": ", sep = "")
    if (num_qs < 8 | num_qs > 10) {
        #cat("skip", "\n")
        next
    }

    correct_answer <- count_slowly(input[[i]]$record, input[[i]]$check)
    fast_answer <- ca(input[[i]]$record, input[[i]]$check)

    if (fast_answer == correct_answer) {
        #cat("OK", "\n")
    } else {
        cat(i,"DEAD!\n")
        cli::cli_alert_danger("num_qs: {num_qs} => record: {input[[i]]$record} check: {input[[i]]$check}\nfast: {fast_answer} correct: {correct_answer}")

    }        

    
}

cat(cli::col_green("Part 2 Test Cases"), "\n")

test_input2 <- dup_inputs(test_input)

example02_answers <- c(1, 16384, 1, 16, 2500, 506250)
lookup <- dict()
ca2 <- function(record, check) ca(record, check, lookup)
for (i in seq_along(example01_answers)) {
    test_label <- paste0("Test ", i, ": ", paste0(c("'", test_input2[[i]]$record, "'"), collapse = ""), " ", pretty_print_vector(test_input2[[i]]$check))

    test_example(
    \(x) do.call(ca2, x),
    test_input2[[i]],
    example02_answers[i],
    label =  test_label   
    )
}

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

tic <- Sys.time()
arrangements <- unlist(lapply(input, \(x) do.call(ca, x)))
toc <- Sys.time()
cli::cli_alert_success("Elap time {toc - tic}")
cli::cli_alert_info("The Part 1 answer is {sum(arrangements)}")

lookup <- dict()
ca2 <- function(record, check) ca(record, check, lookup)

cli::cli_alert_info("Trying with dynamic programming")
tic <- Sys.time()
arrangements2 <- unlist(lapply(input, \(x) do.call(ca2, x)))
toc <- Sys.time()
cli::cli_alert_success("Elap time {toc - tic}")
#slow_arrangements <- unlist(lapply(input, \(x) do.call(count_slowly, x)))

cli::cli_alert_info("The Part 1 answer is {sum(arrangements2)}")
#cli::cli_alert_info("The SLOW answer is {sum(slow_arrangements)}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


#lookup <- dict()
#ca2 <- function(record, check) ca(record, check, lookup)
input2 <- dup_inputs(input)
cli::cli_alert_info("Trying with dynamic programming")
tic <- Sys.time()
arrangements2 <- unlist(lapply(input2, \(x) do.call(ca2, x)))
toc <- Sys.time()
elap_time <- toc - tic

cli::cli_alert_success("Elap time {elap_time} {attr(elap_time, 'units')}")
opts <- options(scipen = 999)
cli::cli_alert_info("The Part 2 answer is {sum(arrangements2)}")
cat("\n")