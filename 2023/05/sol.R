base::load("common_functions.Rdata")

library(bit64)
day_folder <- "2023/05"

create_map_function <- function(lines, debug = FALSE) {
    #func_name <- gsub(strsplit(lines[1], " ")[[1]][1], pattern = "-", replacement = "_")
    tmp <- lapply(strsplit(lines, " "), as.integer64) |>
        lapply(function(x) data.frame(dest_start = x[1], src_start = x[2], rng = x[3]))
    lookup <- Reduce(rbind, tmp)
    lookup <- lookup[order(lookup$src_start), ]
    if (debug) return(lookup)
    function(src) {
        dest <- src
        #browser()
        for (i in seq_along(src)) {
            idx <- src[i] >= lookup$src_start & src[i] <= lookup$src_start + lookup$rng - 1
            if (any(idx)) {
                lookup_idx <- seq_len(nrow(lookup))[idx]

                dest[i] <- lookup[lookup_idx, 1] + (src[i] - lookup[lookup_idx, 2])
            }
        }
        unique(dest)
    }    
}
create_map_function_segs <- function(lines, debug = FALSE) {
    #func_name <- gsub(strsplit(lines[1], " ")[[1]][1], pattern = "-", replacement = "_")
    tmp <- lapply(strsplit(lines, " "), as.integer64) |>
        lapply(function(x) data.frame(dest_start = x[1], src_start = x[2], rng = x[3]))
    lookup <- Reduce(rbind, tmp)
    lookup <- lookup[order(lookup$src_start), ]
    if (debug) return(lookup)
    function(src) {
        dest <- vector("list", length(src))
        #browser()
        for (i in seq_along(src)) {
            start <- src[[i]][1]
            len <- src[[i]][2]

            tmp <- list()
            # find smallest src_start larger than start
            idx <- 1L
            while(len > as.integer64(0)) {
                
                if (lookup$src_start[idx] >= start + len) {
                    cli::cli_alert_info("completely below start: adding ({start}, {len})")
                    tmp <- append(tmp, list(c(start, len)))
                    len <- as.integer64(0)
                }else if(lookup$src_start[idx] > start) {
                    dest_start <- start
                    dest_len <- lookup$src_start[idx] - start

                    cli::cli_alert_info("adding from lookup({dest_start}, {dest_len})")
                    tmp <- append(tmp, list(c(dest_start, dest_len)))
                    start <- lookup$src_start[idx]
                    len <- len - dest_len
                    
                }else if(lookup$src_start[idx] <= start & lookup$src_start[idx] + lookup$rng[idx] - as.integer64(1) >= start) {
                    len_in_rng <- min(len, lookup$rng[idx] + (lookup$src_start[idx] - start))
                    dest_start <- lookup$dest_start[idx] + start - lookup$src_start[idx]
                    dest_len <- len_in_rng
                    cli::cli_alert_info("Range found: adding ({dest_start}, {dest_len})")
                    tmp <- append(tmp, list(c(dest_start, dest_len)))
                    start <- start + len_in_rng
                    len <- len - len_in_rng
                    idx <- idx + 1
                }else if(idx > nrow(lookup)) {
                    cli::cli_alert_info("no more lookup: ({start}, {len})")
                    tmp <- append(tmp, list(c(start, len)))
                    len <- as.integer64(0)
                
                } else if(lookup$src_start[idx] + lookup$rng[idx] <= start) {
                    idx <- idx + 1
                } else {
                    cli::cli_alert_danger("WTF: start={start}, len={len}, idx={idx}")
                }
                #idx <- idx + 1


            }
            dest[[i]] <- tmp
        }
        do.call(c, dest)
    }    
}

input_parser <- function(file, debug = FALSE) {
    lines <- readLines(file.path(day_folder, file))

    seeds <- as.integer64(regexprmatch(lines[1], pattern = "[0-9]+")[[1]])
    map_lines <- lines[-c(1,2)] 
    num_functions <- sum(map_lines == "") + 1

    func_list <- vector("list", num_functions)

    func_names <- character(num_functions)

    is_blank <- map_lines == ""
    idx <- cumsum(is_blank) + 1
    idx[is_blank] <- 0

    for(map_idx in seq_len(num_functions)) {
        lines <- map_lines[idx == map_idx]
        func_names[map_idx] <- gsub(strsplit(lines[1], " ")[[1]][1], pattern = "-", replacement = "_")
        func_list[[map_idx]] <- create_map_function(lines[-1], debug)        
        }
    
    names(func_list) <- func_names

    append(list(seeds = seeds), func_list)
}

input_parser_segs <- function(file, debug = FALSE) {
    lines <- readLines(file.path(day_folder, file))

    seeds <- as.integer64(regexprmatch(lines[1], pattern = "[0-9]+")[[1]])
    seeds <- lapply(seq(1, length(seeds), by = 2), function(idx) c(seeds[idx], seeds[idx+1]))
    map_lines <- lines[-c(1,2)] 
    num_functions <- sum(map_lines == "") + 1

    func_list <- vector("list", num_functions)

    func_names <- character(num_functions)

    is_blank <- map_lines == ""
    idx <- cumsum(is_blank) + 1
    idx[is_blank] <- 0

    for(map_idx in seq_len(num_functions)) {
        lines <- map_lines[idx == map_idx]
        func_names[map_idx] <- gsub(strsplit(lines[1], " ")[[1]][1], pattern = "-", replacement = "_")
        func_list[[map_idx]] <- create_map_function_segs(lines[-1], debug)        
        }
    
    names(func_list) <- func_names

    append(list(seeds = seeds), func_list)
}
find_locations <- function(inputs, seed_func = function(x) x) {
    
    seeds <- seed_func(inputs[["seeds"]])
    start <- seeds
    for (f_name in names(inputs[-1])) {
        cli::cli_alert_info("Using {f_name} map")
        start <- inputs[[f_name]](start)
    }
    start
}

batch_search <- function(inputs) {
    use_inputs <- inputs
    min_location <- as.integer64(199602917)

    for (i in seq(from = 1, to = length(inputs[["seeds"]]), by = 2)) {
        seed_start <- inputs[["seeds"]][i]
        seed_length <- inputs[["seeds"]][i + 1]
        cli::cli_alert_info("batch {i}: {seed_start}, {seed_length}")
        use_inputs[["seeds"]] <- seq(from = seed_start, to = seed_start + seed_length - 1)
        cli::cli_alert_info("Updated seed")
        new_locations <- find_locations(use_inputs)
        cli::cli_alert_info("Found batch {i} new locations")
        min_new_locations <- min(new_locations)
        min_location <- pmin(min_location, min_new_locations)
        cli::cli_alert_info("New min is {min_location}")
    }
    min_location
}

find_src_range <- function(tbl, upper = Inf) {
    idx <- tbl$dest_start + tbl$rng - 1 <= upper 
    #lower_bound <- tbl$dest_start >= rng[1]
    #idx <- upper_bound & lower_bound
    #c(min(tbl$src_start[idx]), max(tbl$src_start[idx] + tbl$rng[idx] - 1))
    max(tbl$src_start[idx] + tbl$rng[idx] - 1)
}

find_seed_search_range <- function(inputs, max_dest = as.integer64(199602917)) {
    upper <- max_dest
    for(func_idx in seq(8, 2, -1)) {
        upper <- find_src_range(inputs[[func_idx]], upper)
    }
    upper
}
seed_range_func <- function(x) {
    start <- x[seq_along(x) %% 2 == 1]
    len <- x[seq_along(x) %% 2 == 0]
    lst <- lapply(seq_along(start), function(x) seq(from = start[x], length.out = len[x]))
    unique(do.call(c, lst))
}

test_input <- input_parser("example01")
test_input_segs <- input_parser_segs("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  find_locations,
  test_input,
  as.integer64(c(82L, 43L, 86L, 35L)),
  label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
  function(x) {
    locs <- find_locations(x)
    start_locs <- lapply(locs, function(x) x[1])
    do.call(min, start_locs)
  },
  test_input_segs,
  as.integer64(46),
  label = "Test 2 : seed range func"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")
input_segs <- input_parser_segs("input")
locations <- find_locations(input)

answer <- min(locations)

cli::cli_alert_info("The Part 1 answer is {answer}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


locs <- find_locations(input_segs)
start_locs <- lapply(locs, function(x) x[1])
min_locations <- do.call(min, start_locs)

cli::cli_alert_info("The Part 2 answer is {min_locations}")
cat("\n")