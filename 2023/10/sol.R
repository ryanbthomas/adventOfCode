base::load("common_functions.Rdata")

day_folder <- "2023/10"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    mat_rows <- strsplit(lines, "")

    map <- Reduce(rbind, mat_rows)

    start <- which(map == "S", arr.ind = TRUE)
    #s_col <- (idx - 1L) %% ncol(map) + 1L
    #s_row <- idx %/% ncol(map) + (s_col < ncol(map))

    list(
        start = start[1, ],
        map = map
    )
}

rev_direction <- function(d) {
    opp <- c(N = "S", S = "N", E = "W", W = "E")
    opp[d]
}

find_next_segment <- function(map, node, entry_dir = NULL, debug = FALSE) {

    current_pipe <- map[node["row"], node["col"]]
    # the directions below are reversed.
    # the idea is if you approach a '|' from the south the next move
    # must be north
    moves <- list(
        "N" = c(row = -1L, col = 0L),
        "E" = c(row = 0L, col = 1L), 
        "S" = c(row = 1L, col = 0L),         
        "W" = c(row = 0L, col = -1L)    
    )
    exit_direction <- list(
        "|" = c(S = "N", N = "S"),
        "-" = c(W = "E", E = "W"),
        "J" = c(W = "N", N = "W"),
        "F" = c(S = "E", E = "S"),
        "L" = c(N = "E", E = "N"),
        "7" = c(W = "S", S = "W")
    )

    exit_dir <- exit_direction[[current_pipe]][entry_dir]
    pos <- pretty_print_vector(node) 
    if (debug) cli::cli_alert_info("{entry_dir} -> {pos} {current_pipe} -> {exit_dir}")
    list(node = node + moves[[exit_dir]], entry_dir = rev_direction(exit_dir))   

}

observe_adjacent_nodes <- function(map, node) {
    moves <- list(
        "N" = c(row = -1L, col = 0L),
        "E" = c(row = 0L, col = 1L), 
        "S" = c(row = 1L, col = 0L),         
        "W" = c(row = 0L, col = -1L)    
    )
    
    new_nodes <- lapply(moves, \(d) node + d)

    ridx <- vapply(new_nodes, \(n) n["row"] >=1 && n["row"] <= nrow(map), FUN.VALUE = TRUE)
    cidx <- vapply(new_nodes, \(n) n["col"] >=1 && n["col"] <= ncol(map), FUN.VALUE = TRUE)

    new_nodes[ridx & cidx]
}

find_first_pipe <- function(map, node, entry_dir = NULL, debug = FALSE) {
    moves <- list(
        "N" = list(move = c(row = -1L, col = 0L), valid = c("7", "|", "F")),
        "E" = list(move = c(row = 0L, col = 1L), valid = c("J", "-", "7")),
        "S" = list(move = c(row = 1L, col = 0L), valid = c("J", "|", "L")),
        "W" = list(move = c(row = 0L, col = -1L), valid = c("F", "-", "L"))
    )
    directions <- setdiff(names(valid_moves), entry_dir)

    start_node <- NULL
    for(d in directions) {
        m <- moves[[d]]
        new_node <- node + m$move 

        if (new_node["row"] < 1 || new_node["row"] > nrow(map)) {   
            next()
        }
        if (new_node["col"] < 1 || new_node["col"] > ncol(map)) {   
            next()
        }

        idx <- map[new_node["row"], new_node["col"]] %in% m[["valid"]]
        if (idx) {
            new_pipe <- map[new_node["row"], new_node["col"]]
            if (debug) cli::cli_alert_info("found first pipe: {pretty_print_vector(new_node)}  '{new_pipe}'")
            return(list(node = new_node, entry_dir = rev_direction(d)))
        }

        if (map[new_node["row"], new_node["col"]] == "S") {
            start_node = list(node = new_node, entry_dir = rev_direction(d))
        }
    }

    # if we're here we should look for the start pos or soemthing has gone
    # wrong

    if (is.null(start_node)) {
        browser()
        stop("Danger Will Robinison!!!", call. = FALSE)
    }
    start_node
}

traverse_pipes <- function(input, output = "length", debug = FALSE) {
    cycle_len <- 0L
    
    start_node <- input$start
    tmp <- find_first_pipe(input$map, start_node, debug = debug)
    node <- tmp[["node"]]
    loop_nodes <- append(list(), list(node))
    entry_dir <- tmp[["entry_dir"]]
    while(!all(node == start_node)) {
        cycle_len <- cycle_len + 1L
        tmp <- find_next_segment(input$map, node, entry_dir = entry_dir, debug = debug) 
        node <- tmp[["node"]]
        loop_nodes <- append(loop_nodes, list(node))
        entry_dir <- tmp[["entry_dir"]]
    }
    if (output == "length") return(cycle_len)
    loop_nodes
}

pretty_print_map <- function(map, loop = NULL, to_file = "") {
    if (!is.null(loop)) loop <- vapply(loop, pretty_print_vector, FUN.VALUE = "")
    append_param <- ifelse(to_file == "", FALSE, TRUE)
    for (j in seq_len(nrow(map))) {
        r <- vector("character", ncol(map))
        for (i in seq_len(ncol(map))) {
            
            r[i] <- map[j, i]
            if (pretty_print_vector(c(row = j, col = i)) %in% loop) {
                r[i] <- crayon::green(map[j, i])
            }
            if (r[i] == "I") {
                r[i] <- crayon::red(r[i])
            }
        }
        cat(r, "\n", sep = "", file = to_file, append = append_param)
    }
    invisible(map)
}

replace_s <- function(map, loop) {
    s <- loop[[length(loop)]]
    end <- loop[[length(loop)-1]]
    start <- loop[[1]]

    start_d <- start - s
    end_d <- end - s
    exit_east <- data.frame(
      row = 0L,
      col = 1L,
      pipe = c("-", "L", "F") 
    )
    exit_west <- data.frame(
      row = 0L,
      col = -1L,
      pipe = c("-", "J", "7") 
    )

    exit_north <- data.frame(
      row = -1L,
      col = 0L,
      pipe = c("|", "J", "L") 
    )
    exit_south <- data.frame(
      row = 1L,
      col = 0L,
      pipe = c("|", "F", "7") 
    )
   all_exits <- Reduce(rbind, list(exit_east, exit_west, exit_north, exit_south))
   start_opts <- all_exits[all_exits$row == start_d["row"] & all_exits$col == start_d["col"], "pipe"]
   end_opts <- all_exits[all_exits$row == end_d["row"] & all_exits$col == end_d["col"], "pipe"]
   map[s["row"], s["col"]] <- intersect(start_opts, end_opts)
    map
}

count_enclosed_tiles <- function(input, map = input$map, debug = FALSE) {
    loop <- traverse_pipes(input, output = "loop")
    loop_str <- vapply(loop, pretty_print_vector, FUN.VALUE = "")
    map <- replace_s(map, loop)
    #prior_pipe <- ""
    #perimeter <- find_map_perimeter(map)

    for(j in seq_len(nrow(map))) {
        num_crossed_pipes <- 0L
        on_loop <- FALSE
        hort_seg <- c()        
        for(i in seq_len(ncol(map))) {
            node <- c(row = j, col = i)
            #if (map[j, i] == "O") next()

            if (!pretty_print_vector(node) %in% loop_str) {
                map[j, i] <- ifelse(num_crossed_pipes %% 2 == 0, "O", "I")
                on_loop <- FALSE
            
            } else if (map[j, i] == "|"){

                on_loop <- FALSE
                num_crossed_pipes <- num_crossed_pipes + 1L
            } else if (map[j, i] %in% c("L","F")) { # open a loop
                on_loop <- TRUE
                hort_seg <- append(hort_seg, map[j, i])
            } else if (map[j, i] %in% c("7", "J")) { # close a loop
                if ( all(c("L", "7") %in% c(hort_seg, map[j, i]))|| all(c("F", "J") %in% c(hort_seg, map[j, i]))) {
                    num_crossed_pipes <- num_crossed_pipes + 1L
                }
                on_loop <- FALSE
                hort_seg <- c()
                
            }

        }
    }
    if (debug) return(list(map = map, loop = loop))
    sum(map == "I")
}

find_map_perimeter <- function(map, loop = NULL) {
    
    ncols <- ncol(map)
    nrows <- nrow(map)
    
    top <- lapply(seq_len(ncols), \(i) c(row = 1L, col = i))
    bottom <- lapply(seq_len(ncols), \(i) c(row = nrows, col = i))

    left <- lapply(seq(2L, nrows -1L), \(j) c(row = j, col = 1L))
    right <- lapply(seq(2L, nrows - 1L), \(j) c(row = j, col = ncols))

    p <- c(top, right, bottom, left)

    idx <- vapply(p, \(n) list(n) %in% loop, FUN.VALUE = TRUE)

    p[!idx]
    
}

bfs_count_enclosed_tiles <- function(input, debug = FALSE) {
    loop <- traverse_pipes(input, output = "loop")
    #if (list(c(row = 1L, col = 1L)) %in% loop) {
    #    stop("Lucy! You got some 'splainin' to do.", call. = FALSE)
    #}

    map <- input$map
    fringe <- find_map_perimeter(map, loop)
    seen <- list()
    while(length(fringe) > 0) {
        node <- fringe[[1]]
        fringe <- fringe[-1]
        if (!list(node) %in% loop) {
            map[node["row"], node["col"]] <- "O"
            adj_nodes <- observe_adjacent_nodes(input$map, node)
            idx <- vapply(adj_nodes, \(n) list(n) %in% fringe || list(n) %in% seen, FUN.VALUE = TRUE)
            fringe <- append(fringe, adj_nodes[!idx])
        }
        seen <- append(seen, list(node))

    }
    map
}

test_input01 <- input_parser("example01")
test_input02 <- input_parser("example02")

test_input03 <- input_parser("example03")
test_input04 <- input_parser("example04")
test_input05 <- input_parser("example05")
test_input06 <- input_parser("example06")
cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  traverse_pipes,
  test_input01,
  7L,
  label = "Test 1"
)

test_example(
  traverse_pipes,
  test_input02,
  15L,
  label = "Test 2"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    count_enclosed_tiles,
    test_input03,
    4L,
    label = "Test 3"
)

test_example(
    count_enclosed_tiles,
    test_input04,
    4L,
    label = "Test 4"
)

test_example(
    count_enclosed_tiles,
    test_input05,
    8L,
    label = "Test 5"
)
test_example(
    count_enclosed_tiles,
    test_input06,
    10L, 
    label = "Test 6"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

total_pipe_length <- traverse_pipes(input)
mid_point <- total_pipe_length %/% 2 + 1

cli::cli_alert_info("The Part 1 answer is {mid_point}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

cnt <- count_enclosed_tiles(input)

cli::cli_alert_info("The Part 2 answer is {cnt}")
cat("\n")