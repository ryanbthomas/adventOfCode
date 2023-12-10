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

traverse_pipes <- function(input, debug = FALSE) {
    cycle_len <- 0L

    start_node <- input$start
    tmp <- find_first_pipe(input$map, start_node, debug = debug)
    node <- tmp[["node"]]
    entry_dir <- tmp[["entry_dir"]]
    while(!all(node == start_node)) {
        cycle_len <- cycle_len + 1L
        tmp <- find_next_segment(input$map, node, entry_dir = entry_dir, debug = debug) 
        node <- tmp[["node"]]
        entry_dir <- tmp[["entry_dir"]]
    }
    cycle_len
}

test_input01 <- input_parser("example01")
test_input02 <- input_parser("example02")

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


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

total_pipe_length <- traverse_pipes(input)
mid_point <- total_pipe_length %/% 2 + 1

cli::cli_alert_info("The Part 1 answer is {mid_point}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


cli::cli_alert_info("The Part 2 answer is ")
cat("\n")