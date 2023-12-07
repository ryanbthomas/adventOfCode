base::load("common_functions.Rdata")

day_folder <- "2021/05"
extract_points <- function(x, pattern = "[0-9]+") {
    tmp <- regmatches(x, gregexec(pattern, x, perl = TRUE))
    f <- function(x) {
        a <- as.integer(as.vector(x)) + 1 # move from index to 1
        list(a[c(2, 1)], a[c(4, 3)])
    }
    lapply(tmp, f)
}
update_max_points <- function(mp, pt) {
    y <- pmax(mp[1], pt[[1]][1], pt[[2]][1])
    x <- pmax(mp[2], pt[[1]][2], pt[[2]][2])
    c(y, x)
}

filter_lines <- function(pts, cond = \(x) TRUE) {
    keep_points <- vector("logical", length(pts))
    max_dims <- c(0, 0)
    for (idx in seq_along(pts)) {
        keep_points[idx] <- cond(pts[[idx]])

        if (keep_points[idx]) {
            max_dims <- update_max_points(max_dims, pts[[idx]])
        }
    }
    list(lines = pts[keep_points], max_dims = max_dims)
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(extract_points))
}


test_input <- input_parser("example_input")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
   function(x) filter_lines(x)[["max_dims"]],
   test_input[[1]],
   c(10, 10),
   label = "Test 1 Max Dims"
)
gcd <- function(x, y) {
    r <- x %% y
    return(ifelse(r, gcd(y, r), y))
}

no_diag <- function(pt) {
    pt[[1]][1] == pt[[2]][1] || pt[[1]][2] == pt[[2]][2]
}

gen_line_pts2 <- function(pts) {
    rise <- as.integer(pts[[2]][1] - pts[[1]][1])
    run <- as.integer(pts[[2]][2] - pts[[1]][2])
    if (run == 0) {
        vert_moves <- seq(from = pts[[1]][1], to = pts[[2]][1], by = ifelse(rise >= 0, 1, -1))
        return(lapply(vert_moves, function(x) c(x, pts[[1]][2])))
    }

    if (rise == 0) {
        hort_moves <- seq(from = pts[[1]][2], to = pts[[2]][2], by = ifelse(run >= 0, 1, -1))
        return(lapply(hort_moves, function(x) c(pts[[1]][1], x)))
    }
    #print(paste0("rise: ", rise))
    #print(paste0("run: ", run))
    g <- abs(gcd(rise, run))
    #print(paste0("gcd(rise, run): ", g))
    
    #by <- ifelse(run >= 0, 1, -1)
    #if
    steps <- seq(from = 0, to = run, by = run %/% g)
    #print(steps)
    lapply(steps, function(x) c(rise/run * x + pts[[1]][1], pts[[1]][2] + x))    

}

gen_line_pts <- function(pts, verbose = Sys.getenv("DEBUG", "FALSE")) {
    rise <- pts[[2]][1] - pts[[1]][1]
    vert_step <- ifelse(rise >= 0, 1, -1)
    run <- pts[[2]][2] - pts[[1]][2]
    hort_step <- ifelse(run >= 0, 1, -1)
    line_pts <- list()
    for (i in seq(from = 0, to = run, by = hort_step)) {
        #print("i:", i, "\n")
        for (j in seq(from = 0, to = rise, by = vert_step)) {
            #print("j:", j, "\n")
            pt <- c(pts[[1]][1] + j, pts[[1]][2] + i)
            if (verbose) cli::cli_alert_info("considering {pt}")
            cond_part2 <- abs(pt[1] - (rise/run * (pt[2]-pts[[1]][2]) + pts[[1]][1]))
            if (verbose) cli::cli_alert_info("run: {run}; cond_part2: {cond_part2}")
            if (abs(run) < 0.01 || cond_part2 < 0.01) {
                #cli::cli_alert_info("adding {pt} to line_pts")
                line_pts <- append(line_pts, list(pt))
            }
        }
    }
    line_pts
}
make_vent_map <- function(line_segs, max_dims) {
    grid <- matrix(0, nrow = max_dims[1], ncol = max_dims[2])
    for (line_seg in line_segs) {
        #print(line_seg)
        line_seg_pts <- gen_line_pts2(line_seg)
        #print("All points")
        #print(line_seg_pts)
        for (pt in line_seg_pts) {
            #print(paste0("adding ", pretty_print_vector(pt)))
            # the +1 is necessary to go from zero based indexing
            # in the problem to the 1 based indexing in R
            grid[pt[1], pt[2]] <-  grid[pt[1], pt[2]] + 1
        }
        #print(grid)
    }
    grid
}

fin <- filter_lines(test_input[[1]])

test_example(
    function(x) {
        grid <- make_vent_map(x$lines, x$max_dims)
        sum(grid >= 2)
    },
    fin,
    12L,
    label = "Test 2 no diag Dangeous points"

)


test_example(
    function(x) filter_lines(x, cond = no_diag)[["max_dims"]],
    test_input[[1]],
    c(10, 10),
    label = "Test 3 Max Dims no diag"
)

filtered_input <- filter_lines(test_input[[1]], cond = no_diag)

test_example(
    function(x) {
        grid <- make_vent_map(x$lines, x$max_dims)
        sum(grid >= 2)
    },
    filtered_input,
    5L,
    label = "Test 4 no diag Dangeous points"

)

#cat(cli::col_green("Part 2 Test Cases"), "\n")


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")

input <- input_parser("input")

no_diags <- filter_lines(input[[1]], no_diag)

grid <- make_vent_map(no_diags$lines, no_diags$max_dims)

num_dangerous_points <- sum(grid >= 2)


cli::cli_alert_info("The Part 1 answer is {num_dangerous_points}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

all_lines <- filter_lines(input[[1]])
grid <- make_vent_map(all_lines$lines, all_lines$max_dims)

num_dangerous_points <- sum(grid >= 2)

cli::cli_alert_info("The Part 2 answer is {num_dangerous_points}")
cat("\n")