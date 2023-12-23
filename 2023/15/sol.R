base::load("common_functions.Rdata")

day_folder <- "2023/15"


input_parser <- function(file) {
    tmp <- readLines(file.path(day_folder, file))
    unlist(strsplit(tmp, ","))
}


hash <- function(s, val = 0) {
    
    next_val <- ((val + utf8ToInt(substr(s, 1, 1))) * 17) %% 256
    if (nchar(s) == 1 ) {
        return(next_val)
    }
    hash(substr(s, 2, nchar(s)), next_val)
}

# part 2 functions
decode_lens_op <- function(input) {
    add_op <- grepl(input, pattern = "=")
    label <- regexprmatch(input, pattern = "^[a-z]+")
    focal_length <- rep(NA_integer_, length(input))
    focal_length[add_op] <- as.integer(unlist(regexprmatch(input, pattern = "[0-9]+")))

    data.frame(op = ifelse(add_op, "add", "rm"), label = unlist(label), focal_length = focal_length)
}

setup_boxes <- function() {
    e <- new.env()
    for (box in seq(0, 255)) {
        e[[as.character(box)]] <- list()
    }
    e
}

init_lens_boxes <- function(input) {
    steps <- decode_lens_op(input)
    boxes <- setup_boxes()

    for (s in seq_len(nrow(steps))) {
        label <- steps[s, "label"]
        box_id <- as.character(hash(label))
        if (steps[s, "op"] == "add") {
            boxes[[box_id]][[label]] <- steps[s, "focal_length"]
        } else if (label %in% names(boxes[[box_id]])){
            # op is remove and label exists in box
            boxes[[box_id]][[label]] <- NULL
        }
    }
    boxes
}

calc_box_focus_power <- function(box) {
    if (length(box) == 0) return(0)
    v <- unlist(box)
    sum(v * seq_along(v))

}

calc_focal_power <- function(boxes) {
    fp <- 0
    for(boxid in names(boxes)) {
        fp <- fp + (as.integer(boxid) + 1) * calc_box_focus_power(boxes[[boxid]])
    }
    fp
}

hashmap <- function(input) {
    boxes <- init_lens_boxes(input)
    calc_focal_power(boxes)
}

cat(cli::col_green("Part 1 Test Cases"), "\n")

test_input <- input_parser("example01")

part1_example_answers <- c(30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231)

for (i in seq_along(test_input)) {
    test_example(
    hash,
    test_input[i],
    part1_example_answers[i],
    label = paste0("Test Part 1.",i,": ", test_input[i] , " => ", part1_example_answers[i])
    )
}
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    hashmap,
    test_input,
    145,
    label = "Test Part 2: hashmap"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

hashes <- vapply(input, hash, FUN.VALUE = 0)

cli::cli_alert_info("The Part 1 answer is {sum(hashes)}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

focal_power <- hashmap(input)
# 229878 too high
cli::cli_alert_info("The Part 2 answer is {focal_power}")
cat("\n")