2023 Advent of Code Day 01
================

# The Puzzle

## — Day 1: Trebuchet?! —

Something is wrong with global snow production, and you’ve been selected
to take a look. The Elves have even given you a map; on it, they’ve used
stars to mark the top fifty locations that are likely to be having
problems.

You’ve been doing this long enough to know that to restore snow
operations, you need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on
each day in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

You try to ask why they can’t just use a weather machine (“not powerful
enough”) and where they’re even sending you (“the sky”) and why your map
looks mostly blank (“you sure ask a lot of questions”) and hang on did
you just say the sky (“of course, where do you think snow comes from”)
when you realize that the Elves are already loading you into a trebuchet
(“please hold still, we need to strap you in”).

As they’re making the final adjustments, they discover that their
calibration document (your puzzle input) has been amended by a very
young Elf who was apparently just excited to show off her art skills.
Consequently, the Elves are having trouble reading the values on the
document.

The newly-improved calibration document consists of lines of text; each
line originally contained a specific calibration value that the Elves
now need to recover. On each line, the calibration value can be found by
combining the first digit and the last digit (in that order) to form a
single two-digit number.

For example:

    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet

In this example, the calibration values of these four lines are 12, 38,
15, and 77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of the
calibration values?

## Part One Solution

My first solution to this problem split each line into individual
characters and then looked for numbers starting from the front and then
the end of the line.

``` r
parser <- function(x) {
    strsplit(x, "")
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}

find_first_digit <- function(x) {
    for(i in seq_along(x)) {
        if (grepl('[0-9]', x[i])) return(x[i])
    }
}

make_num_from_first_and_last <- function(x) {
    as.integer(paste0(find_first_digit(x), find_first_digit(rev(x))))
}
```

The answer was then calculated using the following code.

``` r
input <- input_parser("input")
nums <- unlist(lapply(input[[1]], make_num_from_first_and_last))
result1 <- sum(nums)
```

Your puzzle answer was 54877.

## Alternative Methods

### Method 2

I realized I could improve the code by treating the string as a whole
and using regular expressions.

``` r
parser2 <- function(x) {
    x
}

input_parser2<- function(file) {
    read_input(file.path(day_folder, file), list(parser2))
}

find_digits <- function(x, pattern = "[0-9]") {
    tmp <- regmatches(x, gregexec(pattern, x, perl = TRUE))
    num_str <- lapply(tmp, function(x) paste0(x[1], x[length(x)]))
    as.integer(unlist(num_str))
}
```

The answer was then calculated using the following code.

``` r
input2 <- input_parser2("input")
nums <- find_digits(input2[[1]])
result2 <- sum(nums)
```

### Method 3

This solution comes from Adam Austin via mastodon
[https://github.com/ataustin/advent-of-code/blob/main/y2023/d01/solution.R]()

``` r
decode <- function(input) {
    nums <- gsub("[a-z]", "", input)
    digs <- paste0(substr(nums, 1, 1), substr(nums, nchar(nums), nchar(nums)))
    sum(as.integer(digs))
}
```

The full solution is calculated by

``` r

input <- readLines("input")
decode(input)
```

## Comparison of methods

| method   |      elap |
|:---------|----------:|
| method 1 | 0.5647359 |
| method 2 | 0.2746937 |
| method 3 | 0.0183895 |

## — Part Two —

Your calculation isn’t quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six,
seven, eight, and nine also count as valid “digits”.

Equipped with this new information, you now need to find the real first
and last digit on each line. For example:

    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and
76. Adding these together produces 281.

What is the sum of all of the calibration values?

## Part Two Solution

Your puzzle answer was 54100.
