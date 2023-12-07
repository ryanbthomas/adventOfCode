test_example <- function(f, x, expected, label, is_same = identical) {
    actual <- f(x)

    if (is_same(actual, expected)) {
        cli::cli_alert_success("{label} Passed.")
        return(invisible(actual))
    }

    cli::cli_alert_danger("{label} Failed.")
    cli::cli_inform("Expected: {expected}")
    cli::cli_inform("Actual: {actual}")
    
}
