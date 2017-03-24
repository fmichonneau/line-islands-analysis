list_csv_files <- function(path) {
    list.files(path = path, pattern = "csv$", full.names = TRUE)
}

read_csv_files <- function(csv_files, ...) {
    res <- lapply(csv_files, function(x) read.csv(x, stringsAsFactors = FALSE, ...))
    names(res) <- csv_files
    res
}

hash_csv_files <- function(path) {
    csv_files <- list_csv_files(path)
    csv_content <- read_csv_files(csv_files)
    digest::digest(csv_content)
}
