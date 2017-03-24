db_structure <- list(
    plate_data =
        "CREATE TABLE `plate_data` (
          `plate`	TEXT,
	  `row`	        TEXT,
	  `column`	TEXT,
	  `phylum`	TEXT,
	  `ufid_unique`	TEXT,
	  `ufid`	TEXT,
	  `filled_by`	TEXT,
	  `notes`	TEXT,
          PRIMARY KEY(plate, row, column)
        );",
    sequence_data =
        "CREATE TABLE `sequence_data` (
	   `plate`              text,
	   `row`                text,
	   `column`	        text,
           `sequence`           text,
           `sequence_attempt`   text,
           FOREIGN KEY (plate, row, column)  REFERENCES plate_data(plate, row, column)
        );",
    sequence_contaminations =
        "CREATE TABLE `sequence_info` (
           `plate`        text,
           `row`	   text,
	   `column`	   text,
           `sequence_contaminated` boolean,
           `sequence_note`         text,
         FOREIGN KEY (plate, row, column)  REFERENCES plate_data(plate, row, column)
        );",
    dictionary_phyla_id =
        "CREATE TABLE `dictionary_phyla_id` (
	`phyla_ID`	TEXT,
	`phylum`	TEXT
        );",
    plate_tracking =
        "CREATE TABLE `plate_tracking` (
	`plate`	        TEXT,
	`is_full`	TEXT DEFAULT '(null)',
	`sent_on`	TEXT,
	`received_on`	TEXT,
	`extracted_on`	TEXT,
        `notes`         TEXT
        );")
## TODO -- number_individuals should be defined here and have a foreign key associated with the ufdb

## @param con connection to database
## @param db_str database structure
## returns a vector of boolean indicating whether the tables listed in
## database structure query have been succesfully created
create_database <- function(con, db_str) {
    res <- lapply(db_str, function(qry) {
        dbSendQuery(conn=con, statement=qry)
        })
    crtd <- sapply(names(db_str), function(x) {
        dbExistsTable(conn=con, x)
    })
    crtd
}


## add data to an existing table in a database
add_data <- function(con, file, table, ...) {
    if (!file.exists(file)) {
        stop(file, " doesn't exist.")
    }
    tab <- read.csv(file = file, stringsAsFactors = FALSE)
    dbWriteTable(conn=con, value=tab, name=table, row.names=FALSE,
                 overwrite=FALSE, append=TRUE)
}

import_fasta <- function(file, prefix="FMSLI13", db_prefix="FMSL13_",
                         attempt=1, con, dry_run = TRUE) {
    fcon <- readLines(file)
    titles <- grep(">", fcon)
    seqData <- fcon[setdiff(seq_len(length(fcon)), titles)]
    if (length(titles) != length(seqData)) {
        stop("Each sequence should be on a single line.")
    } else {
        ids <- lapply(strsplit(fcon[titles], "_"), function(x) {
            yy <- grep(prefix, x)
            if(length(yy) > 0) {
                plate <- x[yy+1]
                rowL <- gsub("([A-H]{1})(\\d{2})(.*)", "\\1", x[yy+2])
                colL <- gsub("([A-H]{1})(\\d{2})(.*)", "\\2", x[yy+2])
                c(plate, rowL, colL)
            }
        })
        title_pb <- sapply(ids, is.null)
        if (any(title_pb)) {
            warning(paste(fcon[titles][title_pb], collapse=", "), " don't have the prefix, and are not imported.")
            seqData <- seqData[!title_pb]
        }
        res <- do.call("rbind", ids)
        res <- data.frame(plate = paste0(db_prefix, res[, 1]),
                          row = res[, 2],
                          column = gsub("^0{1}", "", res[, 3]),
                          sequence = seqData,
                          sequence_attempt = attempt,
                          stringsAsFactors=FALSE)
        stopifnot(all(res$sequence_row %in% LETTERS[1:8]) &&
                  all(res$sequence_column %in% as.character(1:12)))
        if (dry_run) {
            return(res)
        } else {
            dbWriteTable(conn=con, value=res, name="sequence_data", overwrite=FALSE,
                         row.names=FALSE, append=TRUE)
        }
    }
}

get_latest_file <- function(path, pattern) {
    res <- list.files(path = path, pattern = paste0(pattern, "_?(.+)csv$"),
                      full.name = TRUE)
    res <- res[length(res)]
    if (length(res) != 1)
        stop("No file or more than 1 file for ", pattern)
    message("Using ", res, " for ", sQuote(pattern), ".")
    res
}


write_tables <- function(con, path, patterns) {
    lapply(patterns, function(x) {
        csv_file <- get_latest_file(path, x)
        dbWriteTable(conn = con, value = read.csv(csv_file),
                     name = x, row.names=FALSE, overwrite=TRUE)
    })
}

create_sli_database <- function(database_file) {
    if (file.exists(database_file))
        file.remove(database_file)

    con <- dbConnect(RSQLite::SQLite(), database_file)

    create_database(con, db_structure)

    plate_data <- list.files(path="data-raw-csv", pattern="^plate_(.+)csv$",
                             full.names=TRUE)
    lapply(plate_data, function(x) add_data(con, table="plate_data", file=x))

    add_data(con, table = "sequence_contaminations",
             file = "data-raw-csv/sequence_contaminations.csv")

    write_tables(con, "data-raw-csv",
                 c("tracking_info", "dictionary_phyla_id",
                   "specimens", "stations", "ufdb", "number_individuals",
                   "sli_specimens_live_head"))

    import_fasta(file="data-raw-fasta/FMSLI_plates1-6_10-18_cleaned.fasta", con=con, dry_run = FALSE)
    dbDisconnect(con)
}
