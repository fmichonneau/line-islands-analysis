## 2018-03-09 --- the FASTA file Matt sent was messy, clean it up before trying to import it
## in database

cleanup_fasta <- function(dirty_fasta, clean_fasta) {

    fst_file <- ape::read.dna(dirty_fasta, format = "fasta")
    fst_ids <- dimnames(fst_file)[[1]]

    fst_df <- tibble::tibble(ids = fst_ids) %>%
        dplyr::mutate(fst_tidy = purrr::map(ids, function(x) {
            if (grepl("XXXX", x)) {
                plate_col <- substr(x, 1, 1)
                plate_row <- substr(x, 2, 3)
                plate_id <- unlist(strsplit(x, "_"))[2:3]
                res <- tibble::tibble(
                    plate_id = paste(plate_id, collapse = "_"),
                    plate_col = plate_col,
                    plate_row = plate_row
                )
            } else if (grepl("^FMSL13", x)) {
                tmp_id <- unlist(strsplit(x, "_"))
                res <- tibble::tibble(
                    plate_id = paste(tmp_id[1], tmp_id[2], sep = "_"),
                    plate_col = tmp_id[3],
                    plate_row = tmp_id[4]
                )
            } else stop("problem")
            res
        })) %>%
        tidyr::unnest() %>%
        ## prefix in database for plates is FMSL not FMSLI
        mutate(plate_id = gsub("FMSLI", "FMSL", plate_id),
               new_label = paste0(plate_id, "_", plate_col, sprintf("%.2d", as.numeric(plate_row))))

    dimnames(fst_file)[[1]] <- fst_df$new_label
    ape::write.dna(fst_file, file = clean_fasta,
                   format = "fasta", colsep = "", colw = 100000)
    invisible(clean_fasta)
}
