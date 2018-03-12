## create a fasta file with sequences that have their IDs from the collection database

extract_new_ufid <- function(file = "data/sli_database.sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    ufdb <- tbl(con, "ufdb")

    collect(ufdb, n = Inf) %>%
        filter(grepl("[0-9]{5}[A-Z]{1}", Specimen.Notes)) %>%
        mutate(plated_id = regmatches(Specimen.Notes, gregexpr("[0-9]{5}[A-Z]{1,2}", Specimen.Notes))) %>%
        select(new_ufid = UFID, plated_id) %>%
        unnest()

}

generate_fasta <- function(file = "data/sli_database.sqlite", fasta_file) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file)

    seq_data <- tbl(con, "sequence_data") %>% collect()
    plate_data <- tbl(con, "plate_data") %>% collect()
    ufdb <- tbl(con, "ufdb") %>%
        collect() %>%
        mutate(UFID = as.character(UFID))


    new_ufid <- extract_new_ufid(file)

    plate_data_ <- plate_data %>%
        left_join(new_ufid, by = c("ufid_unique" = "plated_id")) %>%
        mutate(ufid = if_else(is.na(new_ufid), ufid, as.character(new_ufid)))

    res <- left_join(seq_data, plate_data_, by = c("plate", "row", "column")) %>%
        left_join(ufdb, by = c("phylum" = "Phyla.ID", "ufid" = "UFID"))

    out <- pmap_chr(res, function(plate, row, column, sequence, ufid, ufid_unique,
                              phylum, Genus, Species, Qualifier, ...) {
        glue::glue(">{phylum}-{Genus}-{Species}-{Qualifier}-{plate}_{row}{column}-{ufid_unique}-{ufid}
                     {sequence}")

    })
    cat(out, sep = "\n", file = fasta_file)
}


generate_tree <- function(file, tree_file, ...) {

    seq <- ape::read.dna(file = file, format = "fasta", ...)
    tr <- ape::nj(ape::dist.dna(seq, model = "raw"))
    ape::write.tree(tr, file = tree_file)


}
