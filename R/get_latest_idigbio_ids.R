sequenced_uf_numbers <- function(db_file) {
    db <- dplyr::src_sqlite(db_file, create = FALSE)

    seq_data <- dplyr::tbl(db, "sequence_data")
    plate_data <- dplyr::tbl(db, "plate_data")

    res <- dplyr::left_join(seq_data, plate_data) %>%
        dplyr::collect() %>%
        dplyr::mutate(idig_key = paste(ufid, tolower(phylum), sep = "-")) %>%
        dplyr::select(plate, row, column, idig_key, sequence)

    res
}

idigbio_query <- function(uf_numbers) {
    idigbio_fields <- function() {
        c('uuid',
          'catalognumber',
          'datecollected',
          'institutioncode',
          'phylum',
          'data.dwc:phylum',
          'data.dwc:class',
          'data.dwc:order',
          'data.dwc:family',
          'data.dwc:genus',
          'scientificname',
          'datecollected',
          'country',
          'geopoint')
    }

    qry <- list(
        catalognumber = as.list(uf_numbers$idig_key),
        institutioncode = "uf",
        collectioncode = "invertebrate zoology"
    )

    ridigbio::idig_search_records(rq = qry, fields = idigbio_fields())
}

id_table <- function(uf_numbers, idig) {
    dplyr::left_join(uf_numbers, idig, by = c("idig_key" = "catalognumber"))

}

export_fasta <- function(dt) {
    make_fasta <- function(plate, row, column, idig_key, scientificname, sequence) {
        lbl <- paste(plate, row, column, idig_key, scientificname, sep = "_")
        lbl <- gsub("\\s", "_", lbl)
        paste0(">", lbl, "\n", sequence, "\n")
    }
    res <- dt %>%
        dplyr::select(plate, row, column, idig_key, scientificname, sequence) %>%
        purrr::pmap(make_fasta)
    cat(unlist(res), file = "data/seqs_with_idigbio_ids.fasta", sep = "")
}
