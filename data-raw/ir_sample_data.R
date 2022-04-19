## code to prepare `sample_data` dataset goes here

# import raw data
d_mir <- utils::read.csv("./inst/extdata/klh_hodgkins_mir.csv",
                         header = TRUE)

d_reference <- utils::read.csv("./inst/extdata/klh_hodgkins_reference.csv",
                               header = TRUE,
                               as.is = TRUE)

# reformat spectra and metadata

colnames(d_mir)[1] <- "x"
d_mir <- ir_stack(d_mir)

metadata <-
  tibble::tibble(
    id_measurement = seq_along(d_mir$spectra),
    id_sample = d_reference$Sample.Name,
    sample_type = d_reference$Category,
    sample_comment = d_reference$Description,
    klason_lignin = units::set_units(d_reference$X..Klason.lignin..measured./100, "1"),
    holocellulose = units::set_units(d_reference$X..Cellulose...Hemicellulose..measured./100, "1")
  )

# convert to class ir
ir_sample_data <-
  ir::ir_new_ir(spectra = d_mir$spectra,
                metadata = metadata) %>%
  dplyr::relocate(.data$spectra, .after = dplyr::everything(.))

# export
usethis::use_data(ir_sample_data, overwrite = TRUE)
