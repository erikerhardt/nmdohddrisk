#' Read Provider RORA
#'
#' @param fn_list           list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_provider_RORA
#' @importFrom readxl read_xlsx
#' @import dplyr
#' @import forcats
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import tidyselect
#' @export
#'
#' @examples
#' \dontrun{
#' dd_read_provider_RORA(
#'     fn_list           = c(
#'                           "FY23 RORA Monitoring Data_20230914.xlsx"
#'                         )
#'   , path_data         = "../Data_in/Provider_RORA"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_provider_RORA <-
  function(
    fn_list           = c(
                          "Copy of RORA 09.11.2023 (002).xlsx"
                        )
  , path_data         = "../Data_in/Provider_RORA"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_provider_RORA"

  # Might want to read multiple sheets

  dat_sheet <- list()

  for (n_fn in fn_list) {
    print(n_fn)

    dat_sheet[[ n_fn ]] <-
      readxl::read_xlsx(
        file.path(path_data, n_fn)
      , sheet     = 2
      , guess_max = 1e5
      )
  }

  dat_provider_RORA <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "ID"
        , "ProviderName"
        , "MedicaidID"
        #, "PhysicalAddress"
        , "PhysicalCity"
        , "PhysicalCityImport"
        , "PhysicalState"
        , "PhysicalZip"
        #, "MailingAddress"
        #, "MailingCity"
        #, "Email"
        #, "MailingZip"
        #, "MailingState"
        #, "MailingCityImport"
        , "BusAreaCode"
        #, "BusPhone"
        #, "FaxAreaCode"
        #, "Fax"
        , "CellAreaCode"
        #, "Cell"
        #, "Note"
        #, "CorporateAddress"
        #, "CorporateCity"
        #, "CorporateCityImport"
        #, "CorporateState"
        #, "CorporateZip"
        #, "CorpAreaCode"
        #, "CorpPhone"
        , "Deleted"
        #, "UserName"
        #, "NMCRS"
        , "LiabilityRider"
        , "BondEndDate"
        , "BondStartDate"
        , "LiabilityEndDate"
        , "LiabilityStartDate"
        , "BondRider"
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::ends_with("Date")
      , .fns  = lubridate::as_date
      )
    , PhysicalZip   = str_split(PhysicalZip , pattern = "-", simplify = TRUE)[, 1]
    #, MailingZip    = str_split(MailingZip  , pattern = "-", simplify = TRUE)[, 1]
    #, CorporateZip  = str_split(CorporateZip, pattern = "-", simplify = TRUE)[, 1]
    , ProviderName  = ProviderName #|> stringr::str_replace_all(pattern = "[[:punct:]]", "") |> toupper() # remove punctuation
    , dat_provider_RORA = TRUE
    )


  #dat_provider_RORA |> str()


  if (!is.null(path_results_dat)) {
    save(
        dat_provider_RORA
      , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
      )
  }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_provider_RORA
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_provider_RORA)

} # dd_read_provider_RORA
