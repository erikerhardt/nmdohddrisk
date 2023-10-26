#' Read Provider RORA
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
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
    fn_list           = NULL
  , path_data         = "../Data_in/Provider_RORA"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_provider_RORA"

  dat_sheet <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "csv$"
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[2]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    , sw_delim                = c(FALSE, "|")[1]
    , sw_read_package_csv_txt = c("readr", "utils")[1]
    )

  # align column classes
  # which(!(unlist(lapply(dat_sheet[[1]], class)) == unlist(lapply(dat_sheet[[2]], class))))
  for (i_sheet in seq_len(length(dat_sheet))) {
    dat_sheet[[ i_sheet ]] <-
      dat_sheet[[ i_sheet ]] |>
      dplyr::mutate(
        PrimaryProviderID    = PrimaryProviderID    |> as.character()
      )
  }

  dat_provider_RORA <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::rename(
      P_RORA_RoraID                              = RoraID
    , P_RORA_SystemConcernID                     = SystemConcernID
    , P_RORA_RoraType                            = RoraType
    , P_RORA_Waiver                              = Waiver
    , P_RORA_Office                              = Office
    , P_RORA_ConcerningProviderID                = ConcerningProviderID
    , P_RORA_ConcerningProvider                  = ConcerningProvider
    , P_RORA_ConcerningProivderMedicaid          = ConcerningProivderMedicaid
    , P_RORA_RequestDate                         = RequestDate
    , P_RORA_Submitter                           = Submitter
    , P_RORA_Title                               = Title
    , P_RORA_PrimaryProviderID                   = PrimaryProviderID
    , P_RORA_PrimaryProvider                     = PrimaryProvider
    , P_RORA_PrimaryProviderMedicaid             = PrimaryProviderMedicaid
    , P_RORA_ClosedDate                          = ClosedDate
    , P_RORA_FirstName                           = FirstName
    , P_RORA_LastName                            = LastName
    , P_RORA_MiddleInitial                       = MiddleInitial
    , P_RORA_SSN                                 = SSN
    , P_RORA_Primary_Concern                     = Primary_Concern
    , P_RORA_Secondary_Concern                   = Secondary_Concern
    , P_RORA_Detailed_Concern                    = Detailed_Concern
    , P_RORA_JacksonRelated                      = JacksonRelated
    , P_RORA_ProviderRole                        = ProviderRole
    , P_RORA_Concerning_Healthcare_Provider_Role = Concerning_Healthcare_Provider_Role
    , P_RORA_Concerning_State_Agency_Role        = Concerning_State_Agency_Role
    , P_RORA_Care_or_Compliance                  = Care_or_Compliance
    , P_RORA_Risk                                = Risk
    , P_RORA_Problem                             = Problem
    ) |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "P_RORA_RoraID"
        , "P_RORA_SystemConcernID"
        , "P_RORA_RoraType"
        , "P_RORA_Waiver"
        , "P_RORA_Office"
        , "P_RORA_ConcerningProviderID"
        #, "P_RORA_ConcerningProvider"
        #, "P_RORA_ConcerningProivderMedicaid"
        , "P_RORA_RequestDate"
        #, "P_RORA_Submitter"
        #, "P_RORA_Title"
        , "P_RORA_PrimaryProviderID"
        #, "P_RORA_PrimaryProvider"
        #, "P_RORA_PrimaryProviderMedicaid"
        , "P_RORA_ClosedDate"
        #, "P_RORA_FirstName"
        #, "P_RORA_LastName"
        #, "P_RORA_MiddleInitial"
        , "P_RORA_SSN"
        , "P_RORA_Primary_Concern"
        , "P_RORA_Secondary_Concern"
        , "P_RORA_Detailed_Concern"
        #, "P_RORA_JacksonRelated"
        , "P_RORA_ProviderRole"
        , "P_RORA_Concerning_Healthcare_Provider_Role"
        , "P_RORA_Concerning_State_Agency_Role"
        , "P_RORA_Care_or_Compliance"
        , "P_RORA_Risk"
        , "P_RORA_Problem"
        )
      )
    ) |>
    dplyr::filter(
      P_RORA_RoraType ==  "Provider"
    ) |>
    dplyr::rename(
      P_RORA_Region = P_RORA_Office
    ) |>
    dplyr::mutate(
    #  P_RORA_RoraType                = P_RORA_RoraType |> factor()
      P_RORA_SystemConcernID         = ifelse(P_RORA_SystemConcernID == "N/A", NA, P_RORA_SystemConcernID)
    , P_RORA_Waiver                  = P_RORA_Waiver |> factor()
    , P_RORA_Region                  = P_RORA_Region |>
                                       stringr::str_replace(pattern = "RO$", replacement = "")      |>
                                       stringr::str_replace(pattern = "^M$", replacement = "Metro") |>
                                       factor(levels = c("Metro", "NE", "NW", "SE", "SW", "NA"))
    , P_RORA_RequestDate             = P_RORA_RequestDate |> lubridate::mdy()
    , P_RORA_SSN                     = P_RORA_SSN |>
                                       stringr::str_replace_all(pattern = "-", replacement = "") |>
                                       as.numeric()
    , P_RORA_Primary_Concern         = P_RORA_Primary_Concern   |> factor()
    , P_RORA_Secondary_Concern       = P_RORA_Secondary_Concern |> factor()
    , P_RORA_Detailed_Concern        = P_RORA_Detailed_Concern  |> factor()
    # XXX Collapse ProviderRole categories?
    #, P_RORA_ProviderRole            = P_RORA_ProviderRole
    #, P_RORA_HCProvider              = P_RORA_HCProvider |> factor()
    #, P_RORA_Agency                  = P_RORA_Agency     |> factor()
    , P_RORA_Care_or_Compliance      = P_RORA_Care_or_Compliance |> factor()
    , P_RORA_Risk                    = P_RORA_Risk |>
                                       forcats::fct_collapse(
                                         "Priority 1 (high)" =
                                           c(
                                             "Emergency"
                                           , "Priority 1"
                                           )
                                         , "Priority 2 (low)" =
                                           c(
                                             "Priority 2"
                                           )
                                       ) |>
                                       factor()
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
    nmdohddrisk::dd_plot_missing_codebook(
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
##
##
##   # e_text_unicode_to_ascii(
##   #   fn_in   = file.path(path_data, "RORA_Data_Export_20230930.csv")
##   # , fn_out  = file.path(path_data, "RORA_Data_Export_20230930_nounicode.csv")
##   # , unicode_generic_replacement = ""
##   # , sw_print_line_text = TRUE
##   # )
##   #
##   # dat_sheet <-
##   #   erikmisc::e_read_data_subdir_into_lists(
##   #     fn_path                 = path_data
##   #   , fn_detect               = "csv$"
##   #   , sw_fn_or_dat            = c("fn", "dat")[2]
##   #   , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
##   #   , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
##   #   , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
##   #   , sw_clean_names          = c(TRUE, FALSE)[2]
##   #   , sw_list_or_flat         = c("list", "flat")[2]
##   #   , sw_delim                = c(FALSE, "|")[2]
##   #   , sw_read_package_csv_txt = c("readr", "utils")[1]
##   #   )
##
##   # Might want to read multiple sheets
##
##   dat_sheet <- list()
##
##   for (n_fn in fn_list) {
##     print(n_fn)
##
##     dat_sheet[[ n_fn ]] <-
##       readxl::read_xlsx(
##         file.path(path_data, n_fn)
##       , sheet     = 2
##       , guess_max = 1e5
##       )
##   }
##
##   dat_provider_RORA <-
##     dat_sheet |>
##     dplyr::bind_rows() |>
##     dplyr::distinct() |>
##     dplyr::select(
##       tidyselect::all_of(
##         c(
##           "ID"
##         , "ProviderName"
##         , "MedicaidID"
##         #, "PhysicalAddress"
##         , "PhysicalCity"
##         , "PhysicalCityImport"
##         , "PhysicalState"
##         , "PhysicalZip"
##         #, "MailingAddress"
##         #, "MailingCity"
##         #, "Email"
##         #, "MailingZip"
##         #, "MailingState"
##         #, "MailingCityImport"
##         , "BusAreaCode"
##         #, "BusPhone"
##         #, "FaxAreaCode"
##         #, "Fax"
##         , "CellAreaCode"
##         #, "Cell"
##         #, "Note"
##         #, "CorporateAddress"
##         #, "CorporateCity"
##         #, "CorporateCityImport"
##         #, "CorporateState"
##         #, "CorporateZip"
##         #, "CorpAreaCode"
##         #, "CorpPhone"
##         , "Deleted"
##         #, "UserName"
##         #, "NMCRS"
##         , "LiabilityRider"
##         , "BondEndDate"
##         , "BondStartDate"
##         , "LiabilityEndDate"
##         , "LiabilityStartDate"
##         , "BondRider"
##         )
##       )
##     ) |>
##     dplyr::mutate(
##       dplyr::across(
##         .cols = tidyselect::ends_with("Date")
##       , .fns  = lubridate::as_date
##       )
##     , PhysicalZip   = str_split(PhysicalZip , pattern = "-", simplify = TRUE)[, 1]
##     #, MailingZip    = str_split(MailingZip  , pattern = "-", simplify = TRUE)[, 1]
##     #, CorporateZip  = str_split(CorporateZip, pattern = "-", simplify = TRUE)[, 1]
##     , ProviderName  = ProviderName #|> stringr::str_replace_all(pattern = "[[:punct:]]", "") |> toupper() # remove punctuation
##     , dat_provider_RORA = TRUE
##     )
##
##
##   #dat_provider_RORA |> str()
##
##
##   if (!is.null(path_results_dat)) {
##     save(
##         dat_provider_RORA
##       , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
##       )
##   }
##
##   if (sw_plot_missing) {
##     nmdohddrisk::dd_plot_missing_codebook(
##         dat_this         = dat_provider_RORA
##       , name_dat         = name_dat
##       , path_results_dat = path_results_dat
##       #, sw_width         = 10
##       #, sw_height        = 10
##       , sw_codebook      = sw_codebook
##       )
##   }
##
##   return(dat_provider_RORA)
##
## } # dd_read_provider_RORA
