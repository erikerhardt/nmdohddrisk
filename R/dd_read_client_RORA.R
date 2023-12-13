#' Read Client RORA
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_RORA
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
#' dd_read_client_RORA(
#'     fn_list           = c(
#'                           "FY23 RORA Monitoring Data_20230914.xlsx"
#'                         )
#'   , path_data         = "../Data_in/Client_RORA"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_RORA <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_RORA"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_RORA"

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
      , SystemConcernID      = SystemConcernID      |> as.character()
      )
  }

  dat_client_RORA <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "RoraID"
        , "SystemConcernID"
        , "RoraType"
        , "Waiver"
        , "Office"
        , "ConcerningProviderID"
        #, "ConcerningProvider"
        #, "ConcerningProivderMedicaid"
        , "RequestDate"
        #, "Submitter"
        #, "Title"
        , "PrimaryProviderID"
        #, "PrimaryProvider"
        #, "PrimaryProviderMedicaid"
        , "ClosedDate"
        #, "FirstName"
        #, "LastName"
        #, "MiddleInitial"
        , "SSN"
        , "Primary_Concern"
        , "Secondary_Concern"
        , "Detailed_Concern"
        #, "JacksonRelated"
        , "ProviderRole"
        , "Concerning_Healthcare_Provider_Role"
        , "Concerning_State_Agency_Role"
        , "Care_or_Compliance"
        , "Risk"
        , "Problem"
        )
      )
    ) |>
    dplyr::rename(
      C_RORA_RoraID                              = RoraID
    , C_RORA_SystemConcernID                     = SystemConcernID
    , C_RORA_RoraType                            = RoraType
    , C_RORA_Waiver                              = Waiver
    , C_RORA_Office                              = Office
    , C_RORA_ConcerningProviderID                = ConcerningProviderID
    , C_RORA_RequestDate                         = RequestDate
    , C_RORA_PrimaryProviderID                   = PrimaryProviderID
    , C_RORA_ClosedDate                          = ClosedDate
    , C_RORA_SSN                                 = SSN
    , C_RORA_Primary_Concern                     = Primary_Concern
    , C_RORA_Secondary_Concern                   = Secondary_Concern
    , C_RORA_Detailed_Concern                    = Detailed_Concern
    , C_RORA_ProviderRole                        = ProviderRole
    , C_RORA_Concerning_Healthcare_Provider_Role = Concerning_Healthcare_Provider_Role
    , C_RORA_Concerning_State_Agency_Role        = Concerning_State_Agency_Role
    , C_RORA_Care_or_Compliance                  = Care_or_Compliance
    , C_RORA_Risk                                = Risk
    , C_RORA_Problem                             = Problem
    ) |>
    dplyr::filter(
      C_RORA_RoraType ==  "Individual"
    ) |>
    dplyr::rename(
      C_RORA_Region = C_RORA_Office
    , Client_SSN  = C_RORA_SSN
    ) |>
    dplyr::mutate(
    #  C_RORA_RoraType          = C_RORA_RoraType |> factor()
      C_RORA_SystemConcernID    = ifelse(C_RORA_SystemConcernID == "N/A", NA, C_RORA_SystemConcernID)
    , C_RORA_Waiver             = C_RORA_Waiver |> factor()
    , C_RORA_Region             = C_RORA_Region |>
                                  stringr::str_replace(pattern = "RO$", replacement = "")      |>
                                  stringr::str_replace(pattern = "^M$", replacement = "Metro") |>
                                  factor(levels = c("Metro", "NE", "NW", "SE", "SW", "NA"))
    , C_RORA_RequestDate        = C_RORA_RequestDate |> lubridate::mdy()
    , C_RORA_ClosedDate         = C_RORA_ClosedDate  |> lubridate::mdy()
    , Client_SSN                = Client_SSN |>
                                  stringr::str_replace_all(pattern = "-", replacement = "") |>
                                  as.numeric()
    , C_RORA_Primary_Concern    = C_RORA_Primary_Concern   |> factor()
    , C_RORA_Secondary_Concern  = C_RORA_Secondary_Concern |> factor()
    , C_RORA_Detailed_Concern   = C_RORA_Detailed_Concern  |> factor()
    # XXX Collapse ProviderRole categories?
    #, C_RORA_ProviderRole      = C_RORA_ProviderRole
    #, C_RORA_HCProvider        = C_RORA_HCProvider |> factor()
    #, C_RORA_Agency            = C_RORA_Agency     |> factor()
    , C_RORA_Care_or_Compliance = C_RORA_Care_or_Compliance |> factor()
    , C_RORA_Risk               = C_RORA_Risk |>
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
    , Date                      = C_RORA_RequestDate
    , dat_client_RORA = TRUE
    ) |>
    dplyr::relocate(
      Client_SSN
    , Date
    )

  #dat_client_RORA |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_RORA
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_RORA)

} # dd_read_client_RORA
