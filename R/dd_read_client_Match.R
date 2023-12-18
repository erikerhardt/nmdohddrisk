#' Read Client Matching ID files
#'
#' @param fn_list           file to read or \code{NULL} for most recent by filename
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_Match
#' @importFrom readxl read_xlsx
#' @importFrom janitor clean_names
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
#' dd_read_client_Match(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_Matching"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_Match <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_Matching"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_Match"

  if ( is.null(fn_list) ) {
    fn_list <-
      e_file_first_last(name = NULL, path = path_data)
  } else {
    fn_list <-
      file.path(path_data, fn_list)
  }

  dat_sheet <-
    readxl::read_xlsx(
      path      = fn_list
    , guess_max = 1e5
    , na        = "N/A"
    ) |>
    janitor::clean_names(
      case = "none"
    )


  #str(dat_sheet)

  dat_client_Match <-
    dat_sheet |>
    dplyr::rename(
      Client_TherapID     = Therap_ID
    , Client_Name         = ClientName
    , Client_NameWithID   = ClientNameWithID
    , Client_NameWithSSN  = ClientNameWithSSN
    , Client_Base_TblID   = ClientBase_TblID
    ) |>
    dplyr::mutate(
      Client_TherapID   = Client_TherapID |> toupper()
    , Client_DOB        = Client_DOB |> lubridate::as_date()
    , Client_DOD        = ifelse(Client_DOD == "9999-12-31", NA, Client_DOD) |> lubridate::as_date()
    , Client_Gender     = Client_Gender |> factor(levels = c("M", "F"))
    , Client_Ethnicity  = Client_Ethnicity |> factor()
      # https://www.census.gov/quickfacts/fact/note/US/RHI625222
    , Client_Race       = Client_Race      |>
                          forcats::fct_collapse(
                            "White" =
                              c(
                                "Caucasian"
                              )
                          , "Black or African American" =
                              c(
                                "Black"
                              )
                          , "American Indian and Alaska Native" =
                              c(
                                "American Indian"
                              )
                          , "Asian" =
                              c(
                                "Asian/Pacific Islander"
                              )
                          , "Hispanic" =
                              c(
                                "Hispanic"
                              )
                          , "Other" =
                              c(
                                "Other"
                              , "Unknown"
                              )
                            #"Native Hawaiian and Other Pacific Islander"
                          , "Two or more races" =
                              c(
                                "African American and White"
                              , "Asian and White"
                              , "Native American and African Am"
                              , "Native American and White"
                              , "Native Hawaiian or Other Pacif"
                              )
                          ) |>
                          factor(levels =
                            c(
                              "White"
                            , "Black or African American"
                            , "American Indian and Alaska Native"
                            , "Asian"
                            , "Hispanic"
                            , "Other"
                            , "Two or more races"
                            )
                          )
    , Client_County     = Client_County    |> factor()
    , Client_Region     = Client_Region    |> factor()
    , dat_client_Match = TRUE
    )

  #dat_client_Match |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_Match
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_Match)

} # dd_read_client_Match
