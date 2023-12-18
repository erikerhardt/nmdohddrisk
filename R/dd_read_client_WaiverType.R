#' Read Client Waiver Type file
#'
#' @param fn_list           file to read or \code{NULL} for most recent by filename
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_WaiverType
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
#' dd_read_client_WaiverType(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_WaiverType"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_WaiverType <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_WaiverType"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_WaiverType"

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

  dat_client_WaiverType <-
    dat_sheet |>
    dplyr::rename(
      Client_System_ID  = Row_Labels
    , MF                = Med_Frag_Only
    , MV                = Mi_Via
    , SW                = Supports_Waiver
    , DD                = Traditional
    ) |>
    tidyr::replace_na(
      list(
        MF = 0
      , MV = 0
      , SW = 0
      , DD = 0
      )
    ) |>
    dplyr::mutate(
      Client_System_ID = Client_System_ID |> as.numeric()
    , Client_Waiver =
        dplyr::case_when(
          (MF + MV + SW + DD) > 1 ~ "Transition"
        , MF == 1 ~ "MF"
        , MV == 1 ~ "MV"
        , SW == 1 ~ "DD"
        , DD == 1 ~ "DD"
        , TRUE    ~ "None"
        ) |>
        factor(levels = c("DD", "MV", "MF", "Transition", "None"))  # , "SW"
        #|>
        #forcats::fct_drop()
    , dat_client_WaiverType = TRUE
    ) |>
    dplyr::select(
      Client_System_ID
    , Client_Waiver
    )

  #dat_client_WaiverType |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_WaiverType
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_WaiverType)

} # dd_read_client_WaiverType
