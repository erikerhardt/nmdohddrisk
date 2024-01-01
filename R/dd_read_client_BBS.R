#' Read Client BBS
#'
#' @param fn_list           list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_BBS
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
#' dd_read_client_BBS(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_BBS"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_BBS <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_BBS"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_BBS"


  dat_sheet <-
    e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = c("xlsx$")
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[2]
    , excel_sheets            = "all"
    , sw_clean_names          = c(TRUE, FALSE)[2]
    , sw_list_or_flat         = c("list", "flat")[1]
    , excel_range             = NULL
    , excel_col_names         = TRUE
    , sw_delim                = c(FALSE, "|")[1]
    , sw_read_package_csv_txt = c("readr", "utils")[1]
    )

  dat_client_BBS <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::select(
      SSN
    , Date
    ) |>
    dplyr::rename(
      Client_SSN = SSN
    , BBS_Date   = Date
    ) |>
    dplyr::mutate(
      BBS_Date = BBS_Date |> lubridate::as_date()
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      BBS_AtRisk      = 1
    , dat_client_BBS  = TRUE
    )

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_BBS
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_BBS)

} # dd_read_client_BBS
