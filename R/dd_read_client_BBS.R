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
#'     fn_list           = c(
#'                           "FY23 BBS Monitoring Data_20230914.xlsx"
#'                         )
#'   , path_data         = "../Data_in/Client_BBS"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_BBS <-
  function(
    fn_list           = c(
                          "FY23 BBS Monitoring Data_20230914.xlsx"
                        )
  , path_data         = "../Data_in/Client_BBS"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_BBS"

  dat_sheet <- list()

  for (n_fn in fn_list) {
    ## n_fn = fn_list[1]
    print(n_fn)
    dat_sheet[[ n_fn ]] <-
      readxl::read_xlsx(
        file.path(path_data, n_fn)
      , sheet     = 1
      , guess_max = 1e5
      , na        = c("N/A", "n/a")
      ) |>
      janitor::clean_names(
        case = "none"
      ) |>
      dplyr::select(
        Date
      , Name
      , Category
      , Activity
      , Topic
      , Region
      , Waiver
      , At_Risk
      ) |>
      dplyr::mutate(
        Date      = Date      |> lubridate::as_date()
      )
  }


  dat_client_BBS <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::mutate(
    #  Self_guardian = Self_guardian |> factor()
      Region        = Region        |> factor(levels = c("Metro", "NE", "NW", "SE", "SW", "test"))
    , At_Risk       = At_Risk       |> factor()
    , Category      = Category      |> factor()
    , dat_client_BBS = TRUE
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
