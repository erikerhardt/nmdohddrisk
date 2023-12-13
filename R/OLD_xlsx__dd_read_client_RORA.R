#' Read Client RORA
#'
#' @param fn_list           list of files to read
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
    fn_list           = c(
                          "Copy of RORA 09.11.2023 (002).xlsx"
                        )
  , path_data         = "../Data_in/Client_RORA"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_RORA"

  # Might want to read multiple sheets

  dat_sheet <- list()

  for (n_fn in fn_list) {
    print(n_fn)
    dat_sheet[[ n_fn ]] <-
      readxl::read_xlsx(
        file.path(path_data, n_fn)
      , sheet     = 1
      , guess_max = 1e5
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
        , "Office"                   # Related to Region, remove trailing "RO"
        , "ConcerningProviderID"
        , "RequestDate"
        #, "SubmittedBy"
        #, "Title"
        #, "Problem"
        #, "DateClosedByRO"
        , "FirstName"
        , "LastName"
        , "MiddleInitial"
        , "SSN"
        , "Primary"
        , "Secondary"
        , "Detail"
        #, "JacksonRelated"
        , "ProviderRole"
        , "HCProvider"
        , "Agency"
        , "PrimaryProviderID"
        , "ComplianceIssue"
        , "Risk"
        )
      )
    ) |>
    dplyr::rename(
      Region = Office
    ) |>
    dplyr::mutate(
      RoraType                = RoraType |> factor()
    , Waiver                  = Waiver |> factor()
    , Region                  = Region |>
                                stringr::str_replace(pattern = "RO$", replacement = "")      |>
                                stringr::str_replace(pattern = "^M$", replacement = "Metro") |>
                                factor(levels = c("Metro", "NE", "NW", "SE", "SW", "NA"))
    , RequestDate             = RequestDate |> lubridate::as_date()
    , SSN                     = SSN |>
                                stringr::str_replace(pattern = "-", replacement = "") |>
                                as.numeric()
    , Primary                 = Primary   |> factor()
    , Secondary               = Secondary |> factor()
    # XXX Collapse ProviderRole categories?
    #, ProviderRole            = ProviderRole
    , HCProvider              = HCProvider |> factor()
    , Agency                  = Agency     |> factor()
    , ComplianceIssue         = ComplianceIssue |> factor()
    , Risk                    = Risk |>
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
    , dat_client_RORA = TRUE
    )

  #dat_client_RORA |> str()


  if (!is.null(path_results_dat)) {
    save(
        dat_client_RORA
      , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
      )
  }

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
