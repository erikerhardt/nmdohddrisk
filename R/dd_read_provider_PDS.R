#' Read Provider PDS
#'
#' @param fn_list           list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_provider_PDS
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
#' dd_read_provider_PDS(
#'     fn_list           = c(
#'                           "Scorecard Jan 23.xlsx"
#'                         , "Scorecard June 23.xlsx"
#'                         , "Scorecard March 23.xlsx"
#'                         , "Scorecard Oct 22.xlsx"
#'                         )
#'   , path_data         = "../Data_in/Provider_PDS_Scorecard"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_provider_PDS <-
  function(
    fn_list           = c(
                          "Scorecard Jan 23.xlsx"
                        , "Scorecard June 23.xlsx"
                        , "Scorecard March 23.xlsx"
                        , "Scorecard Oct 22.xlsx"
                        )
  , path_data         = "../Data_in/Provider_PDS_Scorecard"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_provider_PDS"

  print("Waiting for PDS Scorecard data with Provider IDs")
  dat_provider_PDS = tibble::tibble(X = NA)
  return(dat_provider_PDS)

  # dat_sheet <- list()
  #
  # for (n_fn in fn_list) {
  #   print(n_fn)
  #   dat_sheet[[ n_fn ]] <-
  #     readxl::read_xlsx(
  #       file.path(path_data, n_fn)
  #     , sheet     = 1
  #     , guess_max = 1e5
  #     , na        = "N/A"
  #     )
  # }
  #
  # dat_PDS <-
  #   dat_sheet |>
  #   dplyr::bind_rows() |>
  #   dplyr::rename(
  #     Support_Type_Residential = Residential
  #   , Support_Type_Community   = Community
  #   , Support_Type_Employment  = Employment
  #   ) |>
  #   # determine date ranges by provider for Compliance
  #   dplyr::arrange(
  #     `Provider Name`
  #   , `Most Recent Survey_Date`
  #   ) |>
  #   dplyr::mutate(
  #     `Provider Name`  = `Provider Name` #|> stringr::str_replace_all(pattern = "[[:punct:]]", "") |> toupper() # remove punctuation
  #   , Region_Metro = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "Metro"), "Metro", NA)
  #   , Region_NE    = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "NE"   ), "NE"   , NA)
  #   , Region_NW    = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "NW"   ), "NW"   , NA)
  #   , Region_SE    = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "SE"   ), "SE"   , NA)
  #   , Region_SW    = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "SW"   ), "SW"   , NA)
  #   , Region_DOH   = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "DOH"  ), "DOH"  , NA)
  #   , Region_USA   = ifelse(`Current  Region` == "ALL REGIONS" | stringr::str_detect(`Current  Region`, pattern = "USA"  ), "USA"  , NA)
  #   , Support_Type_Residential = ifelse(Support_Type_Residential == "X", "R", NA)
  #   , Support_Type_Community   = ifelse(Support_Type_Community   == "X", "C", NA)
  #   , Support_Type_Employment  = ifelse(Support_Type_Employment  == "X", "E", NA)
  #   , Support_Type_Residential = ifelse(!is.na(`Support Types`), ifelse(stringr::str_detect(`Support Types`, pattern = "R"), "R", NA), Support_Type_Residential)
  #   , Support_Type_Community   = ifelse(!is.na(`Support Types`), ifelse(stringr::str_detect(`Support Types`, pattern = "C"), "C", NA), Support_Type_Community  )
  #   , Support_Type_Employment  = ifelse(!is.na(`Support Types`), ifelse(stringr::str_detect(`Support Types`, pattern = "E"), "E", NA), Support_Type_Employment )
  #   #, Compliance_Previous      = stringr::str_sub(`Previous Survey_Determination of Compliance`, start = 1, end = 3)
  #   , Compliance_Recent        = stringr::str_sub(`Most Recent Survey_Determination of Compliance`, start = 1, end = 3)
  #   , Compliance_Recent        = Compliance_Recent |>
  #                                 forcats::fct_collapse(Par = c("Par", "Pat")) |>
  #                                 factor(levels = c("Non", "Par", "Com"))
  #   , `Most Recent Survey_Date`= `Most Recent Survey_Date` |> lubridate::as_date()
  #   , `Previous Survey_Date`   = `Previous Survey_Date`    |> lubridate::as_date()
  #   , Survey_Date_Middle       = ifelse(is.na(`Previous Survey_Date`)
  #                                , `Previous Survey_Date`
  #                                , `Most Recent Survey_Date` - (`Most Recent Survey_Date` - `Previous Survey_Date`) / 2
  #                                ) |>
  #                                lubridate::as_date()
  #   , Compliance_Period        = Compliance_Recent
  #   ) |>
  #   dplyr::group_by(
  #     `Provider Name`
  #   ) |>
  #   dplyr::mutate(
  #     Survey_Date_Start = Survey_Date_Middle
  #   , Survey_Date_End   = Survey_Date_Middle |> dplyr::lead()
  #   ) |>
  #   dplyr::ungroup() |>
  #   dplyr::mutate(
  #     Survey_Date_End = ifelse(is.na(Survey_Date_End), lubridate::today(), Survey_Date_End) |> lubridate::as_date()
  #   , dat_PDS = TRUE
  #   ) |>
  #   dplyr::select(
  #     -`Current  Region`
  #   , -`Support Types`
  #   , -`Previous Survey_Determination of Compliance`
  #   , -`Most Recent Survey_Determination of Compliance`
  #   #, -Compliance_Previous
  #   , -Compliance_Recent
  #   , -`Most Recent Survey_Date`
  #   , -`Previous Survey_Date`
  #   , -Survey_Date_Middle
  #   , -`Enforcement_Open Case Internal Review Committee 8/01/2018 and 8/01/2019`
  #   , -`Enforcement_Open Case Internal Review Committee 03/16/2020 and 03/16/2021`
  #   , -`Enforcement_Open Case Internal Review Committee 01/01/2019 and 01/01/2020`
  #   , -`Enforcement_Open Case Internal Review Committee 4/1/22 and 7/1/23`
  #   )
  #
  # #dat_PDS |> str()

  #dat_provider_PDS |> str()


  if (!is.null(path_results_dat)) {
    save(
        dat_provider_PDS
      , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
      )
  }

  if (sw_plot_missing) {
    nmdohddrisk::dd_plot_missing_codebook(
        dat_this         = dat_provider_PDS
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_provider_PDS)

} # dd_read_provider_PDS
