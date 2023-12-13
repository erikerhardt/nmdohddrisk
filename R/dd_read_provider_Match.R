#' Read Provider Matching ID files
#'
#' @param fn_list           list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_provider_Match
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
#' dd_read_provider_Match(
#'     fn_list           = c(
#'                           "Provider Omnicad View_20230918.xlsx"
#'                         )
#'   , path_data         = "../Data_in/Provider_Matching"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_provider_Match <-
  function(
    fn_list           = c(
                          "Provider Omnicad View_20230918.xlsx"
                        )
  , path_data         = "../Data_in/Provider_Matching"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_provider_Match"

  dat_sheet <-
    readxl::read_xlsx(
      file.path(path_data, fn_list)
    , guess_max = 1e5
    #, na        = "N/A"
    ) |>
    janitor::clean_names(
      case = "none"
    )

  #str(dat_sheet)

  dat_provider_Match <-
    dat_sheet |>
    dplyr::rename(
      Prov_ProvidersBase_TblID         = ProvidersBase_TblID
    , Prov_ImportedDate                = ImportedDate
    , Prov_ImportedFileName            = ImportedFileName
    , Prov_ModifiedDate                = ModifiedDate
    , Prov_ModifiedBy                  = ModifiedBy
    , Prov_ProviderLocationFullAddress = ProviderLocationFullAddress
    , Prov_ProviderCity                = ProviderCity
    , Prov_ProviderState               = ProviderState
    , Prov_ProviderZip                 = ProviderZip
    , Prov_ProviderCounty              = ProviderCounty
    , Prov_ProviderRegion              = ProviderRegion
    ) |>
    dplyr::mutate(
    #  Prov_ProvidersBase_TblID    = Prov_ProvidersBase_TblID         |>
    #, Prov_ID                     = Prov_ID                     |>
      Prov_Location_Cd            = Prov_Location_Cd            |> factor()
    , Prov_Type_Cd                = Prov_Type_Cd                |> factor()
    #, Prov_Fed_Tax_Id             = Prov_Fed_Tax_Id             |>
    #, Prov_SSN                    = Prov_SSN                    |>
    #, Prov_DBA_Name               = Prov_DBA_Name               |>
    #, Prov_Name                   = Prov_Name                   |>
    #, Prov_Sort_Name              = Prov_Sort_Name              |>
    , Prov_DOB                    = Prov_DOB                    |> as.numeric() |> e_convert_ExcelDate_to_Date()
    , Prov_Name_Org_Ind           = Prov_Name_Org_Ind           |> factor()
    , Prov_DBA_Org_Ind            = Prov_DBA_Org_Ind            |> factor()
    , Prov_MCare_Ind              = Prov_MCare_Ind              |> factor()
    , Prov_Profit_Ind             = Prov_Profit_Ind             |> factor()
    , Prov_IHS_Ind                = Prov_IHS_Ind                |> factor()
    #, Prov_Svc_Cnty_Cd            = Prov_Svc_Cnty_Cd            |>
    , Prov_Current_Enroll_Status  = Prov_Current_Enroll_Status  |> factor()
    , Prov_Reverify_Date          = Prov_Reverify_Date          |> lubridate::as_date()
    , Prov_Added_Date             = Prov_Added_Date             |> lubridate::as_date()
    , Prov_ImportedDate                = Prov_ImportedDate                |> lubridate::as_date()
    #, Prov_ImportedFileName            = Prov_ImportedFileName            |>
    , Prov_ModifiedDate                = Prov_ModifiedDate                |> lubridate::as_date()
    #, Prov_ModifiedBy                  = Prov_ModifiedBy                  |>
    #, Prov_ProviderLocationFullAddress = Prov_ProviderLocationFullAddress |>
    #, Prov_ProviderCity                = Prov_ProviderCity                |>
    #, Prov_ProviderState               = Prov_ProviderState               |>
    #, Prov_ProviderZip                 = Prov_ProviderZip                 |>
    #, Prov_ProviderCounty              = Prov_ProviderCounty              |>
    , Prov_ProviderRegion              = Prov_ProviderRegion              |>
                                factor(levels = c("Metro", "NE", "NW", "SE", "SW", "NULL"))
    , dat_provider_Match = TRUE
    )

  #dat_provider_Match |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_provider_Match
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_provider_Match)

} # dd_read_provider_Match
