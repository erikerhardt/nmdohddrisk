#' NM DOH DD Prediction Model, main predict and model function
#'
#' @param params              \code{params} object from qmd file, \code{NMDOH_DD_RiskPredictionModel_Data-Model-Summary_yyyymmdd.qmd}
#' @param path_list           list of paths to data and output from \code{dd_set_paths()}
#' @param sw_plot_missing     T/F Create plot of missing values (quick)
#' @param sw_codebook         T/F Create a codebook for each dataset (slow)
#'
#' @return NULL               invisibly
#' @import erikmisc
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' dd_read_ALL_features <-
#'   function(
#'     working_directory   = "D:/Dropbox/StatAcumen/consult/Industry/2023_NM_DOH_DDWaiver/data/Zdrive/Risk_Prediction_Model/Scripts"
#'   , params              = params
#'   , sw_plot_missing     = c(TRUE, FALSE)[1]
#'   , sw_codebook         = c(TRUE, FALSE)[2]
#'   )
#' }
dd_read_ALL_features <-
  function(
    params              = params
  , path_list           = path_list
  , sw_plot_missing     = c(TRUE, FALSE)[1]
  , sw_codebook         = c(TRUE, FALSE)[2]
  ) {

  ## Client data

  ## Client_Matching
  ## Matching demographics
  dat_client_Match <-
    dd_read_client_Match(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_Matching")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_Match <-
    dat_client_Match |>
    dd_features_client_Match()
  #"dat_client_Match" |> dd_save_to_RData()
  ## saved below with dat_client_WaiverType
  #save(dat_client_Match, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_Match", ".RData")))


  ## Client_WaiverType
  ## Waiver type, add to Matching
  dat_client_WaiverType <-
    dd_read_client_WaiverType(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_WaiverType")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_Match <-
    dat_client_WaiverType |>
    dd_features_client_WaiverType(dat_client_Match = dat_client_Match)
  #"dat_client_Match" |> dd_save_to_RData()
  save(dat_client_Match, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_Match", ".RData")))


  ## Client_BBS
  ## Bureau of Behavioral Supports (BBS)
  dat_client_BBS <-
    dd_read_client_BBS(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_BBS")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_BBS <-
    dat_client_BBS |>
    dd_features_client_BBS(dat_client_Match = dat_client_Match)
  #"dat_client_BBS" |> dd_save_to_RData()
  save(dat_client_BBS, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_BBS", ".RData")))


  ## Client_CaseNotes
  dat_client_CaseNotes <-
    dd_read_client_CaseNotes(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_CaseNotes")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_CaseNotes <-
    dat_client_CaseNotes |>
    dd_features_client_CaseNotes(dat_client_Match = dat_client_Match)
  #"dat_client_CaseNotes" |> dd_save_to_RData()
  save(dat_client_CaseNotes, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_CaseNotes", ".RData")))


  ## Client_Conduent_Omnicad
  dat_client_Conduent_Omnicad <-
    dd_read_client_Conduent_Omnicad(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_Conduent_Omnicad")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_Conduent_Omnicad <-
    dat_client_Conduent_Omnicad |>
    dd_features_client_Conduent_Omnicad(dat_client_Match = dat_client_Match)
  #"dat_client_Conduent_Omnicad" |> dd_save_to_RData()
  save(dat_client_Conduent_Omnicad, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_Conduent_Omnicad", ".RData")))

  # 12/17/2023 Replaced by Client_WaiverType
  #
  # # Update Match with Waiver derived from Conduent_Omnicad
  # dat_client_Match <-
  #   dat_client_Match |>
  #   dplyr::left_join(
  #     list_dat_client_Conduent_Omnicad[[ "dat_client_Conduent_Omnicad_Waiver" ]]
  #   , by = dplyr::join_by(Client_System_ID)
  #   )
  # "dat_client_Match" |> dd_save_to_RData()


  ## Client_eCHAT
  ## Acuity
  dat_client_eCHAT <-
    dd_read_client_eCHAT(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_eCHAT")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_eCHAT <-
    dat_client_eCHAT |>
    dd_features_client_eCHAT(dat_client_Match = dat_client_Match)
  #"dat_client_eCHAT" |> dd_save_to_RData()
  save(dat_client_eCHAT, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_eCHAT", ".RData")))


  ## Client_GER
  ## Therap (GER)
  dat_client_GER <-
    dd_read_client_GER(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_GER")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_GER <-
    dat_client_GER |>
    dd_features_client_GER(dat_client_Match = dat_client_Match)
  #"dat_client_GER" |> dd_save_to_RData()
  save(dat_client_GER, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_GER", ".RData")))


  ## Client_IMB_ANE
  ## ANE reports from IMB database
  dat_client_IMB_ANE <-
    dd_read_client_IMB_ANE(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_IMB_ANE")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_IMB_ANE <-
    dat_client_IMB_ANE |>
    dd_features_client_IMB_ANE(dat_client_Match = dat_client_Match)
  #"dat_client_IMB_ANE" |> dd_save_to_RData()
  save(dat_client_IMB_ANE, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_IMB_ANE", ".RData")))


  ## Client_MedFrag_LOC


  ## Client_RORA
  ## RORA
  dat_client_RORA <-
    dd_read_client_RORA(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_RORA")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_RORA <-
    dat_client_RORA |>
    dd_features_client_RORA(dat_client_Match = dat_client_Match)
  #"dat_client_RORA" |> dd_save_to_RData()
  save(dat_client_RORA, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_RORA", ".RData")))

  ## Client_Syncronys

  # 10/25/2023
  # Also read Admits-DDSD_REG_NM-Yearly_20231025.txt
  # 1 year of syncronys data for everyone that is currently on our roster.
  # https://mail.google.com/mail/u/0/#inbox/FMfcgzGwHLpZKlflmvdWdhJLdkBgBlrQ

  ## Syncronys
  dat_client_Syncronys <-
    dd_read_client_Syncronys(
      fn_list           = NULL
    , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Client_Syncronys")
    , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
    , sw_plot_missing   = sw_plot_missing
    , sw_codebook       = sw_codebook
    )
  dat_client_Syncronys <-
    dat_client_Syncronys |>
    dd_features_client_Syncronys(dat_client_Match = dat_client_Match)
  #"dat_client_Syncronys" |> dd_save_to_RData()
  save(dat_client_Syncronys, file = file.path(params$working_directory, path_list$path_results_dat, paste0("dat_client_Syncronys", ".RData")))



  ## Provider data


  # ## Provider_Matching
  # dat_provider_Match <-
  #   dd_read_provider_Match(
  #     fn_list           = NULL
  #   , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Provider_Matching")
  #   , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
  #   , sw_plot_missing   = sw_plot_missing
  #   , sw_codebook       = sw_codebook
  #   )
  # dat_provider_Match <-
  #   dat_provider_Match |>
  #   dd_features_provider_Match()
  # "dat_provider_Match" |> dd_save_to_RData()
  #
  # ## Provider_Conduent_Omnicad
  #
  # ## Provider_PDS_Scorecard
  # ## PDS Provider Data Summary (Scorecard)
  # # * Manually format header and footer to make it easy to read.
  # # Waiting for data with Provider IDs
  # dat_provider_PDS <-
  #   dd_read_provider_PDS(
  #     fn_list           = c(
  #                           "Scorecard Jan 23.xlsx"
  #                         , "Scorecard June 23.xlsx"
  #                         , "Scorecard March 23.xlsx"
  #                         , "Scorecard Oct 22.xlsx"
  #                         )
  #   , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Provider_PDS_Scorecard")
  #   , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
  #   , sw_plot_missing   = sw_plot_missing
  #   , sw_codebook       = sw_codebook
  #   )
  # dat_provider_PDS <-
  #   dat_provider_PDS |>
  #   dd_features_provider_PDS()
  # "dat_provider_PDS" |> dd_save_to_RData()


  ## Provider_QMB

  # ## Provider_RORA
  # ## RORA
  # # 9/8/23 Waiting for new data, requested 3 years.
  # dat_provider_RORA <-
  #   dd_read_provider_RORA(
  #     fn_list           = NULL
  #   , path_data         = file.path(params$working_directory, path_list$path_dat_in, "Provider_RORA")
  #   , path_results_dat  = file.path(params$working_directory, path_list$path_results_dat)
  #   , sw_plot_missing   = sw_plot_missing
  #   , sw_codebook       = sw_codebook
  #   )
  # dat_provider_RORA <-
  #   dat_provider_RORA |>
  #   dd_features_provider_RORA()
  # "dat_provider_RORA" |> dd_save_to_RData()


  invisible(NULL)

} # dd_read_ALL_features
