#' Read Client Conduent Omnicad
#'
#' @param fn_list           NULL, no list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_Conduent_Omnicad
#' @importFrom erikmisc e_read_data_subdir_into_lists
#' @importFrom readxl read_xlsx
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import tictoc
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
#' dd_read_client_Conduent_Omnicad(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_Conduent_Omnicad"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_Conduent_Omnicad <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_Conduent_Omnicad"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  dat_names <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "xlsx$"
    , sw_fn_or_dat            = c("fn", "dat")[1]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    )

  length(dat_names)


  # 12/17/2023 Replaced by Client_WaiverType
  #
  # # Waiver is determined, in addition to getting billing information
  # # Client_COE_Cd - Waiver 095 MedFrag, 096 DD Waiver and Mi Via Waiver
  # # Prior Auth indicates DD Waiver (so those without are Mi Via Waiver)
  # # cost center 86651
  #
  # # --------------------------------------------------------------------------
  # # Prior Auth indicates DD Waiver (so those without are Mi Via Waiver)
  #
  # name_dat_waiver <- "dat_client_Conduent_Omnicad_Waiver"
  #
  # dat_names_prior_auth <-
  #   dat_names |>
  #   stringr::str_subset(
  #     pattern = stringr::fixed("DOH Weekly Prior Auth Data Dump")
  #   )
  #
  #
  # #dat_sheet_prior_auth <- list()
  #
  # #library(doParallel)  # for %dopar% operator
  # num_cores <- parallel::detectCores() - 2
  # doParallel::registerDoParallel(num_cores)
  #
  # tictoc::tic(msg = "Timer") # start timer
  #
  #
  # # don't prespecify the "results" list
  # results <-
  #   foreach::foreach(
  #     i_sheet = seq_along(dat_names_prior_auth)
  #   #, .combine =                             # default is a list
  #   , .inorder = FALSE                        # FALSE is faster
  #   , .packages = c("readxl", "dplyr", "stringr", "lubridate")  # character vector of packages that the tasks depend on
  #   , .export = NULL                          # character vector of variables to export
  #   #) %do% # sequential
  #   ) %dopar%  # parallel
  #   {
  #
  #
  # #for (i_sheet in seq_along(dat_names_prior_auth)) {
  #   # i_sheet = 40
  #
  #   print(paste0("Reading Conduent Omnicad, DOH Weekly Prior Auth Data Dump, File ", i_sheet, " of ", length(dat_names_prior_auth)))
  #
  #   # some workbooks have multiple base and line sheets,
  #   #   so read and bind each separately, then join.
  #   this_sheet_names <-
  #     readxl::excel_sheets(
  #       path      = file.path(path_data, dat_names_prior_auth[i_sheet])
  #     )
  #
  #   this_sheet_names_base <-
  #     this_sheet_names |>
  #     stringr::str_subset(
  #       pattern = stringr::fixed("base")
  #     )
  #   #no-line-prior-auth# this_sheet_names_line <-
  #   #no-line-prior-auth#   this_sheet_names |>
  #   #no-line-prior-auth#   stringr::str_subset(
  #   #no-line-prior-auth#     pattern = stringr::fixed("line")
  #   #no-line-prior-auth#   )
  #
  #   dat_this_base <- list()
  #   for (i_sheet_base in seq_along(this_sheet_names_base)) {
  #     ## i_sheet_base = 1
  #     dat_this_base[[ i_sheet_base ]] <-
  #       readxl::read_xlsx(
  #         path      = file.path(path_data, dat_names_prior_auth[i_sheet])
  #       , sheet     = this_sheet_names_base[i_sheet_base]
  #       , guess_max = 1e5
  #       , progress  = FALSE
  #       ) |>
  #       dplyr::select(
  #         PA_Client_System_ID
  #       ) |>
  #       dplyr::rename(
  #         Client_System_ID = PA_Client_System_ID
  #       )
  #   }
  #   dat_this_base <-
  #     dat_this_base |>
  #     dplyr::bind_rows() |>
  #     dplyr::distinct()
  #
  #   #no-line-prior-auth# dat_this_line <- list()
  #   #no-line-prior-auth# for (i_sheet_line in seq_along(this_sheet_names_line)) {
  #   #no-line-prior-auth#   ## i_sheet_line = 1
  #   #no-line-prior-auth#   dat_this_line[[ i_sheet_line ]] <-
  #   #no-line-prior-auth#     readxl::read_xlsx(
  #   #no-line-prior-auth#       path      = file.path(path_data, dat_names_prior_auth[i_sheet])
  #   #no-line-prior-auth#     , sheet     = this_sheet_names_line[i_sheet_line]
  #   #no-line-prior-auth#     , guess_max = 1e5
  #   #no-line-prior-auth#     , progress  = FALSE
  #   #no-line-prior-auth#     ) |>
  #   #no-line-prior-auth#     dplyr::select(
  #   #no-line-prior-auth#       Line_Svc_Date_First
  #   #no-line-prior-auth#     , TCN
  #   #no-line-prior-auth#     , Proc_Cd
  #   #no-line-prior-auth#     , Line_Allowed_Units
  #   #no-line-prior-auth#     , Line_Pd_Units
  #   #no-line-prior-auth#     , Line_Proc_Code_Mod1
  #   #no-line-prior-auth#     , Line_Proc_Code_Mod2
  #   #no-line-prior-auth#     ) |>
  #   #no-line-prior-auth#     dplyr::mutate(
  #   #no-line-prior-auth#       Line_Svc_Date_First = Line_Svc_Date_First |> lubridate::as_date()
  #   #no-line-prior-auth#     )
  #   #no-line-prior-auth# }
  #   #no-line-prior-auth# dat_this_line <-
  #   #no-line-prior-auth#   dat_this_line |>
  #   #no-line-prior-auth#   dplyr::bind_rows()
  #
  #   # join base and line
  #   #dat_sheet_prior_auth[[ i_sheet ]] <-
  #   out  <-
  #     dat_this_base
  #     #no-line-prior-auth# dplyr::left_join(
  #     #no-line-prior-auth# , dat_this_line
  #     #no-line-prior-auth# , by = dplyr::join_by(TCN)
  #     #no-line-prior-auth# ) |>
  #     #no-line-prior-auth# # pre-calculate features (otherwise too huge when a single file)
  #     #no-line-prior-auth# dd_features_client_Conduent_Omnicad_read()
  #
  # #} # i_sheet
  #
  #   # use a return value
  #   return( out )
  #
  # } # foreach
  #
  # tictoc::toc() # end timer
  #
  # # explicitly close the implicitly created cluster
  # doParallel::stopImplicitCluster()
  #
  #
  # dat_client_Conduent_Omnicad_Waiver_DD <-
  #   #dat_sheet_prior_auth |>
  #   results |>
  #   dplyr::bind_rows() |>
  #   dplyr::distinct() |>
  #   dplyr::arrange(
  #     Client_System_ID
  #   ) |>
  #   dplyr::mutate(
  #     Client_Waiver = "DD"
  #   )
  #
  #
  # #dat_client_Conduent_Omnicad_Waiver_DD |> str()



  # --------------------------------------------------------------------------
  # Billing data

  name_dat <- "dat_client_Conduent_Omnicad"

  # Client Fields of interest
  # DOH Weekly Claims Data Dump2023-08-29-07-52-12-254.xlsx
  #   Tab: clms header_1
  #     TCN number
  #     Client_COE_Cd       - Waiver 095 MedFrag, 096 DD Waiver and Mi Via Waiver
  #     Client_Cnty_Geo     - County number (5 Regions, Metro = Albuquerque)
  #   Tab: clms line_2
  #     TCN number
  #     Proc_Cd
  #     Line_Allowed_Units
  #     Line_Pd_Units

  dat_names_weekly_claims <-
    dat_names |>
    stringr::str_subset(
      pattern = stringr::fixed("DOH Weekly Claims Data Dump")
    )


  #dat_sheet_weekly_claims <- list()

  #library(doParallel)  # for %dopar% operator
  num_cores <- parallel::detectCores() - 2
  doParallel::registerDoParallel(num_cores)

  tictoc::tic(msg = "Timer") # start timer


  # don't prespecify the "results" list
  results <-
    foreach::foreach(
      i_sheet = seq_along(dat_names_weekly_claims)
    #, .combine =                             # default is a list
    , .inorder = FALSE                        # FALSE is faster
    , .packages = c("readxl", "dplyr", "stringr", "lubridate")  # character vector of packages that the tasks depend on
    , .export = NULL                          # character vector of variables to export
    #) %do% # sequential
    ) %dopar%  # parallel
    {


  #for (i_sheet in seq_along(dat_names_weekly_claims)) {
    # i_sheet = 40

    print(paste0("Reading Conduent Omnicad, DOH Weekly Claims Data Dump, File ", i_sheet, " of ", length(dat_names_weekly_claims)))

    # some workbooks have multiple header and line sheets,
    #   so read and bind each separately, then join.
    this_sheet_names <-
      readxl::excel_sheets(
        path      = file.path(path_data, dat_names_weekly_claims[i_sheet])
      )

    this_sheet_names_header <-
      this_sheet_names |>
      stringr::str_subset(
        pattern = stringr::fixed("header")
      )
    this_sheet_names_line <-
      this_sheet_names |>
      stringr::str_subset(
        pattern = stringr::fixed("line")
      )

    dat_this_header <- list()
    for (i_sheet_header in seq_along(this_sheet_names_header)) {
      ## i_sheet_header = 1
      dat_this_header[[ i_sheet_header ]] <-
        readxl::read_xlsx(
          path      = file.path(path_data, dat_names_weekly_claims[i_sheet])
        , sheet     = this_sheet_names_header[i_sheet_header]
        , guess_max = 1e5
        , progress  = FALSE
        ) |>
        dplyr::select(
          Client_System_ID
        , TCN
        , Client_COE_Cd
        )
    }
    dat_this_header <-
      dat_this_header |>
      dplyr::bind_rows()

    dat_this_line <- list()
    for (i_sheet_line in seq_along(this_sheet_names_line)) {
      ## i_sheet_line = 1
      dat_this_line[[ i_sheet_line ]] <-
        readxl::read_xlsx(
          path      = file.path(path_data, dat_names_weekly_claims[i_sheet])
        , sheet     = this_sheet_names_line[i_sheet_line]
        , guess_max = 1e5
        , progress  = FALSE
        ) |>
        dplyr::select(
          Line_Svc_Date_First
        , TCN
        , Proc_Cd
        , Line_Allowed_Units
        , Line_Pd_Units
        , Line_Proc_Code_Mod1
        , Line_Proc_Code_Mod2
        , Line_Billed_Amt
        #, Line_Allowed_Chrg_Amt
        , Line_Pd_Amt
        #, Rndr_Prov_ID
        ) |>
        dplyr::rename(
          Conduent_Omnicad_Line_Billed_Amt = Line_Billed_Amt
        , Conduent_Omnicad_Line_Pd_Amt     = Line_Pd_Amt
        #, Prov_ID                          = Rndr_Prov_ID
        ) |>
        dplyr::mutate(
          Line_Svc_Date_First       = Line_Svc_Date_First |> lubridate::as_date()
        , Conduent_Omnicad_Diff_Amt = Conduent_Omnicad_Line_Pd_Amt - Conduent_Omnicad_Line_Billed_Amt
        )
    }
    dat_this_line <-
      dat_this_line |>
      dplyr::bind_rows()

    # join header and line
    #dat_sheet_weekly_claims[[ i_sheet ]] <-
    out  <-
      dplyr::left_join(
        dat_this_header
      , dat_this_line
      , by = dplyr::join_by(TCN)
      ) |>
      # pre-calculate features (otherwise too huge when a single file)
      dd_features_client_Conduent_Omnicad_read()

    # # pre-calculate features (otherwise too huge when a single file)
    # dat_sheet_weekly_claims[[ i_sheet ]] <-
    #   dat_sheet_weekly_claims[[ i_sheet ]] |>
    #   dd_features_client_Conduent_Omnicad_read()

  #} # i_sheet

    # use a return value
    return( out )

  } # foreach

  tictoc::toc() # end timer

  # explicitly close the implicitly created cluster
  doParallel::stopImplicitCluster()


  dat_sheet_weekly_claims <-
    #dat_sheet_weekly_claims |>
    results |>
    dplyr::bind_rows() |>
    dplyr::arrange(
      Client_System_ID
    , Line_Svc_Date_First
    ) |>
    dplyr::distinct()

  dat_client_Conduent_Omnicad <-
    dat_sheet_weekly_claims |>
    dplyr::select(
      -Client_COE_Cd
    ) |>
    dplyr::mutate(
      dat_client_Conduent_Omnicad = TRUE
    )

  #dat_client_Conduent_Omnicad |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  #### Takes too long and data are not missing
  ## if (sw_plot_missing) {
  ##   dd_plot_missing_codebook(
  ##       dat_this         = dat_client_Conduent_Omnicad
  ##     , name_dat         = name_dat
  ##     , path_results_dat = path_results_dat
  ##     #, sw_width         = 10
  ##     #, sw_height        = 10
  ##     , sw_codebook      = sw_codebook
  ##     )
  ## }


  # 12/17/2023 Replaced by Client_WaiverType
  #
  # # Waiver continued --------------------
  # dat_client_Conduent_Omnicad_Waiver <-
  #   dat_client_Conduent_Omnicad_Waiver_DD |>
  #   dplyr::full_join(
  #     dat_sheet_weekly_claims |>
  #     dplyr::select(
  #       Client_System_ID
  #     , Client_COE_Cd
  #     ) |>
  #     dplyr::distinct()
  #   , by = dplyr::join_by(Client_System_ID)
  #   ) |>
  #   dplyr::mutate(
  #     Client_Waiver =
  #       dplyr::case_when(
  #         Client_Waiver == "DD"                         ~ "DD"
  #         # Client_COE_Cd - Waiver 095 MedFrag, 096 DD Waiver and Mi Via Waiver
  #       , is.na(Client_Waiver) & Client_COE_Cd == "096" ~ "MV"
  #       , is.na(Client_Waiver) & Client_COE_Cd == "095" ~ "MF"
  #       , TRUE                                          ~ ""
  #       ) |>
  #       factor(levels = c("DD", "MV", "MF"))
  #   ) |>
  #   dplyr::select(
  #     Client_System_ID
  #   , Client_Waiver
  #   )
  #
  # #dat_client_Conduent_Omnicad_Waiver |> str()
  #
  # name_dat_waiver |> dd_save_to_RData()


  #return(dat_client_Conduent_Omnicad)

  # list_dat_client_Conduent_Omnicad <-
  #   list(
  #     dat_client_Conduent_Omnicad        = dat_client_Conduent_Omnicad
  #   , dat_client_Conduent_Omnicad_Waiver = dat_client_Conduent_Omnicad_Waiver
  #   )
  # return(list_dat_client_Conduent_Omnicad)

  return(dat_client_Conduent_Omnicad)

} # dd_read_client_Conduent_Omnicad
