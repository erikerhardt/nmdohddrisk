#' NM DOH DD Prediction Model, main predict and model function
#'
#' @param working_directory             Working directory
#' @param path_prefix_out               Path and file prefix for output folders
#' @param DD_Group                      All or specific Waiver group
#' @param date_Current                  Date for last day of data to use for analysis
#' @param m_months_GER                  GER       data, date range to keep, from first ANE or last observation back number of months
#' @param m_months_Syncronys            Syncronys data, date range to keep, from first ANE or last observation back number of months
#' @param m_months_Conduent_Omnicad     Omnicad   data, date range to keep, from first ANE or last observation back number of months
#' @param m_months_RORA                 RORA      data, date range to keep, from first ANE or last observation back number of months
#' @param m_months_BBS                  BBS       data, date range to keep, from first ANE or last observation back number of months
#' @param sw_rfsrc_ntree                passed to \code{e_rfstr_classification()}, Random forest, number of trees
#' @param sw_alpha_sel                  passed to \code{e_rfstr_classification()}, Random forest, model selection alpha level
#' @param sw_unit_of_analysis           ID for first ANE or (ID, Date) for all ANE.
#' @param sw_select_full                passed to \code{e_rfstr_classification()}, run RF with model selection, or only fit full model
#' @param var_subgroup_analysis         passed to \code{e_rfstr_classification()}, variable(s) list (in \code{c(var1, var2)}) for subgroup analysis (group-specific ROC curves and confusion matrices) using ROC threshold from non-subgroup ROC curve, or \code{NULL} for none
#' @param sw_imbalanced_binary          passed to \code{e_rfstr_classification()}, T/F to use standard or imbalanced binary classification with \code{rfsrc::imbalanced()}.  Also increases ntree to \code{5 * sw_rfsrc_ntree}.
#' @param sw_quick_full_only            passed to \code{e_rfstr_classification()}, T/F to only fit full model and return model object
#' @param sw_m_months_select_quick_full T/F to only search for best \code{m_months} arguments.
#' @param sw_m_months_values            if \code{sw_m_months_select_quick_full == TRUE}, list of number of months for each data source to perform model selection on m_months arguments.  One strategy for search is to start with \code{c(1, 2)} and if the result is 1, then keep 1, otherwise try \code{c(2, 3)}, and so on for 4, 6, 9, and 12 months.  Increment only 4 values at a time for 16 processors.
#'
#' @return dat_client_RORA
#' @importFrom stats predict
#' @importFrom readxl read_xlsx
#' @import erikmisc
#' @import dplyr
#' @import forcats
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import readr
#' @import tidyselect
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import tictoc
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
dd_prediction_model <-
  function(
    params                        = params
  , path_list                     = path_list
  , m_months_GER                  = "4 months"  #"6 months"  # Date range to keep previous to first ANE or last observation
  , m_months_Syncronys            = "3 months"  #"2 months"
  , m_months_Conduent_Omnicad     = "4 months"  #"2 months"
  , m_months_RORA                 = "4 months"  #"2 months"
  , m_months_BBS                  = "2 months"  #"12 months"
  , m_months_CaseNotes            = "4 months"  #"2 months"
  , sw_rfsrc_ntree                = 500
  , sw_alpha_sel                  = 0.20
  , sw_unit_of_analysis           = c("Client_System_ID", "Client_System_ID__ANE_Date")[1]
  , sw_select_full                = c("select", "full")[1]
  , var_subgroup_analysis         = "Client_Waiver"
  , sw_imbalanced_binary          = c(FALSE, TRUE)[1]
  , sw_quick_full_only            = c(FALSE, TRUE)[1]
  , n_boot_resamples              = 20
  , sw_m_months_select_quick_full = FALSE
  , sw_m_months_values            = list(  # or just FALSE
                                      GER              = c(1, 2)
                                    , Syncronys        = c(1, 2)
                                    , Conduent_Omnicad = c(1)
                                    , RORA             = c(1)
                                    , BBS              = c(1)
                                    , CaseNotes        = c(1)
                                    )
  ) {
  ## working_directory             = "D:/Dropbox/StatAcumen/consult/Industry/2023_NM_DOH_DDWaiver/data/Zdrive/Risk_Prediction_Model/Scripts"
  ## path_prefix_out               = "out_V14"
  ## DD_Group                      = c("All", "DD", "MV", "MF")[1]
  ## date_Current                  = "2023-12-01"

  # working_directory             <- params$working_directory
  # path_prefix_out               <- path_list$path_prefix_out
  # DD_Group                      <- params$DD_Group
  # date_today                    <- params$date_today

  date_Current                  <- params$date_Current |> lubridate::as_date()

  name_analysis <-
    paste0(
      "NM DOH DD Risk Prediction Model: "
    , path_list$path_prefix_out
    , ", "
    , "Group "
    , params$DD_Group
    , ", "
    , "last date "
    , params$date_Current
    , ", "
    #, "("
    #, "GER "
    #, m_months_GER               |> stringr::str_replace(pattern = " months", "mo")
    #, ", "
    #, "Syncronys "
    #, m_months_Syncronys         |> stringr::str_replace(pattern = " months", "mo")
    #, ", "
    #, "Conduent_Omnicad "
    #, m_months_Conduent_Omnicad  |> stringr::str_replace(pattern = " months", "mo")
    #, ", "
    #, "RORA "
    #, m_months_RORA              |> stringr::str_replace(pattern = " months", "mo")
    #, ", "
    #, "BBS "
    #, m_months_BBS               |> stringr::str_replace(pattern = " months", "mo")
    #, ", "
    #, "CaseNotes "
    #, m_months_CaseNotes         |> stringr::str_replace(pattern = " months", "mo")
    #, ")"
    )


  # library(nmdohddrisk)
  # library(erikmisc)
  # library(tidyverse)
  #
  # # Parallel processing
  # library(parallel)


  # Parallel processing
  #library(parallel)
  #options(rf.cores = parallel::detectCores() - 1) # OpenMP Parallel Processing
  #options(mc.cores = parallel::detectCores() - 1) # R-side Parallel Processing

  dir_old <- setwd(params$working_directory)

  fn_list <- list.files(path_list$path_results_dat, pattern = "\\.RData")

  # load all data
  for (n_fn in fn_list) {
    load(file.path(path_list$path_results_dat, n_fn))
  }

  # ANE Before and After date_Current (for output file)
  dat_client_IMB_ANE_before_after <-
    dat_client_IMB_ANE |>
    dplyr::mutate(
      ANE_Before =
        dplyr::case_when(
          (Date <= params$date_Current) & (ANE_Substantiated == 1) ~ Date
        , TRUE ~ NA #"No ANE Before"
        )
    , ANE_After =
        dplyr::case_when(
          (Date >  params$date_Current) & (ANE_Substantiated == 1) ~ Date
        , TRUE ~ NA #"No ANE After"
        )
    ) |>
    dplyr::select(
      Client_System_ID
    , ANE_Before
    , ANE_After
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(
      Client_System_ID
    ) |>
    dplyr::summarize(
      ANE_Before_First = ANE_Before |> dplyr::first(na_rm = TRUE)
    , ANE_Before_Last  = ANE_Before |> dplyr::last (na_rm = TRUE)
    , ANE_After_First  = ANE_After  |> dplyr::first(na_rm = TRUE)
    , ANE_After_Last   = ANE_After  |> dplyr::last (na_rm = TRUE)
    , ANE_Before_First = dplyr::case_when(!(ANE_Before_First == ANE_Before_Last) ~ ANE_Before_First, TRUE ~ NA)
    , ANE_After_Last   = dplyr::case_when(!(ANE_After_First  == ANE_After_Last ) ~ ANE_After_Last  , TRUE ~ NA)
    ) |>
    #tidyr::fill(
    #  ANE_Before_First
    #, .direction = "updown"
    #) |>
    #tidyr::fill(
    #  ANE_After_Last
    #, .direction = "downup"
    #) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::right_join(
      dat_client_Match |>
      dplyr::select(
        Client_System_ID
      )
    , by = dplyr::join_by(Client_System_ID)
    ) |>
    dplyr::select(
      Client_System_ID
    , ANE_Before_First
    , ANE_Before_Last
    , ANE_After_First
    , ANE_After_Last
    )



  # # ANE Before and After date_Current (for output file)
  # dat_client_IMB_ANE_before_after <-
  #   dat_client_IMB_ANE |>
  #   dplyr::mutate(
  #     ANE_Before_First =
  #       dplyr::case_when(
  #         (Date <= params$date_Current) & (ANE_Substantiated == 1) ~ Date
  #       , TRUE ~ NA #"No ANE Before"
  #       )
  #   , ANE_After_Last =
  #       dplyr::case_when(
  #         (Date >  params$date_Current) & (ANE_Substantiated == 1) ~ Date
  #       , TRUE ~ NA #"No ANE After"
  #       )
  #   ) |>
  #   dplyr::select(
  #     Client_System_ID
  #   , ANE_Before_First
  #   , ANE_After_Last
  #   ) |>
  #   dplyr::distinct() |>
  #   dplyr::group_by(
  #     Client_System_ID
  #   ) |>
  #   dplyr::mutate(
  #     ANE_Before_First = ANE_Before_First |> dplyr::first(na_rm = TRUE)
  #   , ANE_After_Last   = ANE_After_Last   |> dplyr::last (na_rm = TRUE)
  #   ) |>
  #   #tidyr::fill(
  #   #  ANE_Before_First
  #   #, .direction = "updown"
  #   #) |>
  #   #tidyr::fill(
  #   #  ANE_After_Last
  #   #, .direction = "downup"
  #   #) |>
  #   dplyr::ungroup() |>
  #   dplyr::distinct() |>
  #   dplyr::right_join(
  #     dat_client_Match |>
  #     dplyr::select(
  #       Client_System_ID
  #     )
  #   , by = dplyr::join_by(Client_System_ID)
  #   )




  if (!sw_m_months_select_quick_full) {

    ## ----------------------------------------
    # Train data

    # The unit of analysis is (Client_System_ID, ANE_Date)

    list_dat_each_Model_Date_features_Train <-
      dd_list_dat_each_Model_Date_features(
        sw_ANE_Current                = c("ANE", "Current")[1]    # "ANE" to train, "Current" to predict
      , dat_client_Match              = dat_client_Match
      , dat_client_IMB_ANE            = dat_client_IMB_ANE
      , dat_client_GER                = dat_client_GER
      , dat_client_Syncronys          = dat_client_Syncronys
      , dat_client_RORA               = dat_client_RORA
      , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
      , dat_client_BBS                = dat_client_BBS
      , dat_client_CaseNotes          = dat_client_CaseNotes
      , dat_client_eCHAT              = dat_client_eCHAT
      , date_Current                  = date_Current
      , sw_unit_of_analysis           = sw_unit_of_analysis
      , m_months_GER                  = m_months_GER
      , m_months_Syncronys            = m_months_Syncronys
      , m_months_Conduent_Omnicad     = m_months_Conduent_Omnicad
      , m_months_RORA                 = m_months_RORA
      , m_months_BBS                  = m_months_BBS
      , m_months_CaseNotes            = m_months_CaseNotes
      )

    # Waiver subset
    if ( !(params$DD_Group == "All") ) {
      for (i_list in seq_len(length(list_dat_each_Model_Date_features_Train))) {
        ## i_list = 3
        list_dat_each_Model_Date_features_Train[[ i_list ]] <-
          list_dat_each_Model_Date_features_Train[[ i_list ]] |>
          dplyr::filter(
            Client_Waiver == params$DD_Group
          ) |>
          dplyr::select(
            -Client_Waiver
          )
      }
    }


    if (sw_unit_of_analysis == c("Client_System_ID", "Client_System_ID__ANE_Date")[1]) {

      #firstANE# # remove ANE_Date
      #firstANE# ## lapply(list_dat_each_Model_Date_features_Train, str)
      #firstANE# for (i_list in seq_len(length(list_dat_each_Model_Date_features_Train))) {
      #firstANE#   list_dat_each_Model_Date_features_Train[[ i_list ]] <-
      #firstANE#     list_dat_each_Model_Date_features_Train[[ i_list ]] |>
      #firstANE#     dplyr::select(
      #firstANE#       -tidyselect::all_of("ANE_Date")
      #firstANE#     )
      #firstANE# }

      if ( params$DD_Group == "All" ) {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
          )
      } else {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
          )
      }

    } # "Client_System_ID"
    if (sw_unit_of_analysis == c("Client_System_ID", "Client_System_ID__ANE_Date")[2]) {

      if ( params$DD_Group == "All" ) {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
          )
      } else {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
          )
      }

      # The unit of analysis is (Client_System_ID, ANE_Date)
      dat_all_Model_ID_Train <-
        dat_all_Model_ID_Train |>
        tidyr::unite(
          "Client_System_ID__ANE_Date"
        , Client_System_ID
        , ANE_Date
        , sep = "__"
        )
    } # "Client_System_ID__ANE_Date"


    # Projection plot
    p_proj <-
      e_plot_projection(
        dat_plot                  =
          dat_all_Model_ID_Train |>
          dplyr::select(
            -tidyselect::any_of(
              c(
                "Client_System_ID__ANE_Date"
              , "Client_System_ID"
              , "ANE_Date"
              )
            )
          )
      , var_group                 = "ANE_Substantiated"
      , var_color                 = NULL
      , var_shape                 = NULL
      , var_facet                 = NULL #"Client_Waiver"
      , text_title                = paste0("ANE Substantiated: ", name_analysis)
      , sw_print                  = FALSE
      , sw_projection             = c("umap", "tsne")[1]
      , n_obs_sample              = NULL
      )

    ggsave(
          file.path(
            path_list$path_results_out
          , path_list$path_prefix_out
          , paste0(
              path_list$path_prefix_out
            , "__"
            , "plot_projection_train"
            , ".png"
            )
          )
      , plot   = p_proj
      , width  = 10
      , height = 10
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, units  = "in"
      #, useDingbats = FALSE
      )




    #dat_all_Model_ID_Train
    #dat_all_Model_ID_Train |> str()
    #dat_all_Model_ID_Train |> summary()

    # Train model
    out_e_rf_Model_DD_Train <-
      e_rfsrc_classification(
        dat_rf_class            = dat_all_Model_ID_Train
      , rf_y_var                = NULL
      , rf_x_var                = NULL
      , rf_id_var               = sw_unit_of_analysis   #"Client_System_ID__ANE_Date"  # "Client_System_ID"
      , sw_rfsrc_ntree          = sw_rfsrc_ntree
      , sw_alpha_sel            = sw_alpha_sel
      , sw_select_full          = sw_select_full
      , sw_na_action            = c("na.omit", "na.impute")[1]
      , sw_save_model           = c(TRUE, FALSE)[2]
      , plot_title              = name_analysis
      , out_path                = file.path(path_list$path_results_out, path_list$path_prefix_out)
      , file_prefix             = path_list$path_prefix_out
      , var_subgroup_analysis   = var_subgroup_analysis
      , plot_format             = c("png", "pdf")[1]
      , n_marginal_plot_across  = 6
      , sw_imbalanced_binary    = sw_imbalanced_binary
      , sw_threshold_to_use     = c(FALSE, TRUE)[1]
      , sw_quick_full_only      = sw_quick_full_only
      , sw_reduce_output        = c(TRUE, FALSE)[1]
      , sw_subsample_bootstrap  = c(TRUE, FALSE)[2]
      , n_single_decision_tree_plots = 0
      , k_partial_coplot_var    = 0
      , n_boot_resamples        = n_boot_resamples
      )

    if (sw_quick_full_only) {
      out_e_rf_Model_DD_Train$o_class_sel     <- out_e_rf_Model_DD_Train$o_class_full
      out_e_rf_Model_DD_Train$o_class_sel_ROC <- out_e_rf_Model_DD_Train$o_class_full_ROC
    }

  } # !sw_m_months_select_quick_full

  # model selection
  if (sw_m_months_select_quick_full) {

    tab_m_months_iter <-
      sw_m_months_values |>
      expand.grid()

    print("nmdohddrisk::dd_prediction_model:  m_months selection, iterates:")
    print(tab_m_months_iter)

    #library(doParallel)  # for %dopar% operator
    num_cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(num_cores)

    tictoc::tic(msg = "Timer") # start timer

    # don't prespecify the "results" list

    results <-
      foreach::foreach(
        i_iter = seq_len(nrow(tab_m_months_iter))
      #, .combine =                             # default is a list
      , .inorder = TRUE                         # FALSE is faster
      , .packages = c("nmdohddrisk", "erikmisc", "tidyverse")  # character vector of packages that the tasks depend on
      , .export = NULL                          # character vector of variables to export
      #) %do% # sequential
      ) %dopar%  # parallel
      {
      ## i_iter = 1
      row_iter <- tab_m_months_iter[i_iter, ]

      ## ----------------------------------------
      # Train data
      list_dat_each_Model_Date_features_Train <-
        dd_list_dat_each_Model_Date_features(
          sw_ANE_Current                = c("ANE", "Current")[1]    # "ANE" to train, "Current" to predict
        , dat_client_Match              = dat_client_Match
        , dat_client_IMB_ANE            = dat_client_IMB_ANE
        , dat_client_GER                = dat_client_GER
        , dat_client_Syncronys          = dat_client_Syncronys
        , dat_client_RORA               = dat_client_RORA
        , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
        , dat_client_BBS                = dat_client_BBS
        , dat_client_CaseNotes          = dat_client_CaseNotes
        , dat_client_eCHAT              = dat_client_eCHAT
        , date_Current                  = params$date_Current
        , m_months_GER                  = row_iter$GER
        , m_months_Syncronys            = row_iter$Syncronys
        , m_months_Conduent_Omnicad     = row_iter$Conduent_Omnicad
        , m_months_RORA                 = row_iter$RORA
        , m_months_BBS                  = row_iter$BBS
        , m_months_CaseNotes            = row_iter$CaseNotes
        )

      # Waiver subset
      if ( !(params$DD_Group == "All") ) {
        for (i_list in seq_len(length(list_dat_each_Model_Date_features_Train))) {
          ## i_list = 3
          list_dat_each_Model_Date_features_Train[[ i_list ]] <-
            list_dat_each_Model_Date_features_Train[[ i_list ]] |>
            dplyr::filter(
              Client_Waiver == params$DD_Group
            ) |>
            dplyr::select(
              -Client_Waiver
            )
        }
      }

      if ( params$DD_Group == "All" ) {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
          )
      } else {
        dat_all_Model_ID_Train <-
          dd_dat_all_Model_ID(
            list_dat_features = list_dat_each_Model_Date_features_Train
          , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
          )
      }

      # Train model
      out_e_rf_Model_DD_Train <-
        e_rfsrc_classification(
          dat_rf_class            = dat_all_Model_ID_Train
        , rf_y_var                = NULL
        , rf_x_var                = NULL
        , rf_id_var               = "Client_System_ID"
        , sw_rfsrc_ntree          = sw_rfsrc_ntree
        , sw_alpha_sel            = sw_alpha_sel
        , sw_select_full          = sw_select_full
        , sw_na_action            = c("na.omit", "na.impute")[1]
        , sw_save_model           = c(TRUE, FALSE)[2]
        , plot_title              = name_analysis
        , out_path                = file.path(path_list$path_results_out, path_list$path_prefix_out)
        , file_prefix             = path_list$path_prefix_out
        , var_subgroup_analysis   = var_subgroup_analysis
        , plot_format             = c("png", "pdf")[1]
        , n_marginal_plot_across  = 6
        , sw_imbalanced_binary    = sw_imbalanced_binary
        , sw_threshold_to_use     = c(FALSE, TRUE)[1]
        , sw_quick_full_only      = sw_quick_full_only
        , sw_reduce_output        = c(TRUE, FALSE)[1]
        , sw_subsample_bootstrap  = c(TRUE, FALSE)[2]
        , n_single_decision_tree_plots = 0
        , k_partial_coplot_var    = 0
        , n_boot_resamples        = n_boot_resamples
        )

      out <-
        out_e_rf_Model_DD_Train$o_class_full_ROC$roc_curve_best$Yes |>
        dplyr::mutate(
          GER              = row_iter$GER
        , Syncronys        = row_iter$Syncronys
        , Conduent_Omnicad = row_iter$Conduent_Omnicad
        , RORA             = row_iter$RORA
        , BBS              = row_iter$BBS
        , CaseNotes        = row_iter$CaseNotes
        ) |>
        dplyr::relocate(
          GER
        , Syncronys
        , Conduent_Omnicad
        , RORA
        , BBS
        , CaseNotes
        )


      # use a return value
      return( out )

    } # foreach

    print(tictoc::toc()) # end timer

    # explicitly close the implicitly created cluster
    doParallel::stopImplicitCluster()

    out_results <-
      results |>
      dplyr::bind_rows() |>
      dplyr::arrange(
        dplyr::desc(`Geom Mean`)
      ) |>
      dplyr::relocate(
        `Geom Mean`
      , .after = "Group"
      )

    readr::write_csv(
        x = out_results
      , file =
          file.path(
            path_list$path_results_out
          , path_list$path_prefix_out
          , paste0(
              paste0(
                path_list$path_prefix_out
              , "__"
              , "m_months_selection__"
              , params$DD_Group
              , "_"
              , params$date_Current
              , "_"
              , Sys.time()
              ) |>
              stringr::str_replace_all("[^[:alnum:]]", "_")
              #stringr::str_replace_all("[[:punct:]]", "_")
            , ".csv"
            )
          )
      )

    return(out_results)

  } # sw_m_months_select_quick_full



  ## ----------------------------------------
  # Predict data
  list_dat_each_Model_Date_features_Predict <-
    dd_list_dat_each_Model_Date_features(
      sw_ANE_Current                = c("ANE", "Current")[2]    # "ANE" to train, "Current" to predict
    , dat_client_Match              = dat_client_Match
    , dat_client_IMB_ANE            = dat_client_IMB_ANE
    , dat_client_GER                = dat_client_GER
    , dat_client_Syncronys          = dat_client_Syncronys
    , dat_client_RORA               = dat_client_RORA
    , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
    , dat_client_BBS                = dat_client_BBS
    , dat_client_CaseNotes          = dat_client_CaseNotes
    , dat_client_eCHAT              = dat_client_eCHAT
    , date_Current                  = date_Current
    , sw_unit_of_analysis           = sw_unit_of_analysis
    , m_months_GER                  = m_months_GER
    , m_months_Syncronys            = m_months_Syncronys
    , m_months_Conduent_Omnicad     = m_months_Conduent_Omnicad
    , m_months_RORA                 = m_months_RORA
    , m_months_BBS                  = m_months_BBS
    , m_months_CaseNotes            = m_months_CaseNotes
    )

  # Waiver subset
  if ( !(params$DD_Group == "All") ) {
    for (i_list in seq_len(length(list_dat_each_Model_Date_features_Predict))) {
      ## i_list = 3
      list_dat_each_Model_Date_features_Predict[[ i_list ]] <-
        list_dat_each_Model_Date_features_Predict[[ i_list ]] |>
        dplyr::filter(
          Client_Waiver == params$DD_Group
        ) |>
        dplyr::select(
          -Client_Waiver
        )
    }
  }

  # if ( params$DD_Group == "All" ) {
  #   dat_all_Model_ID_Predict <-
  #     dd_dat_all_Model_ID(
  #       list_dat_features = list_dat_each_Model_Date_features_Predict
  #     , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
  #     )
  # } else {
  #   dat_all_Model_ID_Predict <-
  #     dd_dat_all_Model_ID(
  #       list_dat_features = list_dat_each_Model_Date_features_Predict
  #     , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
  #     )
  # }

  if (sw_unit_of_analysis == c("Client_System_ID", "Client_System_ID__ANE_Date")[1]) {

    #firstANE# # remove ANE_Date
    #firstANE# ## lapply(list_dat_each_Model_Date_features_Predict, str)
    #firstANE# for (i_list in seq_len(length(list_dat_each_Model_Date_features_Predict))) {
    #firstANE#   list_dat_each_Model_Date_features_Predict[[ i_list ]] <-
    #firstANE#     list_dat_each_Model_Date_features_Predict[[ i_list ]] |>
    #firstANE#     dplyr::select(
    #firstANE#       -tidyselect::all_of("ANE_Date")
    #firstANE#     )
    #firstANE# }

    if ( params$DD_Group == "All" ) {
      dat_all_Model_ID_Predict <-
        dd_dat_all_Model_ID(
          list_dat_features = list_dat_each_Model_Date_features_Predict
        , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
        )
    } else {
      dat_all_Model_ID_Predict <-
        dd_dat_all_Model_ID(
          list_dat_features = list_dat_each_Model_Date_features_Predict
        , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
        )
    }

  } # "Client_System_ID"
  if (sw_unit_of_analysis == c("Client_System_ID", "Client_System_ID__ANE_Date")[2]) {

    if ( params$DD_Group == "All" ) {
      dat_all_Model_ID_Predict <-
        dd_dat_all_Model_ID(
          list_dat_features = list_dat_each_Model_Date_features_Predict
        , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Client_Age)
        )
    } else {
      dat_all_Model_ID_Predict <-
        dd_dat_all_Model_ID(
          list_dat_features = list_dat_each_Model_Date_features_Predict
        , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, ANE_Date, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Age)
        )
    }

    # The unit of analysis is (Client_System_ID, ANE_Date)
    dat_all_Model_ID_Predict <-
      dat_all_Model_ID_Predict |>
      tidyr::unite(
        "Client_System_ID__ANE_Date"
      , Client_System_ID
      , ANE_Date
      , sep = "__"
      )
  } # "Client_System_ID__ANE_Date"




  dat_all_Model_ID_Predict <-
    dat_all_Model_ID_Predict |>
    dplyr::select(
      -ANE_Substantiated
    )


  #dat_all_Model_ID_Predict
  #dat_all_Model_ID_Predict |> str()
  #dat_all_Model_ID_Predict |> summary()

  # Predict ANE
  out_e_rf_Model_DD_Predict <-
    predict(
      object        = out_e_rf_Model_DD_Train$o_class_sel
    , newdata       = dat_all_Model_ID_Predict
    )

  #out_e_rf_Model_DD_Predict
  #out_e_rf_Model_DD_Predict$predicted
  #out_e_rf_Model_DD_Predict$class

  dat_all_Model_ID_Predict_out_all <-
    dat_all_Model_ID_Predict |>
    dplyr::bind_cols(
      dplyr::bind_cols(
        out_e_rf_Model_DD_Predict$predicted
      #, tibble(Class_pred = out_e_rf_Model_DD_Predict$class)
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(Yes)
    ) |>
    dplyr::mutate(
    #  Client_System_ID  = forcats::fct_reorder(factor(Client_System_ID), dplyr::desc(Yes))
      Class =
        ifelse(
          Yes > out_e_rf_Model_DD_Train$o_class_sel_ROC$roc_curve_best$Yes$thresh
        , "Yes"
        , "No"
        ) |>
        factor(levels = c("Yes", "No"))
    , Yes = Yes |> round(3) |> as.numeric()
    ) |>
    dplyr::rename(
      Probability_ANE = Yes
    ) |>
    dplyr::relocate(
      Probability_ANE
    , Class
    , .after = Client_System_ID
    ) |>
    dplyr::left_join(
      out_e_rf_Model_DD_Train$o_class_sel_ROC$roc_curve$Yes |>
      dplyr::select(
        Sens
      , Spec
      , thresh
      ) |>
      dplyr::rename(
        Probability_ANE = thresh
      )
    , by = join_by(Probability_ANE)
    ) |>
    dplyr::select(
      -tidyselect::any_of("Client_Waiver")
    ) |>
    dplyr::left_join(
      dat_client_Match |>
      dplyr::select(
        Client_System_ID
      , Client_TherapID
      , Client_SSN
      , Client_Waiver
      #, Client_Region
      , Client_County
      , Client_Res_Addr_Line_1
      , Client_Res_Addr_Line_2
      , Client_Res_Addr_City
      , Client_Res_State_Code
      , Client_Res_Addr_Zip_5
      , Client_Res_Addr_Zip_4
      , Client_County
      )
    , by = dplyr::join_by(Client_System_ID)
    ) |>
    dplyr::left_join(
      dat_client_IMB_ANE_before_after
    , by = dplyr::join_by(Client_System_ID)
    ) |>
    dplyr::mutate(
      date_Current = params$date_Current
    , date_of_run  = lubridate::today()
    )




  dat_all_Model_ID_Predict_out <-
    dat_all_Model_ID_Predict_out_all |>
    dplyr::select(
      Client_System_ID
    , Client_TherapID
    , Client_SSN
    , Probability_ANE
    , Class
    , Sens
    , Spec
    , date_of_run
    , date_Current
    , ANE_Before_First
    , ANE_Before_Last
    , ANE_After_First
    , ANE_After_Last
    , Client_Waiver
    , Client_Res_Addr_Line_1
    , Client_Res_Addr_Line_2
    , Client_Res_Addr_City
    , Client_Res_State_Code
    , Client_Res_Addr_Zip_5
    , Client_Res_Addr_Zip_4
    , Client_County
    )


  save(
      dat_all_Model_ID_Predict_out_all
    , file =
        file.path(
          path_list$path_results_out
        , path_list$path_prefix_out
        , paste0(
            path_list$path_prefix_out
          , "__"
          , "dat_all_Model_ID_Predict_out_all"
          , ".RData"
          )
        )
    )
  readr::write_csv(
      x    = dat_all_Model_ID_Predict_out_all
    , file =
        file.path(
          path_list$path_results_out
        , path_list$path_prefix_out
        , paste0(
            path_list$path_prefix_out
          , "__"
          , "dat_all_Model_ID_Predict_out_all"
          , ".csv"
          )
        )
    )

  readr::write_csv(
      x    = dat_all_Model_ID_Predict_out
    , file =
        file.path(
          path_list$path_results_out
        , path_list$path_prefix_out
        , paste0(
            path_list$path_prefix_out
          , "__"
          , "dat_all_Model_ID_Predict_out"
          , ".csv"
          )
        )
    )



  dat_all_Model_ID_Predict_out_plot <-
    dat_all_Model_ID_Predict_out |>
    dplyr::mutate(
      Client_System_ID  =
        forcats::fct_reorder(
          factor(Client_System_ID)
        , dplyr::desc(Probability_ANE)
        )
    )


  p <- ggplot(dat_all_Model_ID_Predict_out_plot, aes(x = Client_System_ID, y = Probability_ANE, colour = Class))
  p <- p + theme_bw()
  p <- p + geom_hline(aes(yintercept = out_e_rf_Model_DD_Train$o_class_sel_ROC$roc_curve_best$Yes$thresh), colour = "black", linetype = c("none", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[3], linewidth = 0.3, alpha = 0.5)
  p <- p + geom_point(alpha = 1)
  p <- p + scale_x_discrete(breaks = NULL)
  if ( params$DD_Group == "All" ) {
    p <- p + facet_grid(. ~ Client_Waiver, space = "free_x", drop = TRUE)
  }
  p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) # remove axis labels
  p <- p + labs(
                  title     = name_analysis
                , subtitle  = "Client System ID by Classification probability"
                #, x         = "x"
                #, y         = "y"
                #, caption = paste0(  "Caption 1"
                #                  , "\nCaption 2"
                #                  )
                #, colour    = "Class"
                #, shape     = "Class"
                #, linetype  = "General Health"  #"Diagnosis"
                #, fill      = "Diagnosis"
                #, tag = "A"
                )
  p <- p + theme( # legend position inside plot (top-right = c(1,1),c(0.95,0.95))
              legend.justification  = c(1, 1)         # x, y anchor position of legend to align with position in plot
            , legend.position       = c(0.95, 0.95)   # x, y position inside plot for legend anchor
            #, legend.background     = element_blank() # blank background so points/grid shows through
            #, legend.key            = element_blank() # blank background so points/grid shows through
            )
  #print(p)

  ggsave(
        file.path(
          path_list$path_results_out
        , path_list$path_prefix_out
        , paste0(
            path_list$path_prefix_out
          , "__"
          , "plot_predictions"
          , ".png"
          )
        )
    , plot   = p
    , width  = 16
    , height = 8
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    #, units  = "in"
    #, useDingbats = FALSE
    )


  # Projection plot
  p_proj <-
    e_plot_projection(
      dat_plot                  =
        dat_all_Model_ID_Predict_out_all |>
        dplyr::select(
          tidyselect::any_of(c("Class", names(dat_all_Model_ID_Train)))
        , -Client_System_ID
        )
    , var_group                 = "Class"
    , var_color                 = NULL
    , var_shape                 = NULL
    , var_facet                 = NULL #"Client_Waiver"
    , text_title                = paste0("Classification: ", name_analysis)
    , sw_print                  = FALSE
    , sw_projection             = c("umap", "tsne")[1]
    , n_obs_sample              = NULL
    )

  ggsave(
        file.path(
          path_list$path_results_out
        , path_list$path_prefix_out
        , paste0(
            path_list$path_prefix_out
          , "__"
          , "plot_projection_class_sel"
          , ".png"
          )
        )
    , plot   = p_proj
    , width  = 10
    , height = 10
    ## png, jpeg
    , dpi    = 300
    , bg     = "white"
    ## pdf
    #, units  = "in"
    #, useDingbats = FALSE
    )



  setwd(dir_old)

  invisible(NULL)

} # dd_prediction_model
