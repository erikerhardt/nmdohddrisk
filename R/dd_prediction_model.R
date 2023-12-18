#' NM DOH DD Prediction Model, main predict and model function
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_RORA
#' @importFrom readxl read_xlsx
#' @import erikmisc
#' @import dplyr
#' @import forcats
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import tidyselect
#' @import parallel
#' @import tsne
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
    working_directory         = "D:/Dropbox/StatAcumen/consult/Industry/2023_NM_DOH_DDWaiver/data/Zdrive/Risk_Prediction_Model/Scripts"
  , path_prefix_out           = "out_V06"
  , DD_Group                  = c("All", "DD", "MV", "MF")[1]
  , date_Current              = "2023-09-06"
    # Date range to keep previous to first ANE or last observation
  , m_months_GER              = "6 months"
  , m_months_Syncronys        = "2 months"
  , m_months_Conduent_Omnicad = "2 months"
  , m_months_RORA             = "2 months"
  , sw_rfsrc_ntree            = 500
  , sw_alpha                  = 0.20
  ) {

  name_analysis <-
    paste0(
      "NM DOH DD Prediction Model: "
    , path_prefix_out
    , ", "
    , "Group "
    , DD_Group
    , ", "
    , "last date "
    , date_Current
    , ", "
    , "("
    , "GER "
    , m_months_GER               |> stringr::str_replace(pattern = " months", "mo")
    , ", "
    , "Syncronys "
    , m_months_Syncronys         |> stringr::str_replace(pattern = " months", "mo")
    , ", "
    , "Conduent_Omnicad "
    , m_months_Conduent_Omnicad  |> stringr::str_replace(pattern = " months", "mo")
    , ", "
    , "RORA "
    , m_months_RORA              |> stringr::str_replace(pattern = " months", "mo")
    , ")"
    )


  # library(nmdohddrisk)
  # library(erikmisc)
  # library(tidyverse)
  #
  # # Parallel processing
  # library(parallel)


  # Parallel processing
  #library(parallel)
  options(rf.cores = parallel::detectCores() - 2) # OpenMP Parallel Processing
  options(mc.cores = parallel::detectCores() - 2) # R-side Parallel Processing

  dir_old <- setwd(working_directory)

  # file paths
  date_today <-
    lubridate::today()

  path_results <-
    "../Results"

  path_results_dat <-
    file.path(
      path_results
    , paste0(
        "dat_"
      , date_today
      )
    )
  path_results_out <-
    file.path(
      path_results
    , paste0(
        "out_"
      , date_today
      )
    )

  fn_list <- list.files(path_results_dat, pattern = "\\.RData")

  # load all data
  for (n_fn in fn_list) {
    load(file.path(path_results_dat, n_fn))
  }

  date_Current <-
    date_Current |>
    lubridate::as_date()


  # ANE to train, Current to predict
  sw_ANE_Current <- c("ANE", "Current")[1]

  ## ----------------------------------------
  # Train data
  list_dat_each_Model_Date_features_Train <-
    dd_list_dat_each_Model_Date_features(
        sw_ANE_Current                = c("ANE", "Current")[1]      # ANE to train, Current to predict
      , dat_client_Match              = dat_client_Match
      , dat_client_IMB_ANE            = dat_client_IMB_ANE
      , dat_client_GER                = dat_client_GER
      , dat_client_Syncronys          = dat_client_Syncronys
      , dat_client_RORA               = dat_client_RORA
      , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
      , dat_client_BBS                = dat_client_BBS
      , date_Current                  = date_Current
      , m_months_GER                  = m_months_GER
      , m_months_Syncronys            = m_months_Syncronys
      , m_months_Conduent_Omnicad     = m_months_Conduent_Omnicad
      , m_months_RORA                 = m_months_RORA
    )

  # Waiver subset
  if ( !(DD_Group == "All") ) {
    for (i_list in seq_len(length(list_dat_each_Model_Date_features_Train))) {
      ## i_list = 3
      list_dat_each_Model_Date_features_Train[[ i_list ]] <-
        list_dat_each_Model_Date_features_Train[[ i_list ]] |>
        dplyr::filter(
          Client_Waiver == DD_Group
        ) |>
        dplyr::select(
          -Client_Waiver
        )
    }
  }

  if ( DD_Group == "All" ) {
    dat_all_Model_ID_Train <-
      dd_dat_all_Model_ID(
        list_dat_features = list_dat_each_Model_Date_features_Train
      , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Age)
      )
  } else {
    dat_all_Model_ID_Train <-
      dd_dat_all_Model_ID(
        list_dat_features = list_dat_each_Model_Date_features_Train
      , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Age)
      )
  }


  # Projection plot
  p_proj <-
    e_plot_projection(
      dat_plot                  =
        dat_all_Model_ID_Train |>
        dplyr::select(
          -Client_System_ID
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
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
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
      dat_rf_class    = dat_all_Model_ID_Train
    , rf_y_var        = NULL
    , rf_x_var        = NULL
    , rf_id_var       = "Client_System_ID"
    , sw_rfsrc_ntree  = sw_rfsrc_ntree
    , sw_alpha        = sw_alpha
    , sw_save_model   = c(TRUE, FALSE)[2]
    , plot_title      = name_analysis
    , out_path        = file.path(path_results_out, path_prefix_out)
    , file_prefix     = path_prefix_out
    , plot_format     = c("png", "pdf")[1]
    , n_marginal_plot_across = 6
    )


  ## ----------------------------------------
  # Predict data
  list_dat_each_Model_Date_features_Predict <-
    dd_list_dat_each_Model_Date_features(
        sw_ANE_Current                = c("ANE", "Current")[2]      # ANE to train, Current to predict
      , dat_client_Match              = dat_client_Match
      , dat_client_IMB_ANE            = dat_client_IMB_ANE
      , dat_client_GER                = dat_client_GER
      , dat_client_Syncronys          = dat_client_Syncronys
      , dat_client_RORA               = dat_client_RORA
      , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
      , dat_client_BBS                = dat_client_BBS
      , date_Current                  = date_Current
      , m_months_GER                  = m_months_GER
      , m_months_Syncronys            = m_months_Syncronys
      , m_months_Conduent_Omnicad     = m_months_Conduent_Omnicad
      , m_months_RORA                 = m_months_RORA
    )

  # Waiver subset
  if ( !(DD_Group == "All") ) {
    for (i_list in seq_len(length(list_dat_each_Model_Date_features_Predict))) {
      ## i_list = 3
      list_dat_each_Model_Date_features_Predict[[ i_list ]] <-
        list_dat_each_Model_Date_features_Predict[[ i_list ]] |>
        dplyr::filter(
          Client_Waiver == DD_Group
        ) |>
        dplyr::select(
          -Client_Waiver
        )
    }
  }

  if ( DD_Group == "All" ) {
    dat_all_Model_ID_Predict <-
      dd_dat_all_Model_ID(
        list_dat_features = list_dat_each_Model_Date_features_Predict
      , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Client_Waiver, Age)
      )
  } else {
    dat_all_Model_ID_Predict <-
      dd_dat_all_Model_ID(
        list_dat_features = list_dat_each_Model_Date_features_Predict
      , by_for_join       = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Age)
      )
  }


  dat_all_Model_ID_Predict <-
    dat_all_Model_ID_Predict |>
    dplyr::select(
      -ANE_Substantiated
    )


  #dat_all_Model_ID_Predict
  #dat_all_Model_ID_Predict |> str()
  #dat_all_Model_ID_Predict |> summary()

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
      , Client_SSN
      , Client_Waiver
      #, Client_Region
      , Client_County
      # XXX Add street address
      )
    , by = dplyr::join_by(Client_System_ID)
    )

  dat_all_Model_ID_Predict_out <-
    dat_all_Model_ID_Predict_out_all |>
    dplyr::select(
      Client_System_ID
    , Probability_ANE
    , Class
    , Sens
    , Spec
    , Client_SSN
    , Client_Waiver
    , Client_County
    )


  save(
      dat_all_Model_ID_Predict_out_all
    , file =
        file.path(
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
          , "__"
          , "dat_all_Model_ID_Predict_out_all"
          , ".RData"
          )
        )
    )
  readr::write_csv(
      dat_all_Model_ID_Predict_out_all
    , file =
        file.path(
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
          , "__"
          , "dat_all_Model_ID_Predict_out_all"
          , ".csv"
          )
        )
    )

  readr::write_csv(
      dat_all_Model_ID_Predict_out
    , file =
        file.path(
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
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
  if ( DD_Group == "All" ) {
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
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
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
          path_results_out
        , path_prefix_out
        , paste0(
            path_prefix_out
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

} # dd_read_client_RORA
