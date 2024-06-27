#' Set paths for data and output
#'
#' @param params              \code{params} object from qmd file, \code{NMDOH_DD_RiskPredictionModel_Data-Model-Summary_yyyymmdd.qmd}
#' @param path_dat_in         relative path from \code{params$working_directory} to input data folder, such as \code{"../Data_in"}
#' @param path_results        relative path from \code{params$working_directory} to results, such as \code{"../Results"}
#'
#' @return path_list          list of paths to data and output
#' @export
dd_set_paths <-
  function(
    params        = params
  , path_dat_in   = "../Data_in"
  , path_results  = "../Results"
  ) {

  # Set up folders
  dir_old <- setwd(params$working_directory)

  path_dat_in <-
    path_dat_in

  path_results_dat <-
    file.path(
      path_results
    , paste0(
        "dat_"
      , params$date_today
      )
    )
  path_results_out <-
    file.path(
      path_results
    , paste0(
        "out_"
      , params$date_today
      )
    )
  path_prefix_out <-
    paste0(
      params$path_prefix_out
    , "_"
    , params$date_Current
    , "_"
    , params$DD_Group
    )

  #if (!file.exists(path_results)) {
    dir.create(path_results, showWarnings = FALSE)
  #}
  #if (!file.exists(path_results_dat)) {
    dir.create(path_results_dat, showWarnings = FALSE)
  #}
  #if (!file.exists(path_results_out)) {
    dir.create(path_results_out, showWarnings = FALSE)
  #}
  #if (!file.exists(path_results_out)) {
    dir.create(file.path(path_results_out, path_prefix_out), showWarnings = FALSE)
  #}

  # save paths to return from function
  path_list <- list()
  path_list$path_results      <- path_results
  path_list$path_dat_in       <- path_dat_in
  path_list$path_results_dat  <- path_results_dat
  path_list$path_results_out  <- path_results_out
  path_list$path_original     <- dir_old
  path_list$path_prefix_out   <- path_prefix_out

  setwd(dir_old)

  return(path_list)

} # dd_set_paths
