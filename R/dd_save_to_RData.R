#' Save any data object to .RData
#'
#' @param name_dat          name of data object to save
#' XXXparam path_results_dat  path to write .RData file
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' dd_save_to_RData(
#'     name_dat          = "dat_client_Match"
#'   )
#' }
dd_save_to_RData <-
  function(
    name_dat          = NULL
  #, path_results_dat  = path_results_dat
  ) {

  get("path_results_dat", envir = .GlobalEnv)

  #if (!is.null(path_results_dat)) {
    save(
      list =
        ls(
          pattern = name_dat
        , envir = .GlobalEnv
        )
    , file =
        file.path(
          path_results_dat
        , paste0(
            name_dat
          , ".RData"
          )
        )
    )
  #}

  invisible(NULL)

} # dd_save_to_RData
