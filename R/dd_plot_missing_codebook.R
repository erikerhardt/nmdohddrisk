#' Plot data missing values and create codebook
#'
#' @param dat_this          data to plot
#' @param name_dat          name of dataset, such as \code{dat_XXX}
#' @param path_results_dat  path to results
#' @param sw_width          missing plot width in inches
#' @param sw_height         missing plot height in inches
#' @param sw_codebook       T/F generate codebook
#'
#' @return                  NULL, invisibly
#' @importFrom  erikmisc e_plot_missing
#' @importFrom  dataReporter makeCodebook
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' dd_plot_missing_codebook(
#'     dat_this         = dat_GER
#'   , name_dat         = name_dat
#'   , path_results_dat = path_results_dat
#'   #, sw_width         = 10
#'   #, sw_height        = 10
#'   , sw_codebook      = sw_codebook
#'   )
#' }
dd_plot_missing_codebook <-
  function(
    dat_this         = NULL
  , name_dat         = ""
  , path_results_dat = path_results_dat
  , sw_width         = 10
  , sw_height        = 10
  , sw_codebook      = c(TRUE, FALSE)[1]
  ) {
  ## Codebook and missingness
  ggplot2::ggsave(
        file.path(
          path_results_dat
        , paste0("plot_missing_", name_dat, ".png")
        )
      , plot   = dat_this |> erikmisc::e_plot_missing(sw_title_data_name = name_dat) |> suppressWarnings()
      , width  = sw_width
      , height = sw_height
      ## png, jpeg
      , dpi    = 300
      , bg     = "white"
      ## pdf
      #, units  = "in"
      #, useDingbats = FALSE
      )

  if (sw_codebook) {
    dataReporter::makeCodebook(
        dat_this
      , file =
          file.path(
            path_results_dat
          , paste0("codebook_", name_dat, ".Rmd")
          )
      , reportTitle = paste0("NM DOH DD ", name_dat)
      , replace = TRUE, openResult = FALSE, codebook = TRUE, output = "html", maxProbVals = 20
      )
  }

  invisible(NULL)
} # dd_plot_missing_codebook
