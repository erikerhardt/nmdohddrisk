#' Features for Provider PDS
#'
#' @param dat_provider_PDS    dat_provider_PDS data from \code{dd_read_provider_PDS()}
#'
#' @return dat_provider_PDS
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
#' dd_features_provider_PDS(
#'     dat_provider_PDS
#'   )
#' }
dd_features_provider_PDS <-
  function(
    dat_provider_PDS    = NULL
  ) {

  dat_provider_PDS <-
    dat_provider_PDS |>
    dplyr::mutate(

    )

  return(dat_provider_PDS)

} # dd_features_provider_PDS
