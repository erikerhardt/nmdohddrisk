#' Features for Provider RORA
#'
#' @param dat_provider_RORA    dat_provider_RORA data from \code{dd_read_provider_RORA()}
#'
#' @return dat_provider_RORA
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
#' dd_features_provider_RORA(
#'     dat_provider_RORA
#'   )
#' }
dd_features_provider_RORA <-
  function(
    dat_provider_RORA    = NULL
  ) {

  dat_provider_RORA <-
    dat_provider_RORA |>
    dplyr::mutate(

    )

  return(dat_provider_RORA)

} # dd_read_provider_RORA
