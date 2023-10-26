#' Features for Client RORA
#'
#' @param dat_client_RORA    dat_client_RORA data from \code{dd_read_client_RORA()}
#'
#' @return dat_client_RORA
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
#' dd_features_client_RORA(
#'     dat_client_RORA
#'   )
#' }
dd_features_client_RORA <-
  function(
    dat_client_RORA    = NULL
  ) {

  dat_client_RORA <-
    dat_client_RORA |>
    dplyr::mutate(

    )

  return(dat_client_RORA)

} # dd_read_client_RORA
