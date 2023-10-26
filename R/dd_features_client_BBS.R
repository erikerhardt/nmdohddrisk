#' Features for Client BBS
#'
#' @param dat_client_BBS    dat_client_BBS data from \code{dd_read_client_BBS()}
#'
#' @return dat_client_BBS
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
#' dd_features_client_BBS(
#'     dat_client_BBS
#'   )
#' }
dd_features_client_BBS <-
  function(
    dat_client_BBS    = NULL
  ) {

  dat_client_BBS <-
    dat_client_BBS |>
    dplyr::mutate(

    )

  return(dat_client_BBS)

} # dd_read_client_BBS
