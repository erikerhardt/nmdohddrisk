#' Features for Client BBS
#'
#' @param dat_client_BBS      dat_client_BBS data from \code{dd_read_client_BBS()}
#' @param dat_client_Match    dat_client_Match data from \code{dd_read_client_Match()}
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
  , dat_client_Match  = dat_client_Match
  ) {

  dat_client_BBS <-
    dat_client_BBS |>
    dplyr::mutate(

    )

  # Match Client_System_ID
    # ) |>
    # dplyr::left_join(
    #   dat_client_Match
    # ,
    # ) |>
    # dplyr::relocate(
    #   Client_System_ID
    # ) |>
    # dplyr::arrange(
    #   Client_System_ID

  return(dat_client_BBS)

} # dd_read_client_BBS
