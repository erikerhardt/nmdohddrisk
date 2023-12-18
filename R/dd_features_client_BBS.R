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
#'   , dat_client_Match  = dat_client_Match
#'   )
#' }
dd_features_client_BBS <-
  function(
    dat_client_BBS    = NULL
  , dat_client_Match  = dat_client_Match
  ) {

  dat_client_BBS <-
    dat_client_Match |>
    dplyr::select(
      Client_System_ID
    , Client_SSN
    ) |>
    dplyr::left_join(
      dat_client_BBS
    , by = dplyr::join_by(Client_SSN)
    ) |>
    tidyr::replace_na(
      list(BBS_AtRisk = 0)
    ) |>
    dplyr::select(
      Client_System_ID
    , BBS_AtRisk
    ) |>
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_BBS)

} # dd_read_client_BBS
