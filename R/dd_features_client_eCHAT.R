#' Features for Client eCHAT
#'
#' @param dat_client_eCHAT      dat_client_eCHAT data from \code{dd_read_client_eCHAT()}
#' @param dat_client_Match    dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_eCHAT
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
#' dd_features_client_eCHAT(
#'     dat_client_eCHAT
#'   , dat_client_Match  = dat_client_Match
#'   )
#' }
dd_features_client_eCHAT <-
  function(
    dat_client_eCHAT    = NULL
  , dat_client_Match  = dat_client_Match
  ) {

  dat_client_eCHAT <-
    dat_client_Match |>
    dplyr::select(
      Client_System_ID
    , Client_SSN
    ) |>
    dplyr::left_join(
      dat_client_eCHAT
    , by = dplyr::join_by(Client_SSN)
    ) |>
    tidyr::replace_na(
      list(
        eCHAT_TotalYs    = 0
      , eCHAT_TotalScore = 0
      , eCHAT_Acuity     = "None"
      , dat_client_eCHAT = TRUE
      )
    ) |>
    dplyr::select(
      Client_System_ID
    , Date
    , eCHAT_TotalYs
    , eCHAT_TotalScore
    , eCHAT_Acuity
    ) |>
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_eCHAT)

} # dd_features_client_eCHAT
