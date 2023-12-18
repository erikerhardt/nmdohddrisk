#' Features for Client WaiverType
#'
#' Join with Match, return updated Match
#'
#' @param dat_client_WaiverType    dat_client_WaiverType data from \code{dd_read_client_WaiverType()}
#' @param dat_client_Match         dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_Match
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
#' dd_features_client_WaiverType(
#'     dat_client_WaiverType
#'   )
#' }
dd_features_client_WaiverType <-
  function(
    dat_client_WaiverType  = NULL
  , dat_client_Match       = dat_client_Match
  ) {

  dat_client_WaiverType <-
    dat_client_WaiverType |>
    #dplyr::mutate(
    #)
    # Match Client_System_ID
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  dat_client_Match <-
    dat_client_Match |>
    dplyr::left_join(
      dat_client_WaiverType
    , by = dplyr::join_by(Client_System_ID)
    ) |>
    tidyr::replace_na(
      list(Client_Waiver = "None")
    ) |>
    dplyr::mutate(
      Client_Waiver = Client_Waiver |> forcats::fct_drop()
    )

  return(dat_client_Match)

} # dd_read_client_WaiverType
