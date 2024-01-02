#' Features for Client CaseNotes
#'
#' @param dat_client_CaseNotes      dat_client_CaseNotes data from \code{dd_read_client_CaseNotes()}
#' @param dat_client_Match          dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_CaseNotes
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
#' dd_features_client_CaseNotes(
#'     dat_client_CaseNotes
#'   )
#' }
dd_features_client_CaseNotes <-
  function(
    dat_client_CaseNotes  = NULL
  , dat_client_Match      = dat_client_Match
  ) {

  # Match Client_System_ID
  dat_client_CaseNotes <-
    dat_client_CaseNotes |>
    dplyr::left_join(
      dat_client_Match |>
      dplyr::select(
        Client_System_ID
      , Client_TherapID
      )
    , by = dplyr::join_by(Client_TherapID)
    ) |>
    dplyr::select(
      -Client_TherapID
    ) |>
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_CaseNotes)

} # dd_read_client_CaseNotes
