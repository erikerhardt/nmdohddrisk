#' Features for Client Match
#'
#' @param dat_client_Match    dat_client_Match data from \code{dd_read_client_Match()}
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
#' dd_features_client_Match(
#'     dat_client_Match
#'   )
#' }
dd_features_client_Match <-
  function(
    dat_client_Match  = NULL
  ) {

  dat_client_Match <-
    dat_client_Match |>
    #dplyr::mutate(
    #)
    # Match Client_System_ID
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_Match)

} # dd_features_client_Match
