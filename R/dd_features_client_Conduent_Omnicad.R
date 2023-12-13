#' Features for Client Conduent Omnicad
#'
#' @param dat_client_Conduent_Omnicad    dat_client_Conduent_Omnicad data from \code{dd_read_client_Conduent_Omnicad()}
#' @param dat_client_Match        dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_Conduent_Omnicad
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
#' dd_features_client_Conduent_Omnicad(
#'     dat_client_Conduent_Omnicad
#'   )
#' }
dd_features_client_Conduent_Omnicad <-
  function(
    dat_client_Conduent_Omnicad  = NULL
  , dat_client_Match      = dat_client_Match
  ) {

  dat_client_Conduent_Omnicad <-
    dat_client_Conduent_Omnicad |>
    dplyr::semi_join(
      dat_client_Match
    , by = dplyr::join_by(Client_System_ID)
    )

  # Match Client_TherapID
  # dat_client_Conduent_Omnicad already uses Client_System_ID

  return(dat_client_Conduent_Omnicad)

} # dd_read_client_Conduent_Omnicad
