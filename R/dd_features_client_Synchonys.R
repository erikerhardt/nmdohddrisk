#' Features for Client Synchonys
#'
#' @param dat_client_Synchonys    dat_client_Synchonys data from \code{dd_read_client_Synchonys()}
#'
#' @return dat_client_Synchonys
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
#' dd_features_client_Synchonys(
#'     dat_client_Synchonys
#'   )
#' }
dd_features_client_Synchonys <-
  function(
    dat_client_Synchonys    = NULL
  ) {

  dat_client_Synchonys <-
    dat_client_Synchonys |>
    dplyr::mutate(

    )

  return(dat_client_Synchonys)

} # dd_read_client_Synchonys
