#' Features for Client Syncronys
#'
#' @param dat_client_Syncronys    dat_client_Syncronys data from \code{dd_read_client_Syncronys()}
#' @param dat_client_Match        dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_Syncronys
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
#' dd_features_client_Syncronys(
#'     dat_client_Syncronys
#'   )
#' }
dd_features_client_Syncronys <-
  function(
    dat_client_Syncronys  = NULL
  , dat_client_Match      = dat_client_Match
  ) {

  dat_client_Syncronys <-
    dat_client_Syncronys |>
    dplyr::select(
      Client_System_ID
    , Date
    , Syncronys_CarrierID
    #, Syncronys_MemberName
    #, Syncronys_DOB
    #, Syncronys_Gender
    #, Syncronys_Facility
    , Syncronys_AdmitDate
    #, Syncronys_AdmitReason
    , Syncronys_Type
    #, Syncronys_DischgDate
    #, Syncronys_DischgDx
    #, Syncronys_NumED30
    #, Syncronys_NumED365
    #, Syncronys_NumIP30
    #, Syncronys_NumIP365
    #, Syncronys_County
    #, Syncronys_Region
    #, dat_client_Syncronys
    ) |>
    dplyr::distinct()

  # Match Client_TherapID
  # dat_client_Syncronys already uses Client_System_ID

  return(dat_client_Syncronys)

} # dd_features_client_WaiverType
