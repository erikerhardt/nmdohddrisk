#' Features for Client RORA
#'
#' @param dat_client_RORA     dat_client_RORA data from \code{dd_read_client_RORA()}
#' @param dat_client_Match    dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_RORA
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
#' dd_features_client_RORA(
#'     dat_client_RORA
#'   )
#' }
dd_features_client_RORA <-
  function(
    dat_client_RORA   = NULL
  , dat_client_Match  = dat_client_Match
  ) {

  dat_client_RORA <-
    dat_client_RORA |>
    dplyr::mutate(
      C_RORA_Primary_Concern    = C_RORA_Primary_Concern    |>
                                  # forcats::fct_collapse(
                                  #   #"Healthcare Management"
                                  #   #"Rights & Protections"
                                  #   Other =
                                  #     c(
                                  #       "Therapy Service needed"
                                  #     , "Environmental Hazard"
                                  #     , "Documentation"
                                  #     )
                                  # ) |>
                                  # forcats::fct_infreq()
                                  forcats::fct_lump_n(
                                    n           = 2
                                  , other_level = "Other"
                                  , ties.method = c("min", "average", "first", "last", "random", "max")[1]
                                  ) |>
                                  forcats::fct_infreq()
    , C_RORA_Secondary_Concern  = C_RORA_Secondary_Concern  |>
                                  forcats::fct_lump_n(
                                    n           = 3
                                  , other_level = "Other"
                                  , ties.method = c("min", "average", "first", "last", "random", "max")[1]
                                  ) |>
                                  forcats::fct_infreq()
    )

  # Match Client_System_ID
  dat_client_RORA <-
    dat_client_RORA |>
    dplyr::filter(
      !is.na(Client_SSN)
    ) |>
    dplyr::left_join(
      dat_client_Match |>
      dplyr::select(
        Client_System_ID
      , Client_SSN
      )
    , by = dplyr::join_by(Client_SSN)
    ) |>
    dplyr::select(
      -Client_SSN
    ) |>
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_RORA)

} # dd_read_client_RORA
