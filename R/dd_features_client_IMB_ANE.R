#' Features for Client IMB_ANE
#'
#' @param dat_client_IMB_ANE    dat_client_IMB_ANE data from \code{dd_read_client_IMB_ANE()}
#' @param dat_client_Match      dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_IMB_ANE
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
#' dd_features_client_IMB_ANE(
#'     dat_client_IMB_ANE
#'   )
#' }
dd_features_client_IMB_ANE <-
  function(
    dat_client_IMB_ANE  = NULL
  , dat_client_Match    = dat_client_Match
  ) {

  # dat_client_IMB_ANE |> print(width=Inf)

  dat_client_IMB_ANE <-
    dat_client_IMB_ANE |>
    dplyr::mutate(
      ANE_Substantiated_Abuse        =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType == "Abuse")
        , 1
        , 0
        )
    , ANE_Substantiated_Sexual_Abuse =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType == "Sexual Abuse")
        , 1
        , 0
        )
    , ANE_Substantiated_Verbal_Abuse =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType == "Verbal Abuse")
        , 1
        , 0
        )
    , ANE_Substantiated_Neglect      =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType == "Neglect")
        , 1
        , 0
        )
    , ANE_Substantiated_Exploitation =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType == "Exploitation")
        , 1
        , 0
        )
    , ANE_Substantiated_Abuse_ANY    =
        ifelse(
          (ANE_Substantiated == 1) &
          (ANE_AllegationType %in% c("Abuse", "Sexual Abuse", "Verbal Abuse"))
        , 1
        , 0
        )
    ) |>
    dplyr::relocate(
      tidyselect::starts_with("ANE_Substantiated_")
    , .after = "ANE_Substantiated"
    )

  # Match Client_System_ID
  dat_client_IMB_ANE <-
    dat_client_IMB_ANE |>
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
    , Date
    )

  # 10/31/2023
  # Simple, just unique dates Substantiated or not
  dat_client_IMB_ANE <-
    dat_client_IMB_ANE |>
    dplyr::select(
      Client_System_ID
    , Date
    , ANE_Substantiated
    ) |>
    dplyr::arrange(
      Client_System_ID
    , Date
    ) |>
    dplyr::distinct()

  return(dat_client_IMB_ANE)

} # dd_read_client_IMB_ANE
