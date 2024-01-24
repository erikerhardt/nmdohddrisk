#' Features for Client GER
#'
#' @param dat_client_GER      dat_client_GER data from \code{dd_read_client_GER()}
#' @param dat_client_Match    dat_client_Match data from \code{dd_read_client_Match()}
#'
#' @return dat_client_GER
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
#' dd_features_client_GER(
#'     dat_client_GER
#'   )
#' }
dd_features_client_GER <-
  function(
    dat_client_GER    = NULL
  , dat_client_Match  = dat_client_Match
  ) {

  ## Risk_Prediction_Model/Documentation/GER_Power Query Filter Downs for At Risk GER to Model.rtf
  # Columns needed for previous "At Risk" model
  #
  # note: "Other and Sub Event" is a combination of [Other Event], [Event Subtype]
  #
  # #"Inserted Merged Column" = Table.AddColumn(#"Replaced Value", "Other and Sub Event", each Text.Combine({[Other Event], [Event Subtype]}, ""), type text),
  #
  # # Definitions of At Risk categories
  # #"Added Conditional Column" =
  #   Table.AddColumn(
  #   #"Uppercased Text", "At Risk", each
  #   if Text.StartsWith([Injury Severity], "Severe")                    then "Injury"                    else
  #   if Text.StartsWith([Injury Severity], "Moderate")                  then "Injury"                    else
  #   if [Med Err Attn Reqd] = "Consult with Emergency Room"             then "Med Error"                 else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALER")            then "Emergency Service"         else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALAMB")           then "Emergency Service"         else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITLAURG")           then "Emergency Service"         else
  #   if Text.StartsWith([Other and Sub Event], "ASSAULTAGG")            then "Assault"                   else
  #   if Text.StartsWith([Other and Sub Event], "LAW ENFORCEMENT")       then "Law Enforcement"           else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALURGENT")        then "Emergency Service"         else
  #   if Text.StartsWith([Other and Sub Event], "AWOL/MISSING PERSON")   then "Elopement"                 else
  #   if Text.StartsWith([Other and Sub Event], "FALL")                  then "Fall"                      else
  #  if [Restraint Status]                     <> null                  then "Restraint"                 else
  #   if Text.StartsWith([Other and Sub Event], "OUT OF HOME PLACEMENT") then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALAD")            then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALRE")            then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "SUICIDE")               then "Suicide Related Behavior"  else
  #   if Text.StartsWith([Other and Sub Event], "PRN PSYCHOTROPIC USE")  then "PRN Psych Use"             else
  #  if Text.StartsWith([Injury Cause]       , "Fall")                  then "Fall"                      else null),

  dat_client_GER <-
    dat_client_GER |>
    # At Risk categories
    dplyr::mutate(
      GER_Other_and_Sub_Event =
        paste0(GER_Other_Event, GER_Event_Subtype)
    , GER_AtRisk_Injury =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Injury_Severity), pattern = "SEVERE"  ) |
          stringr::str_starts(string = toupper(GER_Injury_Severity), pattern = "MODERATE")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Med_Error =
        dplyr::case_when(
          GER_Med_Err_Attn_Reqd == "Consult with Emergency Room"
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Emergency_Service =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITALER"   ) |
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITALAMB"  ) |
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITLAURG"  ) |
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITALURGENT")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Assault =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "ASSAULTAGG")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Law_Enforcement =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "LAW ENFORCEMENT")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Elopement =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "AWOL/MISSING PERSON")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Fall =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "FALL")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Restraint =
        dplyr::case_when(
          !is.na(GER_Restraint_Status)
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Hospitalization =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "OUT OF HOME PLACEMENT") |
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITALAD"           ) |
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "HOSPITALRE"           )
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Suicide_Related_Behavior =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "SUICIDE")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_PRN_Psych_Use =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Other_and_Sub_Event), pattern = "PRN PSYCHOTROPIC USE")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Fall =
        dplyr::case_when(
          stringr::str_starts(string = toupper(GER_Injury_Cause), pattern = "FALL")
                ~ 1
        , TRUE  ~ 0
        )
    , GER_AtRisk_Sum =
        GER_AtRisk_Injury                   +
        GER_AtRisk_Med_Error                +
        GER_AtRisk_Emergency_Service        +
        GER_AtRisk_Assault                  +
        GER_AtRisk_Law_Enforcement          +
        GER_AtRisk_Elopement                +
        GER_AtRisk_Fall                     +
        GER_AtRisk_Restraint                +
        GER_AtRisk_Hospitalization          +
        GER_AtRisk_Suicide_Related_Behavior +
        GER_AtRisk_PRN_Psych_Use
    )

  # Match Client_System_ID
  dat_client_GER <-
    # GER, split and join with Match
    dplyr::bind_rows(
      # Match with Client_TherapID
      dat_client_GER |>
      dplyr::filter(
        !is.na(Client_TherapID)
      ) |>
      dplyr::select(
        -Client_SSN
      ) |>
      dplyr::left_join(
        dat_client_Match |>
        dplyr::select(
          Client_System_ID
        , Client_TherapID
        )
      , by = dplyr::join_by(Client_TherapID)
      )
      # Match with Client_SSN
    , dat_client_GER |>
      dplyr::filter(
        !is.na(Client_SSN)
      ) |>
      dplyr::select(
        -Client_TherapID
      ) |>
      dplyr::left_join(
        dat_client_Match |>
        dplyr::select(
          Client_System_ID
        , Client_SSN
        )
      , by = dplyr::join_by(Client_SSN)
      )
    ) |>
    dplyr::select(
      -Client_SSN
    , -Client_TherapID
    ) |>
    dplyr::relocate(
      Client_System_ID
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_GER)

} # dd_features_client_GER
