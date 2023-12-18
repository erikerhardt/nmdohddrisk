## Functions to align data, select features, and join into one data for training or prediction


# Match
#' Title
#'
#' @param dat_client_Match dat_client_Match
#'
#' @return dat_client_Match_Model
#' @import dplyr
#' @export
#'
dd_dat_client_Match_Model <-
  function(
    dat_client_Match = dat_client_Match
  ) {

  dat_client_Match_Model <-
    dat_client_Match |>
    dplyr::select(
      Client_System_ID
    , Client_SSN
    , Client_TherapID
    , Client_Gender
    , Client_DOB
    , Client_Ethnicity
    , Client_Race
    , Client_Region
    , Client_Waiver
    ) |>
    dplyr::arrange(
      Client_System_ID
    )

  return(dat_client_Match_Model)
}


# first ANE substantiated event for each person
#' Title
#'
#' @param dat_client_IMB_ANE dat_client_IMB_ANE
#'
#' @return dat_client_IMB_ANE_Model
#' @import dplyr
#' @export
#'
dd_dat_client_IMB_ANE_Model <-
  function(
    dat_client_IMB_ANE = dat_client_IMB_ANE
  ) {

  dat_client_IMB_ANE_Model <-
    dat_client_IMB_ANE |>
    dplyr::filter(
      ANE_Substantiated == 1
    ) |>
    # sort so first is earliest event
    dplyr::arrange(
      Client_System_ID
    , Date
    #, dplyr::desc(ANE_Substantiated)
    ) |>
    dplyr::group_by(
      Client_System_ID
    ) |>
    dplyr::distinct() |>
    dplyr::slice(
      1
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(
      ANE_Date = Date
    )

  return(dat_client_IMB_ANE_Model)
}


#' Title
#'
#' @param dat_client_GER dat_client_GER
#'
#' @return dat_client_GER_Model
#' @import dplyr
#' @export
#'
dd_dat_client_GER_Model <-
  function(
    dat_client_GER = dat_client_GER
  ) {

  # GER, select features
  dat_client_GER_Model <-
    dat_client_GER |>
    dplyr::select(
      Client_System_ID
    , Date
    , GER_Provider
    , GER_Region
    , GER_Waiver
    , GER_Event_Date
    , GER_Event_Type
    , GER_Abuse_Suspected
    , GER_Neglect_Suspected
    , GER_Exploitation_Suspected
    , GER_AtRisk_Injury
    , GER_AtRisk_Med_Error
    , GER_AtRisk_Emergency_Service
    , GER_AtRisk_Assault
    , GER_AtRisk_Law_Enforcement
    , GER_AtRisk_Elopement
    , GER_AtRisk_Fall
    , GER_AtRisk_Restraint
    , GER_AtRisk_Hospitalization
    , GER_AtRisk_Suicide_Related_Behavior
    , GER_AtRisk_PRN_Psych_Use
    , GER_AtRisk_Sum
    ) |>
    dplyr::rename(
      GER_Date = Date
    )

  return(dat_client_GER_Model)
}


#' Title
#'
#' @param dat_client_Syncronys dat_client_Syncronys
#'
#' @return dat_client_Syncronys_Model
#' @import dplyr
#' @export
#'
dd_dat_client_Syncronys_Model <-
  function(
    dat_client_Syncronys = dat_client_Syncronys
  ) {

  # Syncronys, select features
  dat_client_Syncronys_Model <-
    dat_client_Syncronys |>
    dplyr::select(
    , Client_System_ID
    , Date
    , Syncronys_CarrierID
    , Syncronys_AdmitDate
    , Syncronys_Type
    ) |>
    dplyr::arrange(
      Client_System_ID
    , Syncronys_AdmitDate
    )

  return(dat_client_Syncronys_Model)
}


#' Title
#'
#' @param dat_client_RORA dat_client_RORA
#'
#' @return dat_client_RORA_Model
#' @import dplyr
#' @export
#'
dd_dat_client_RORA_Model <-
  function(
    dat_client_RORA = dat_client_RORA
  ) {

  # RORA, select features
  dat_client_RORA_Model <-
    dat_client_RORA |>
    dplyr::select(
      Client_System_ID
    , C_RORA_RoraType
    #, C_RORA_Waiver
    , C_RORA_RequestDate
    , C_RORA_Primary_Concern
    , C_RORA_Secondary_Concern
    , C_RORA_Care_or_Compliance
    , C_RORA_Risk
    ) |>
    dplyr::filter(
      C_RORA_RoraType == "Individual"
    #, C_RORA_Waiver   == "DD Waiver"
    ) |>
    dplyr::arrange(
      Client_System_ID
    , C_RORA_RequestDate
    )

  return(dat_client_RORA_Model)
}


#' Title
#'
#' @param dat_client_BBS dat_client_BBS
#'
#' @return dat_client_BBS_Model
#' @export
#'
dd_dat_client_BBS_Model <-
  function(
    dat_client_BBS = dat_client_BBS
  ) {

  # BBS, select features
  dat_client_BBS_Model <-
    dat_client_BBS

  return(dat_client_BBS_Model)
}


#' Title
#'
#' @param dat_client_GER_Model dat_client_GER_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#'
#' @return dat_client_GER_Model_Date
#' @import dplyr
#' @export
#'
dd_dat_client_GER_Model_Date <-
  function(
    dat_client_GER_Model        = dat_client_GER_Model
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  ) {

  if (sw_ANE_Current == c("ANE", "Current")[1]) {
    # GER, for ANE, keep only data prior to the first ANE date
    dat_client_GER_Model_Date <-
      dat_client_GER_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      , relationship = "many-to-many"
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        is.na(ANE_Date) |
        (GER_Event_Date <= ANE_Date)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        -ANE_Date
      , -ANE_Substantiated
      )
  }

  if (sw_ANE_Current == c("ANE", "Current")[2]) {
    dat_client_GER_Model_Date <-
      dat_client_GER_Model |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        (GER_Event_Date <= date_Current)
      ) |>
      dplyr::ungroup()
  }

  return(dat_client_GER_Model_Date)
}


#' Title
#'
#' @param dat_client_RORA_Model dat_client_RORA_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#'
#' @return dat_client_RORA_Model_Date
#' @import dplyr
#' @export
#'
dd_dat_client_RORA_Model_Date <-
  function(
    dat_client_RORA_Model       = dat_client_RORA_Model
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  ) {

  if (sw_ANE_Current == c("ANE", "Current")[1]) {
    # RORA, for ANE, keep only data prior to the first ANE date
    dat_client_RORA_Model_Date <-
      dat_client_RORA_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      , relationship = "many-to-many"
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        is.na(ANE_Date) |
        (C_RORA_RequestDate <= ANE_Date)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        -ANE_Date
      , -ANE_Substantiated
      )
  }

  if (sw_ANE_Current == c("ANE", "Current")[2]) {
    dat_client_RORA_Model_Date <-
      dat_client_RORA_Model |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        (C_RORA_RequestDate <= date_Current)
      ) |>
      dplyr::ungroup()
  }

  return(dat_client_RORA_Model_Date)
}


#' Title
#'
#' @param dat_client_Syncronys dat_client_Syncronys
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#'
#' @return dat_client_Syncronys_Model_Date
#' @import dplyr
#' @export
#'
dd_dat_client_Syncronys_Model_Date <-
  function(
    dat_client_Syncronys        = dat_client_Syncronys
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  ) {

  if (sw_ANE_Current == c("ANE", "Current")[1]) {
    # Syncronys, for ANE, keep only data prior to the first ANE date
    dat_client_Syncronys_Model_Date <-
      dat_client_Syncronys |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      , relationship = "many-to-many"
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        is.na(ANE_Date) |
        (Syncronys_AdmitDate <= ANE_Date)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        -ANE_Date
      , -ANE_Substantiated
      )

  }

  if (sw_ANE_Current == c("ANE", "Current")[2]) {
    dat_client_Syncronys_Model_Date <-
      dat_client_Syncronys |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        (Syncronys_AdmitDate <= date_Current)
      ) |>
      dplyr::ungroup()
  }

  return(dat_client_Syncronys_Model_Date)
}


#' Title
#'
#' @param dat_client_Conduent_Omnicad dat_client_Conduent_Omnicad
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#'
#' @return dat_client_Conduent_Omnicad_Model_Date
#' @import dplyr
#' @export
#'
dd_dat_client_Conduent_Omnicad_Model_Date <-
  function(
    dat_client_Conduent_Omnicad = dat_client_Conduent_Omnicad
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  ) {

  if (sw_ANE_Current == c("ANE", "Current")[1]) {
    # Omnicad, for ANE, keep only data prior to the first ANE date
    dat_client_Conduent_Omnicad_Model_Date <-
      dat_client_Conduent_Omnicad |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      , relationship = "many-to-many"
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        is.na(ANE_Date) |
        (Line_Svc_Date_First <= ANE_Date)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        -ANE_Date
      , -ANE_Substantiated
      )
  }

  if (sw_ANE_Current == c("ANE", "Current")[2]) {
    dat_client_Conduent_Omnicad_Model_Date <-
      dat_client_Conduent_Omnicad |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::filter(
        (Line_Svc_Date_First <= date_Current)
      ) |>
      dplyr::ungroup()
  }

  return(dat_client_Conduent_Omnicad_Model_Date)
}


#' Title
#'
#' @param dat_client_Match_Model dat_client_Match_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param dat_client_GER_Model_Date dat_client_GER_Model_Date
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#' @param m_months_GER m_months_GER
#'
#' @return dat_client_GER_Model_Date_features_Model
#' @importFrom lubridate duration
#' @import dplyr
#' @export
#'
dd_dat_client_GER_Model_Date_features_Model <-
  function(
    dat_client_Match_Model      = dat_client_Match_Model
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , dat_client_GER_Model_Date   = dat_client_GER_Model_Date
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  , m_months_GER                = m_months_GER
  ) {

    # Join Match, IMB_ANE, and GER
    dat_client_GER_Model_Date_features <-
      dat_client_Match_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      ) |>
      dplyr::left_join(
        dat_client_GER_Model_Date
      , by = join_by(Client_System_ID)
      )

    if (sw_ANE_Current == c("ANE", "Current")[1]) {
      dat_client_GER_Model_Date_features <-
        dat_client_GER_Model_Date_features |>
        dplyr::mutate(
          Last_Date =
            dplyr::case_when(
              !is.na(ANE_Date) ~ ANE_Date
            , TRUE             ~ date_Current
            )
        )
    }
    if (sw_ANE_Current == c("ANE", "Current")[2]) {
      dat_client_GER_Model_Date_features <-
        dat_client_GER_Model_Date_features |>
        dplyr::mutate(
          Last_Date = date_Current
        )
    }

    dat_client_GER_Model_Date_features <-
      dat_client_GER_Model_Date_features |>
      # GER, sum Ger_AtRisk_* features over last m months
      dplyr::filter(
        is.na(GER_Event_Date) |
        GER_Event_Date >= ((Last_Date - lubridate::duration(m_months_GER)) |> as_date())
      , GER_Waiver %in% c("DDSD - DD Waiver")
      ) |>
      dplyr::arrange(
        Client_System_ID
      , dplyr::desc(GER_Date)
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::summarize(
        Client_System_ID                    = Client_System_ID                    |> first()
      #, Client_SSN                          = Client_SSN                          |> first()
      #, Client_TherapID                     = Client_TherapID                     |> first()
      , Client_Gender                       = Client_Gender                       |> first()
      , Client_DOB                          = Client_DOB                          |> first()
      , Client_Ethnicity                    = Client_Ethnicity                    |> first()
      , Client_Race                         = Client_Race                         |> first()
      , Client_Region                       = Client_Region                       |> first()
      , Client_Waiver                       = Client_Waiver                       |> first()
      , ANE_Date                            = ANE_Date                            |> first()
      , ANE_Substantiated                   = ANE_Substantiated                   |> first()
      , GER_Date                            = GER_Date                            |> first()
      , GER_Provider                        = GER_Provider                        |> first()
      , GER_Region                          = GER_Region                          |> first()
      #, GER_Waiver                          = GER_Waiver                          |> first()
      #, GER_Event_Date                      = GER_Event_Date                      |> first()
      #, GER_Event_Type                      = GER_Event_Type                      |> first()
      , GER_Abuse_Suspected                 = sum(GER_Abuse_Suspected                 == "Yes", na.rm = TRUE)
      , GER_Neglect_Suspected               = sum(GER_Neglect_Suspected               == "Yes", na.rm = TRUE)
      , GER_Exploitation_Suspected          = sum(GER_Exploitation_Suspected          == "Yes", na.rm = TRUE)
      , GER_AtRisk_Injury                   = sum(GER_AtRisk_Injury                   , na.rm = TRUE)
      , GER_AtRisk_Med_Error                = sum(GER_AtRisk_Med_Error                , na.rm = TRUE)
      , GER_AtRisk_Emergency_Service        = sum(GER_AtRisk_Emergency_Service        , na.rm = TRUE)
      , GER_AtRisk_Assault                  = sum(GER_AtRisk_Assault                  , na.rm = TRUE)
      , GER_AtRisk_Law_Enforcement          = sum(GER_AtRisk_Law_Enforcement          , na.rm = TRUE)
      , GER_AtRisk_Elopement                = sum(GER_AtRisk_Elopement                , na.rm = TRUE)
      , GER_AtRisk_Fall                     = sum(GER_AtRisk_Fall                     , na.rm = TRUE)
      , GER_AtRisk_Restraint                = sum(GER_AtRisk_Restraint                , na.rm = TRUE)
      , GER_AtRisk_Hospitalization          = sum(GER_AtRisk_Hospitalization          , na.rm = TRUE)
      , GER_AtRisk_Suicide_Related_Behavior = sum(GER_AtRisk_Suicide_Related_Behavior , na.rm = TRUE)
      , GER_AtRisk_PRN_Psych_Use            = sum(GER_AtRisk_PRN_Psych_Use            , na.rm = TRUE)
      , GER_AtRisk_Sum                      = sum(GER_AtRisk_Sum                      , na.rm = TRUE)
      , Last_Date                           = Last_Date                           |> first()
      ) |>
      dplyr::ungroup() |>
      # clean rest of data
      dplyr::mutate(
        Age = round((Last_Date - Client_DOB) / 365.25, 1) |> as.numeric()
      , ANE_Substantiated =
          dplyr::case_when(
            is.na(ANE_Substantiated)  ~ 0
          , TRUE                      ~ ANE_Substantiated
          ) |>
          factor(levels = c(0, 1), labels = c("No", "Yes"))
      ) |>
      dplyr::select(
        Client_System_ID
      ##, Client_SSN
      ##, Client_TherapID
      , Client_Gender
      ##, Client_DOB
      , Client_Ethnicity
      , Client_Race
      , Client_Region
      , Client_Waiver
      ##, ANE_Date
      , ANE_Substantiated
      ##, GER_Date
      ##, GER_Provider
      ##, GER_Region
      ##, GER_Waiver
      ##, GER_Event_Date
      ##, GER_Event_Type
      , GER_Abuse_Suspected
      , GER_Neglect_Suspected
      , GER_Exploitation_Suspected
      , GER_AtRisk_Injury
      , GER_AtRisk_Med_Error
      , GER_AtRisk_Emergency_Service
      , GER_AtRisk_Assault
      , GER_AtRisk_Law_Enforcement
      , GER_AtRisk_Elopement
      , GER_AtRisk_Fall
      , GER_AtRisk_Restraint
      , GER_AtRisk_Hospitalization
      , GER_AtRisk_Suicide_Related_Behavior
      , GER_AtRisk_PRN_Psych_Use
      , GER_AtRisk_Sum
      , Age
      ) |>
      dplyr::relocate(
        ANE_Substantiated
      )

  return(dat_client_GER_Model_Date_features)
}


#' Title
#'
#' @param dat_client_Match_Model dat_client_Match_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param dat_client_RORA_Model_Date dat_client_RORA_Model_Date
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#' @param m_months_RORA m_months_RORA
#'
#' @return dat_client_RORA_Model_Date_features
#' @importFrom lubridate duration
#' @import dplyr
#' @export
#'
dd_dat_client_RORA_Model_Date_features <-
  function(
    dat_client_Match_Model      = dat_client_Match_Model
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , dat_client_RORA_Model_Date  = dat_client_RORA_Model_Date
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  , m_months_RORA               = m_months_RORA
  ) {

    # Join Match, IMB_ANE, and RORA
    dat_client_RORA_Model_Date_features <-
      dat_client_Match_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      ) |>
      dplyr::left_join(
        dat_client_RORA_Model_Date
      , by = join_by(Client_System_ID)
      )

    if (sw_ANE_Current == c("ANE", "Current")[1]) {
      dat_client_RORA_Model_Date_features <-
        dat_client_RORA_Model_Date_features |>
        dplyr::mutate(
          Last_Date =
            dplyr::case_when(
              !is.na(ANE_Date) ~ ANE_Date
            , TRUE             ~ date_Current
            )
        )
    }
    if (sw_ANE_Current == c("ANE", "Current")[2]) {
      dat_client_RORA_Model_Date_features <-
        dat_client_RORA_Model_Date_features |>
        dplyr::mutate(
          Last_Date = date_Current
        )
    }

    dat_client_RORA_Model_Date_features <-
      dat_client_RORA_Model_Date_features |>
      # RORA, sum Ger_AtRisk_* features over last M = 3, 6, and 12 months
      dplyr::filter(
        is.na(C_RORA_RequestDate) |
        C_RORA_RequestDate >= ((Last_Date - lubridate::duration(m_months_RORA)) |> as_date())
      #, RORA_Waiver %in% c("DDSD - DD Waiver")
      ) |>
      dplyr::arrange(
        Client_System_ID
      , dplyr::desc(C_RORA_RequestDate)
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::summarize(
        Client_System_ID                    = Client_System_ID                    |> first()
      #, Client_SSN                          = Client_SSN                          |> first()
      #, Client_TherapID                     = Client_TherapID                     |> first()
      , Client_Gender                       = Client_Gender                       |> first()
      , Client_DOB                          = Client_DOB                          |> first()
      , Client_Ethnicity                    = Client_Ethnicity                    |> first()
      , Client_Race                         = Client_Race                         |> first()
      , Client_Region                       = Client_Region                       |> first()
      , Client_Waiver                       = Client_Waiver                       |> first()
      , ANE_Date                            = ANE_Date                            |> first()
      , ANE_Substantiated                   = ANE_Substantiated                   |> first()
      # New with Model_02
      , C_RORA_RoraType                     = C_RORA_RoraType                     |> first()
      #, C_RORA_Waiver                       = C_RORA_Waiver                       |> first()
      , C_RORA_RequestDate                  = C_RORA_RequestDate                  |> first()
      , C_RORA_Primary_Concern              = sum(!is.na(C_RORA_Primary_Concern     ))
      , C_RORA_Secondary_Concern            = sum(!is.na(C_RORA_Secondary_Concern   ))
      , C_RORA_Care_or_Compliance_1         = sum(C_RORA_Care_or_Compliance           == "Compliance"       , na.rm = TRUE)
      , C_RORA_Care_or_Compliance_2         = sum(C_RORA_Care_or_Compliance           == "Care"             , na.rm = TRUE)
      , C_RORA_Risk_1                       = sum(C_RORA_Risk                         == "Priority 1 (high)", na.rm = TRUE)
      , C_RORA_Risk_2                       = sum(C_RORA_Risk                         == "Priority 2 (low)" , na.rm = TRUE)
      , Last_Date                           = Last_Date                           |> first()
      ) |>
      dplyr::ungroup() |>
      # clean rest of data
      dplyr::mutate(
        Age = round((Last_Date - Client_DOB) / 365.25, 1) |> as.numeric()
      , ANE_Substantiated =
          dplyr::case_when(
            is.na(ANE_Substantiated)  ~ 0
          , TRUE                      ~ ANE_Substantiated
          ) |>
          factor(levels = c(0, 1), labels = c("No", "Yes"))
      ) |>
      dplyr::select(
        Client_System_ID
      ##, Client_SSN
      ##, Client_TherapID
      , Client_Gender
      ##, Client_DOB
      , Client_Ethnicity
      , Client_Race
      , Client_Region
      , Client_Waiver
      ##, ANE_Date
      , ANE_Substantiated
      ## New with Model_02
      #, C_RORA_RoraType
      #, C_RORA_Waiver
      #, C_RORA_RequestDate
      , C_RORA_Primary_Concern
      , C_RORA_Secondary_Concern
      , C_RORA_Care_or_Compliance_1
      , C_RORA_Care_or_Compliance_2
      , C_RORA_Risk_1
      , C_RORA_Risk_2
      , Age
      ) |>
      dplyr::relocate(
        ANE_Substantiated
      )

  return(dat_client_RORA_Model_Date_features)
}


#' Title
#'
#' @param dat_client_Match_Model dat_client_Match_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param dat_client_BBS_Model dat_client_BBS_Model
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#'
#' @return dat_client_BBS_Model_Date_features
#' @import dplyr
#' @export
#'
dd_dat_client_BBS_Model_Date_features <-
  function(
    dat_client_Match_Model      = dat_client_Match_Model
  , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
  , dat_client_BBS_Model        = dat_client_BBS_Model
  , date_Current                = date_Current
  , sw_ANE_Current              = c("ANE", "Current")[1]
  ) {

    # Join Match, IMB_ANE, and BBS
    dat_client_BBS_Model_Date_features <-
      dat_client_Match_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      ) |>
      dplyr::left_join(
        dat_client_BBS_Model
      , by = join_by(Client_System_ID)
      )

    if (sw_ANE_Current == c("ANE", "Current")[1]) {
      dat_client_BBS_Model_Date_features <-
        dat_client_BBS_Model_Date_features |>
        dplyr::mutate(
          Last_Date =
            dplyr::case_when(
              !is.na(ANE_Date) ~ ANE_Date
            , TRUE             ~ date_Current
            )
        )
    }
    if (sw_ANE_Current == c("ANE", "Current")[2]) {
      dat_client_BBS_Model_Date_features <-
        dat_client_BBS_Model_Date_features |>
        dplyr::mutate(
          Last_Date = date_Current
        )
    }

    dat_client_BBS_Model_Date_features <-
      dat_client_BBS_Model_Date_features |>
      # clean rest of data
      dplyr::mutate(
        Age = round((Last_Date - Client_DOB) / 365.25, 1) |> as.numeric()
      , ANE_Substantiated =
          dplyr::case_when(
            is.na(ANE_Substantiated)  ~ 0
          , TRUE                      ~ ANE_Substantiated
          ) |>
          factor(levels = c(0, 1), labels = c("No", "Yes"))
      ) |>
      dplyr::select(
        Client_System_ID
      ##, Client_SSN
      ##, Client_TherapID
      , Client_Gender
      ##, Client_DOB
      , Client_Ethnicity
      , Client_Race
      , Client_Region
      , Client_Waiver
      ##, ANE_Date
      , ANE_Substantiated
      , BBS_AtRisk
      , Age
      ) |>
      dplyr::relocate(
        ANE_Substantiated
      )

  return(dat_client_BBS_Model_Date_features)
}


#' Title
#'
#' @param dat_client_Match_Model dat_client_Match_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param dat_client_Syncronys_Model_Date dat_client_Syncronys_Model_Date
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#' @param m_months_Syncronys m_months_Syncronys
#'
#' @return dat_client_Syncronys_Model_Date_features
#' @importFrom lubridate duration
#' @import dplyr
#' @export
#'
dd_dat_client_Syncronys_Model_Date_features <-
  function(
    dat_client_Match_Model          = dat_client_Match_Model
  , dat_client_IMB_ANE_Model        = dat_client_IMB_ANE_Model
  , dat_client_Syncronys_Model_Date = dat_client_Syncronys_Model_Date
  , date_Current                    = date_Current
  , sw_ANE_Current                  = c("ANE", "Current")[1]
  , m_months_Syncronys              = m_months_Syncronys
  ) {

    dat_client_Syncronys_Model_Date_features <-
      dat_client_Match_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      ) |>
      dplyr::left_join(
        dat_client_Syncronys_Model_Date
      , by = join_by(Client_System_ID)
      )

    if (sw_ANE_Current == c("ANE", "Current")[1]) {
      dat_client_Syncronys_Model_Date_features <-
        dat_client_Syncronys_Model_Date_features |>
        dplyr::mutate(
          Last_Date =
            dplyr::case_when(
              !is.na(ANE_Date) ~ ANE_Date
            , TRUE             ~ date_Current
            )
        )
    }
    if (sw_ANE_Current == c("ANE", "Current")[2]) {
      dat_client_Syncronys_Model_Date_features <-
        dat_client_Syncronys_Model_Date_features |>
        dplyr::mutate(
          Last_Date = date_Current
        )
    }

    dat_client_Syncronys_Model_Date_features <-
      dat_client_Syncronys_Model_Date_features |>
      # Syncronys, sum Syncronys_* features over last m months
      dplyr::filter(
        is.na(Syncronys_AdmitDate) |
        Syncronys_AdmitDate >= ((Last_Date - lubridate::duration(m_months_Syncronys)) |> as_date())
      #, Syncronys_Waiver %in% c("DDSD - DD Waiver")
      ) |>
      dplyr::arrange(
        Client_System_ID
      , dplyr::desc(Syncronys_AdmitDate)
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::summarize(
        Client_System_ID    = Client_System_ID    |> first()
      #, Client_SSN          = Client_SSN          |> first()
      #, Client_TherapID     = Client_TherapID     |> first()
      , Client_Gender       = Client_Gender       |> first()
      , Client_DOB          = Client_DOB          |> first()
      , Client_Ethnicity    = Client_Ethnicity    |> first()
      , Client_Race         = Client_Race         |> first()
      , Client_Region       = Client_Region       |> first()
      , Client_Waiver       = Client_Waiver       |> first()
      , ANE_Date            = ANE_Date            |> first()
      , ANE_Substantiated   = ANE_Substantiated   |> first()
      #, Date                = Date                |> first()
      #, Syncronys_CarrierID = Syncronys_CarrierID |> first()
      #, Syncronys_AdmitDate = Syncronys_AdmitDate |> first()
      , Syncronys_Type_E    = sum(Syncronys_Type  == "E", na.rm = TRUE)
      , Syncronys_Type_I    = sum(Syncronys_Type  == "I", na.rm = TRUE)
      , Last_Date           = Last_Date           |> first()
      ) |>
      dplyr::ungroup() |>
      # clean rest of data
      dplyr::mutate(
        Age = round((Last_Date - Client_DOB) / 365.25, 1) |> as.numeric()
      , ANE_Substantiated =
          dplyr::case_when(
            is.na(ANE_Substantiated)  ~ 0
          , TRUE                      ~ ANE_Substantiated
          ) |>
          factor(levels = c(0, 1), labels = c("No", "Yes"))
      ) |>
      dplyr::select(
        Client_System_ID
      #, Client_SSN
      #, Client_TherapID
      , Client_Gender
      #, Client_DOB
      , Client_Ethnicity
      , Client_Race
      , Client_Region
      , Client_Waiver
      #, ANE_Date
      , ANE_Substantiated
      , Syncronys_Type_E
      , Syncronys_Type_I
      , Age
      #, Last_Date
      ) |>
      dplyr::relocate(
        ANE_Substantiated
      )

  return(dat_client_Syncronys_Model_Date_features)
}


#' Title
#'
#' @param dat_client_Match_Model dat_client_Match_Model
#' @param dat_client_IMB_ANE_Model dat_client_IMB_ANE_Model
#' @param dat_client_Conduent_Omnicad_Model_Date dat_client_Conduent_Omnicad_Model_Date
#' @param date_Current date_Current
#' @param sw_ANE_Current sw_ANE_Current
#' @param m_months_Conduent_Omnicad m_months_Conduent_Omnicad
#'
#' @return dat_client_Conduent_Omnicad_Model_Date_features
#' @importFrom stats var
#' @importFrom moments skewness
#' @importFrom lubridate duration
#' @import dplyr
#' @export
#'
dd_dat_client_Conduent_Omnicad_Model_Date_features <-
  function(
    dat_client_Match_Model                  = dat_client_Match_Model
  , dat_client_IMB_ANE_Model                = dat_client_IMB_ANE_Model
  , dat_client_Conduent_Omnicad_Model_Date  = dat_client_Conduent_Omnicad_Model_Date
  , date_Current                            = date_Current
  , sw_ANE_Current                          = c("ANE", "Current")[1]
  , m_months_Conduent_Omnicad               = m_months_Conduent_Omnicad
  ) {

    dat_client_Conduent_Omnicad_Model_Date_features <-
      dat_client_Match_Model |>
      dplyr::left_join(
        dat_client_IMB_ANE_Model
      , by = join_by(Client_System_ID)
      ) |>
      dplyr::left_join(
        dat_client_Conduent_Omnicad_Model_Date
      , by = join_by(Client_System_ID)
      )

    if (sw_ANE_Current == c("ANE", "Current")[1]) {
      dat_client_Conduent_Omnicad_Model_Date_features <-
        dat_client_Conduent_Omnicad_Model_Date_features |>
        dplyr::mutate(
          Last_Date =
            dplyr::case_when(
              !is.na(ANE_Date) ~ ANE_Date
            , TRUE             ~ date_Current
            )
        )
    }
    if (sw_ANE_Current == c("ANE", "Current")[2]) {
      dat_client_Conduent_Omnicad_Model_Date_features <-
        dat_client_Conduent_Omnicad_Model_Date_features |>
        dplyr::mutate(
          Last_Date = date_Current
        )
    }

    dat_client_Conduent_Omnicad_Model_Date_features <-
      dat_client_Conduent_Omnicad_Model_Date_features |>
      # Conduent_Omnicad, sum Omnicad_* features over m months
      dplyr::filter(
        is.na(Line_Svc_Date_First) |
        Line_Svc_Date_First >= ((Last_Date - lubridate::duration(m_months_Conduent_Omnicad)) |> as_date())
      #, Conduent_Omnicad_Waiver %in% c("DDSD - DD Waiver")
      ) |>
      dplyr::arrange(
        Client_System_ID
      , dplyr::desc(Line_Svc_Date_First)
      ) |>
      dplyr::group_by(
        Client_System_ID
      ) |>
      dplyr::summarize(
        Client_System_ID    = Client_System_ID    |> first()
      #, Client_SSN          = Client_SSN          |> first()
      #, Client_TherapID     = Client_TherapID     |> first()
      , Client_Gender       = Client_Gender       |> first()
      , Client_DOB          = Client_DOB          |> first()
      , Client_Ethnicity    = Client_Ethnicity    |> first()
      , Client_Race         = Client_Race         |> first()
      , Client_Region       = Client_Region       |> first()
      , Client_Waiver       = Client_Waiver       |> first()
      , ANE_Date            = ANE_Date            |> first()
      , ANE_Substantiated   = ANE_Substantiated   |> first()
      #, Line_Svc_Date_First = Line_Svc_Date_First |> first()
      , Conduent_Omnicad_Behavioral_Support_Services = Conduent_Omnicad_Behavioral_Support_Services |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Respite_Living_Supports     = Conduent_Omnicad_Respite_Living_Supports     |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Nursing                     = Conduent_Omnicad_Nursing                     |> sum(na.rm = TRUE)
      , Conduent_Omnicad_BSS_RLS_N_ALL               = Conduent_Omnicad_BSS_RLS_N_ALL               |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Line_Billed_Amt_Sum         = Conduent_Omnicad_Line_Billed_Amt             |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Line_Pd_Amt_Sum             = Conduent_Omnicad_Line_Pd_Amt                 |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Diff_Amt_Sum                = Conduent_Omnicad_Diff_Amt                    |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Diff_Amt_Sum_Abs            = Conduent_Omnicad_Diff_Amt                    |> abs() |> sum(na.rm = TRUE)
      , Conduent_Omnicad_Line_Billed_Amt_Var         = Conduent_Omnicad_Line_Billed_Amt             |> var(na.rm = TRUE)
      , Conduent_Omnicad_Line_Pd_Amt_Var             = Conduent_Omnicad_Line_Pd_Amt                 |> var(na.rm = TRUE)
      , Conduent_Omnicad_Diff_Amt_Var                = Conduent_Omnicad_Diff_Amt                    |> var(na.rm = TRUE)
      , Conduent_Omnicad_Line_Billed_Amt_Skew        = Conduent_Omnicad_Line_Billed_Amt             |> moments::skewness(na.rm = TRUE)
      , Conduent_Omnicad_Line_Pd_Amt_Skew            = Conduent_Omnicad_Line_Pd_Amt                 |> moments::skewness(na.rm = TRUE)
      , Conduent_Omnicad_Diff_Amt_Skew               = Conduent_Omnicad_Diff_Amt                    |> moments::skewness(na.rm = TRUE)
      , Last_Date           = Last_Date           |> first()
      ) |>
      dplyr::ungroup() |>
      # clean rest of data
      dplyr::mutate(
        Age = round((Last_Date - Client_DOB) / 365.25, 1) |> as.numeric()
      , ANE_Substantiated =
          dplyr::case_when(
            is.na(ANE_Substantiated)  ~ 0
          , TRUE                      ~ ANE_Substantiated
          ) |>
          factor(levels = c(0, 1), labels = c("No", "Yes"))
      , Conduent_Omnicad_Line_Billed_Amt_Var         = ifelse(is.nan(Conduent_Omnicad_Line_Billed_Amt_Var   ), 0, Conduent_Omnicad_Line_Billed_Amt_Var   )
      , Conduent_Omnicad_Line_Pd_Amt_Var             = ifelse(is.nan(Conduent_Omnicad_Line_Pd_Amt_Var       ), 0, Conduent_Omnicad_Line_Pd_Amt_Var       )
      , Conduent_Omnicad_Diff_Amt_Var                = ifelse(is.nan(Conduent_Omnicad_Diff_Amt_Var          ), 0, Conduent_Omnicad_Diff_Amt_Var          )
      , Conduent_Omnicad_Line_Billed_Amt_Skew        = ifelse(is.nan(Conduent_Omnicad_Line_Billed_Amt_Skew  ), 0, Conduent_Omnicad_Line_Billed_Amt_Skew  )
      , Conduent_Omnicad_Line_Pd_Amt_Skew            = ifelse(is.nan(Conduent_Omnicad_Line_Pd_Amt_Skew      ), 0, Conduent_Omnicad_Line_Pd_Amt_Skew      )
      , Conduent_Omnicad_Diff_Amt_Skew               = ifelse(is.nan(Conduent_Omnicad_Diff_Amt_Skew         ), 0, Conduent_Omnicad_Diff_Amt_Skew         )
      ) |>
      dplyr::select(
        Client_System_ID
      #, Client_SSN
      #, Client_TherapID
      , Client_Gender
      #, Client_DOB
      , Client_Ethnicity
      , Client_Race
      , Client_Region
      , Client_Waiver
      #, ANE_Date
      , ANE_Substantiated
      , Conduent_Omnicad_Behavioral_Support_Services
      , Conduent_Omnicad_Respite_Living_Supports
      , Conduent_Omnicad_Nursing
      , Conduent_Omnicad_BSS_RLS_N_ALL
      , Conduent_Omnicad_Line_Billed_Amt_Sum
      , Conduent_Omnicad_Line_Pd_Amt_Sum
      , Conduent_Omnicad_Diff_Amt_Sum
      , Conduent_Omnicad_Diff_Amt_Sum_Abs
      , Conduent_Omnicad_Line_Billed_Amt_Var
      , Conduent_Omnicad_Line_Pd_Amt_Var
      , Conduent_Omnicad_Diff_Amt_Var
      , Conduent_Omnicad_Line_Billed_Amt_Skew
      , Conduent_Omnicad_Line_Pd_Amt_Skew
      , Conduent_Omnicad_Diff_Amt_Skew
      , Age
      #, Last_Date
      ) |>
      dplyr::relocate(
        ANE_Substantiated
      )

  return(dat_client_Conduent_Omnicad_Model_Date_features)
}



#' Title
#'
#' @param sw_ANE_Current sw_ANE_Current
#' @param dat_client_Match dat_client_Match
#' @param dat_client_IMB_ANE dat_client_IMB_ANE
#' @param dat_client_GER dat_client_GER
#' @param dat_client_Syncronys dat_client_Syncronys
#' @param dat_client_RORA dat_client_RORA
#' @param dat_client_Conduent_Omnicad dat_client_Conduent_Omnicad
#' @param dat_client_BBS dat_client_BBS
#' @param date_Current date_Current
#' @param m_months_GER m_months_GER
#' @param m_months_Syncronys m_months_Syncronys
#' @param m_months_Conduent_Omnicad m_months_Conduent_Omnicad
#' @param m_months_RORA m_months_RORA
#'
#' @return list_dat_each_Model_Date_features
#' @export
#'
dd_list_dat_each_Model_Date_features <-
  function(
      sw_ANE_Current                = c("ANE", "Current")[1]      # ANE to train, Current to predict
    , dat_client_Match              = dat_client_Match
    , dat_client_IMB_ANE            = dat_client_IMB_ANE
    , dat_client_GER                = dat_client_GER
    , dat_client_Syncronys          = dat_client_Syncronys
    , dat_client_RORA               = dat_client_RORA
    , dat_client_Conduent_Omnicad   = dat_client_Conduent_Omnicad
    , dat_client_BBS                = dat_client_BBS
    , date_Current                  = date_Current
    , m_months_GER                  = m_months_GER
    , m_months_Syncronys            = m_months_Syncronys
    , m_months_Conduent_Omnicad     = m_months_Conduent_Omnicad
    , m_months_RORA                 = m_months_RORA
  ) {

  dat_client_Match_Model <-
    dd_dat_client_Match_Model(
      dat_client_Match = dat_client_Match
    )

  dat_client_IMB_ANE_Model <-
    dd_dat_client_IMB_ANE_Model(
      dat_client_IMB_ANE = dat_client_IMB_ANE
    )

  dat_client_GER_Model <-
    dd_dat_client_GER_Model(
      dat_client_GER = dat_client_GER
    )

  dat_client_Syncronys_Model <-
    dd_dat_client_Syncronys_Model(
      dat_client_Syncronys = dat_client_Syncronys
    )

  dat_client_RORA_Model <-
    dd_dat_client_RORA_Model(
      dat_client_RORA = dat_client_RORA
    )

  dat_client_BBS_Model <-
    dd_dat_client_BBS_Model(
      dat_client_BBS = dat_client_BBS
    )

  # Subset by date
  dat_client_GER_Model_Date <-
    dd_dat_client_GER_Model_Date(
      dat_client_GER_Model      = dat_client_GER_Model
    , dat_client_IMB_ANE_Model  = dat_client_IMB_ANE_Model
    , date_Current              = date_Current
    , sw_ANE_Current            = sw_ANE_Current
    )

  dat_client_RORA_Model_Date <-
    dd_dat_client_RORA_Model_Date(
      dat_client_RORA_Model       = dat_client_RORA_Model
    , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
    , date_Current                = date_Current
    , sw_ANE_Current              = sw_ANE_Current
    )

  dat_client_Syncronys_Model_Date <-
    dd_dat_client_Syncronys_Model_Date(
      dat_client_Syncronys      = dat_client_Syncronys
    , dat_client_IMB_ANE_Model  = dat_client_IMB_ANE_Model
    , date_Current              = date_Current
    , sw_ANE_Current            = sw_ANE_Current
    )

  dat_client_Conduent_Omnicad_Model_Date <-
    dd_dat_client_Conduent_Omnicad_Model_Date(
      dat_client_Conduent_Omnicad = dat_client_Conduent_Omnicad
    , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
    , date_Current                = date_Current
    , sw_ANE_Current              = sw_ANE_Current
    )

  # Features
  dat_client_GER_Model_Date_features <-
    dd_dat_client_GER_Model_Date_features_Model(
      dat_client_Match_Model      = dat_client_Match_Model
    , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
    , dat_client_GER_Model_Date   = dat_client_GER_Model_Date
    , date_Current                = date_Current
    , sw_ANE_Current              = sw_ANE_Current
    , m_months_GER                = m_months_GER
    )

  dat_client_RORA_Model_Date_features <-
    dd_dat_client_RORA_Model_Date_features(
      dat_client_Match_Model      = dat_client_Match_Model
    , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
    , dat_client_RORA_Model_Date  = dat_client_RORA_Model_Date
    , date_Current                = date_Current
    , sw_ANE_Current              = sw_ANE_Current
    , m_months_RORA               = m_months_RORA
    )

  dat_client_Syncronys_Model_Date_features <-
    dd_dat_client_Syncronys_Model_Date_features(
      dat_client_Match_Model          = dat_client_Match_Model
    , dat_client_IMB_ANE_Model        = dat_client_IMB_ANE_Model
    , dat_client_Syncronys_Model_Date = dat_client_Syncronys_Model_Date
    , date_Current                    = date_Current
    , sw_ANE_Current                  = sw_ANE_Current
    , m_months_Syncronys              = m_months_Syncronys
    )

  dat_client_Conduent_Omnicad_Model_Date_features <-
    dd_dat_client_Conduent_Omnicad_Model_Date_features(
      dat_client_Match_Model                  = dat_client_Match_Model
    , dat_client_IMB_ANE_Model                = dat_client_IMB_ANE_Model
    , dat_client_Conduent_Omnicad_Model_Date  = dat_client_Conduent_Omnicad_Model_Date
    , date_Current                            = date_Current
    , sw_ANE_Current                          = sw_ANE_Current
    , m_months_Conduent_Omnicad               = m_months_Conduent_Omnicad
    )

  dat_client_BBS_Model_Date_features <-
    dd_dat_client_BBS_Model_Date_features(
      dat_client_Match_Model      = dat_client_Match_Model
    , dat_client_IMB_ANE_Model    = dat_client_IMB_ANE_Model
    , dat_client_BBS_Model        = dat_client_BBS_Model
    , date_Current                = date_Current
    , sw_ANE_Current              = sw_ANE_Current
    )


  list_dat_each_Model_Date_features <-
    list(
      GER               = dat_client_GER_Model_Date_features
    , RORA              = dat_client_RORA_Model_Date_features
    , Syncronys         = dat_client_Syncronys_Model_Date_features
    , Conduent_Omnicad  = dat_client_Conduent_Omnicad_Model_Date_features
    , BBS               = dat_client_BBS_Model_Date_features
    )

  return(list_dat_each_Model_Date_features)

}



#' Title
#'
#' @param list_dat_features list_dat_features
#' @param by_for_join by_for_join
#'
#' @return dat_all_Model_ID
#' @import dplyr
#' @export
#'
dd_dat_all_Model_ID <-
  function(
    list_dat_features =
      list(
        dat_client_GER_Model_Date_features
      , dat_client_Syncronys_Model_Date_features
      , dat_client_Conduent_Omnicad_Model_Date_features
      , dat_client_RORA_Model_Date_features
      , dat_client_BBS_Model_Date_features
      )
  , by_for_join = dplyr::join_by(ANE_Substantiated, Client_System_ID, Client_Gender, Client_Ethnicity, Client_Race, Client_Region, Age)
  ) {

  for (i_list in seq_len(length(list_dat_features))) {
    ## i_list = 1

    if (i_list == 1) {
      dat_all_Model_ID <-
        list_dat_features[[ i_list ]]
    } else {
      dat_all_Model_ID <-
        dat_all_Model_ID |>
        dplyr::full_join(
          list_dat_features[[ i_list ]]
        , by = by_for_join
        )
    }
  }

  dat_all_Model_ID <-
    dat_all_Model_ID |>
    # for non-overlapping Client_System_ID, fill 0s for numeric columns
    dplyr::mutate(
      dplyr::across(
        where(
          is.numeric
        )
      , ~ tidyr::replace_na(.x, replace = 0)
      )
    #  Syncronys_Type_E    = Syncronys_Type_E |> tidyr::replace_na(replace = 0)
    #, Syncronys_Type_I    = Syncronys_Type_I |> tidyr::replace_na(replace = 0)
    )

  return(dat_all_Model_ID)
}

