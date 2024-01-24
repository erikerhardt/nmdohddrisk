#' Features for Client Conduent Omnicad during read
#'
#' @param dat_client_Conduent_Omnicad    dat_client_Conduent_Omnicad data from \code{dd_read_client_Conduent_Omnicad()}
#'
#' @return dat_client_Conduent_Omnicad
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dd_features_client_Conduent_Omnicad_read(
#'     dat_client_Conduent_Omnicad
#'   )
#' }
dd_features_client_Conduent_Omnicad_read <-
  function(
    dat_client_Conduent_Omnicad  = NULL
  ) {
  ## dat_client_Conduent_Omnicad = dplyr::left_join(dat_this_header, dat_this_line, by = dplyr::join_by(TCN))

  dat_client_Conduent_Omnicad <-
    dat_client_Conduent_Omnicad |>
    dplyr::mutate(
    ####                                             # Behavioral Support Services
      Conduent_Omnicad_H2019                =        #   Behavior support consultation services              H2019
        ifelse(
          Proc_Cd             == "H2019"
        , 1, 0)
    , Conduent_Omnicad_T1023                =        #   Preliminary Risk Screening and Consultation (PSRC)  T1023
        ifelse(
          Proc_Cd             == "T1023"
        , 1, 0)
    , Conduent_Omnicad_T2034_HB             =        #   Crisis supports (not in Orange, but does exist)     T2034_HB
        ifelse(
          Proc_Cd             == "T2034" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T2011_HB             =        #   Crisis supports (not in Orange, but does exist)     T2011_HB
        ifelse(
          Proc_Cd             == "T2011" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T2011                =        #   Crisis supports (not in Orange, but does exist)     T2011
        ifelse(
          Proc_Cd             == "T2011"
        , 1, 0)
    , Conduent_Omnicad_S9446                =        #   Socialization and Sexuality concern                 S9446
        ifelse(
          Proc_Cd             == "S9446"
        , 1, 0)

    ####                                             # Respite and Living Supports
    , Conduent_Omnicad_T1005_HB_HQ          =        #   Respite Grp                                         T1005_HB_HQ
        ifelse(
          Proc_Cd             == "T1005" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "HQ"
        , 1, 0)
    , Conduent_Omnicad_T1005_HB             =        #   Respite, Stand                                      T1005_HB
        ifelse(
          Proc_Cd             == "T1005" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_S5125_HB_UA          =        #   Cust In-Home Sprt, Liv Indep                        S5125_HB_UA
        ifelse(
          Proc_Cd             == "S5125" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "UA"
        , 1, 0)
    , Conduent_Omnicad_S5125_HB             =        #   Cust In-Home Sprts Liv w Natrl Sprts                S5125_HB
        ifelse(
          Proc_Cd             == "S5125" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T2033_HB             =        #   Family Liv Adult                                    T2033_HB
        ifelse(
          Proc_Cd             == "T2033" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T2038_HB             =        #   Indep Liv Transition                                T2038_HB
        ifelse(
          Proc_Cd             == "T2038" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T2033_HB_TG          =        #   Ints Med Res                                        T2033_HB_TG
        ifelse(
          Proc_Cd             == "T2033" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "TG"
        , 1, 0)
    , Conduent_Omnicad_T2033_HB_U7          =        #   Res Care                                            T2033_HB_U7
        ifelse(
          Proc_Cd             == "T2033" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "U7"
        , 1, 0)
    , Conduent_Omnicad_T2016_HB_U7          =        #   Res Wvr-Sprtd Lvg JCM Outlier Day                   T2016_HB_U7
        ifelse(
          Proc_Cd             == "T2016" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "U7"
        , 1, 0)
    , Conduent_Omnicad_T2016_HB_U4          =        #   Sprtd Liv Basic Sprt                                T2016_HB_U4
        ifelse(
          Proc_Cd             == "T2016" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "U4"
        , 1, 0)
    , Conduent_Omnicad_T2016_HB_U6          =        #   Sprtd Liv Extens Sprt                               T2016_HB_U6
        ifelse(
          Proc_Cd             == "T2016" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "U6"
        , 1, 0)
    , Conduent_Omnicad_T2016_HB_U5          =        #   Sprtd Liv Mod Sprt                                  T2016_HB_U5
        ifelse(
          Proc_Cd             == "T2016" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "U5"
        , 1, 0)
    , Conduent_Omnicad_H2022_HB_TG          =        #   Sprtd Liv Non Ambul                                 H2022_HB_TG
        ifelse(
          Proc_Cd             == "H2022" &
          Line_Proc_Code_Mod1 == "HB"    &
          Line_Proc_Code_Mod2 == "TG"
        , 1, 0)
    , Conduent_Omnicad_T2033                =        #   Res Sprt Family Liv MV                              T2033
        ifelse(
          Proc_Cd             == "T2033"
        , 1, 0)

    ####                                             # Nursing
    , Conduent_Omnicad_T1003_HB             =        #   LPN/LVN Srvcs                                       T1003_HB
        ifelse(
          Proc_Cd             == "T1003" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T1002_HB             =        #   RN Srvcs                                            T1002_HB
        ifelse(
          Proc_Cd             == "T1002" &
          Line_Proc_Code_Mod1 == "HB"
        , 1, 0)
    , Conduent_Omnicad_T1003                =        #   LPN/LVN SrvcsMVMF                                   T1003
        ifelse(
          Proc_Cd             == "T1003"
        , 1, 0)
    , Conduent_Omnicad_T1002                =        #   RN SrvcsMFMV                                        T1002
        ifelse(
          Proc_Cd             == "T1002"
        , 1, 0)
    , Conduent_Omnicad_S9122_U1             =        #   Home Health Aid RespiteMF                           S9122_U1
        ifelse(
          Proc_Cd             == "S9122" &
          Line_Proc_Code_Mod1 == "U1"
        , 1, 0)
    , Conduent_Omnicad_T1003_U1             =        #   LPN/LVN Respite Srvcs MF                            T1003_U1
        ifelse(
          Proc_Cd             == "T1003" &
          Line_Proc_Code_Mod1 == "U1"
        , 1, 0)
    , Conduent_Omnicad_T1002_U1             =        #   RN Respite Srvcs MF                                 T1002_U1
        ifelse(
          Proc_Cd             == "T1002" &
          Line_Proc_Code_Mod1 == "U1"
        , 1, 0)
    , Conduent_Omnicad_T1005                =        #   Respite sub care (aide/LPN/RN)MV                    T1005
        ifelse(
          Proc_Cd             == "T1005"
        , 1, 0)
    ) |>
    # for each row, sum the line items into groups
    dplyr::rowwise() |>
    dplyr::mutate(
    ####                                             # COMBINED
      Conduent_Omnicad_Behavioral_Support_Services =
        sum(
          c(
            Conduent_Omnicad_H2019
          , Conduent_Omnicad_T1023
          , Conduent_Omnicad_T2034_HB
          , Conduent_Omnicad_T2011_HB
          , Conduent_Omnicad_T2011
          , Conduent_Omnicad_S9446
          )
        , na.rm = TRUE
        )
    , Conduent_Omnicad_Respite_Living_Supports =
        sum(
          c(
            Conduent_Omnicad_T1005_HB_HQ
          , Conduent_Omnicad_T1005_HB
          , Conduent_Omnicad_S5125_HB_UA
          , Conduent_Omnicad_S5125_HB
          , Conduent_Omnicad_T2033_HB
          , Conduent_Omnicad_T2038_HB
          , Conduent_Omnicad_T2033_HB_TG
          , Conduent_Omnicad_T2033_HB_U7
          , Conduent_Omnicad_T2016_HB_U7
          , Conduent_Omnicad_T2016_HB_U4
          , Conduent_Omnicad_T2016_HB_U6
          , Conduent_Omnicad_T2016_HB_U5
          , Conduent_Omnicad_H2022_HB_TG
          , Conduent_Omnicad_T2033
          )
        , na.rm = TRUE
        )
    , Conduent_Omnicad_Nursing =
        sum(
          c(
            Conduent_Omnicad_T1003_HB
          , Conduent_Omnicad_T1002_HB
          , Conduent_Omnicad_T1003
          , Conduent_Omnicad_T1002
          , Conduent_Omnicad_S9122_U1
          , Conduent_Omnicad_T1003_U1
          , Conduent_Omnicad_T1002_U1
          , Conduent_Omnicad_T1005
          )
        , na.rm = TRUE
        )
    , Conduent_Omnicad_BSS_RLS_N_ALL =
        sum(
          c(
            Conduent_Omnicad_Behavioral_Support_Services
          , Conduent_Omnicad_Respite_Living_Supports
          , Conduent_Omnicad_Nursing
          )
        , na.rm = TRUE
        )
    ) |>
    dplyr::ungroup() |>  # ungroup rows
    dplyr::select(
      Client_System_ID
    , Line_Svc_Date_First
    , TCN
    , Client_COE_Cd
    #, Proc_Cd
    #, Line_Allowed_Units
    #, Line_Pd_Units
    #, Line_Proc_Code_Mod1
    #, Line_Proc_Code_Mod2
    #, Prov_ID
    , Conduent_Omnicad_Line_Billed_Amt
    , Conduent_Omnicad_Line_Pd_Amt
    , Conduent_Omnicad_Diff_Amt
    #, Conduent_Omnicad_H2019
    #, Conduent_Omnicad_T1023
    #, Conduent_Omnicad_T2034_HB
    #, Conduent_Omnicad_T2011_HB
    #, Conduent_Omnicad_T2011
    #, Conduent_Omnicad_S9446
    , Conduent_Omnicad_Behavioral_Support_Services
    #, Conduent_Omnicad_T1005_HB_HQ
    #, Conduent_Omnicad_T1005_HB
    #, Conduent_Omnicad_S5125_HB_UA
    #, Conduent_Omnicad_S5125_HB
    #, Conduent_Omnicad_T2033_HB
    #, Conduent_Omnicad_T2038_HB
    #, Conduent_Omnicad_T2033_HB_TG
    #, Conduent_Omnicad_T2033_HB_U7
    #, Conduent_Omnicad_T2016_HB_U7
    #, Conduent_Omnicad_T2016_HB_U4
    #, Conduent_Omnicad_T2016_HB_U6
    #, Conduent_Omnicad_T2016_HB_U5
    #, Conduent_Omnicad_H2022_HB_TG
    #, Conduent_Omnicad_T2033
    , Conduent_Omnicad_Respite_Living_Supports
    #, Conduent_Omnicad_T1003_HB
    #, Conduent_Omnicad_T1002_HB
    #, Conduent_Omnicad_T1003
    #, Conduent_Omnicad_T1002
    #, Conduent_Omnicad_S9122_U1
    #, Conduent_Omnicad_T1003_U1
    #, Conduent_Omnicad_T1002_U1
    #, Conduent_Omnicad_T1005
    , Conduent_Omnicad_Nursing
    , Conduent_Omnicad_BSS_RLS_N_ALL
    ) |>
    dplyr::distinct() |>
    # for each ID and date, sum the groups
    dplyr::group_by(
      Client_System_ID
    , Client_COE_Cd
    , Line_Svc_Date_First
    #, Prov_ID
    ) |>
    dplyr::summarize(
      Conduent_Omnicad_Behavioral_Support_Services = Conduent_Omnicad_Behavioral_Support_Services |> sum()
    , Conduent_Omnicad_Respite_Living_Supports     = Conduent_Omnicad_Respite_Living_Supports     |> sum()
    , Conduent_Omnicad_Nursing                     = Conduent_Omnicad_Nursing                     |> sum()
    , Conduent_Omnicad_BSS_RLS_N_ALL               = Conduent_Omnicad_BSS_RLS_N_ALL               |> sum()
    , Conduent_Omnicad_Line_Billed_Amt             = Conduent_Omnicad_Line_Billed_Amt             |> sum()
    , Conduent_Omnicad_Line_Pd_Amt                 = Conduent_Omnicad_Line_Pd_Amt                 |> sum()
    , Conduent_Omnicad_Diff_Amt                    = Conduent_Omnicad_Diff_Amt                    |> sum()
    ) |>
    dplyr::ungroup()

  # Match Client_TherapID
  # dat_client_Conduent_Omnicad already uses Client_System_ID

  return(dat_client_Conduent_Omnicad)

} # dd_features_client_Conduent_Omnicad_read
