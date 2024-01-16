#' Read Client IMB ANE
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_IMB_ANE
#' @importFrom readxl read_xlsx
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
#' dd_read_client_IMB_ANE(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_IMB_ANE"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_IMB_ANE <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_IMB_ANE"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_IMB_ANE"

  dat_sheet <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "csv$"
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[2]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    , sw_delim                = c(FALSE, "|")[1]
    , sw_read_package_csv_txt = c("readr", "utils")[1]
    )

  # align column classes
  # which(!(unlist(lapply(dat_sheet[[1]], class)) == unlist(lapply(dat_sheet[[2]], class))))
  for (i_sheet in seq_len(length(dat_sheet))) {
    dat_sheet[[ i_sheet ]] <-
      dat_sheet[[ i_sheet ]] |>
      dplyr::mutate(
        SSN                    = SSN                    |> as.character()
      , PhysicalAbAlleg        = PhysicalAbAlleg        |> as.numeric()
      , SexualAbAlleg          = SexualAbAlleg          |> as.numeric()
      , AbuseAddedByDHI_19     = AbuseAddedByDHI_19     |> as.numeric()
      , NeglectAddedByDHI_20   = NeglectAddedByDHI_20   |> as.numeric()
      , ClosureDate            = ClosureDate            |> lubridate::as_date()
      , DateCPAP               = DateCPAP               |> lubridate::as_date()
      )
  }


  dat_client_IMB_ANE <-
    dat_sheet |>
    dplyr::bind_rows() |>
    #dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "PullDate"
        , "CaseNumber"
        , "SSN"
        , "IncidentDate"
        , "ClosureDate"
        , "PhysicalAbAlleg"
        , "SexualAbAlleg"
        , "VerbalAbAlleg"
        , "EmotAbAlleg"
        , "PhysicalNtAlleg"
        , "MedNtAlleg"
        , "EnvirNtAlleg"
        , "ExpoitationAlleg"
        , "SeriousIncHospAlleg"
        , "SeriousIncIncarAlleg"
        , "SeriousIncEnvirHazAlleg"
        , "DeathUnexp"
        #, "DeathExp"
        , "AbuseAddedByDHI_19"
        , "NeglectAddedByDHI_20"
        , "ExploitationAddedByDHI_21"
        , "IncidentDateTime"
        , "Injury_23"
        , "InjuryAlleg"
        , "PhysicalAbuseAlleg"
        , "DateCPAP"                       # SUBSTANTIATED if date, if 0000-00-00  (NA) then NOT SUBSTANTIATED.
        , "Severity"
        #, "CorrPrevMeasures"
        , "AllegationType"
        #, "Aspiration"
        #, "Falls"
        #, "Staff_Training"
        #, "Staffing"
        #, "Delay_in_Medical_Treatment"
        #, "Medical_Plan"
        #, "ISP_Not_Followed"
        #, "Behavior"
        #, "Elopement"
        #, "Criminal_Hx_Screening_not_completed"
        #, "Human_Rights"
        #, "restraint"
        #, "Medications"
        #, "Sexual_Assault"
        #, "Mealtime_Plan"
        #, "Suicidal_thoughts"
        #, "assault"
        #, "Sepsis"
        #, "Constipation"
        #, "Seizure_Disorder"
        #, "Dehydration"
        #, "Pressure_Ulcers"
        #, "Service_Care_Plans_Not_Followed"
        #, "Supervision"
        #, "Training"
        #, "Medication_Error"
        #, "Domestic_Violence"
        #, "alleged_sexual_assault"
        #, "exploitation"
        #, "reckless_driving"
        #, "environmental_conditions"
        #, "Unauthorized_Restraint"
        #, "Restraint_failed_to_follow_a_Service_Care_Plan"
        #, "Failure_to_seek_alternate_care"
        #, "Verbal_Abuse"
        #, "Not_Following_Service_Standards"
        #, "Self_Neglect"
        , "AbuseAddedByDHI_67"
        , "NeglectAddedByDHI_68"
        , "ExploitationAddedByDHI_69"
        , "Injury_70"
        #, "CaseManagementAgencyName"
        #, "CaseManagementAgencyDEWaiver"
        #, "CaseManagementAgencyDDMedicaidWaiver"
        #, "CaseManagementAgencyMedicallyFragileWaiver"
        #, "ReportingProviderAgencyName"
        #, "ReportingProviderDEWaiver"
        #, "ReportingProviderDDMedicaidWaiver"
        #, "ReportingProviderMedicallyFragileWaiver"
        , "ResponsibleProviderAgencyName"
        #, "ResponsibleProvider_DEWaiver"
        #, "ResponsibleProvider_DDMedicaidWaiver"
        #, "ResponsibleProvider_MedicallyFragileWaiver"
        , "AccusedPersonAgencyName"
        #, "AccusedPersonAgencyDEWaiver"
        #, "AccusedPersonAgencyDDMedicaidWaiver"
        #, "AccusedPersonAgencyMedicallyFragileWaiver"
        )
      )
    ) |>
    dplyr::rename(
      Client_SSN = SSN
    , ANE_IncidentDate                  = IncidentDate
    , ANE_ClosureDate                   = ClosureDate
    , ANE_DateCPAP                      = DateCPAP
    , ANE_Severity                      = Severity
    , ANE_AllegationType                = AllegationType
    , ANE_PullDate                      = PullDate
    , ANE_CaseNumber                    = CaseNumber
    , ANE_PhysicalAbAlleg               = PhysicalAbAlleg
    , ANE_SexualAbAlleg                 = SexualAbAlleg
    , ANE_VerbalAbAlleg                 = VerbalAbAlleg
    , ANE_EmotAbAlleg                   = EmotAbAlleg
    , ANE_PhysicalNtAlleg               = PhysicalNtAlleg
    , ANE_MedNtAlleg                    = MedNtAlleg
    , ANE_EnvirNtAlleg                  = EnvirNtAlleg
    , ANE_ExpoitationAlleg              = ExpoitationAlleg
    , ANE_SeriousIncHospAlleg           = SeriousIncHospAlleg
    , ANE_SeriousIncIncarAlleg          = SeriousIncIncarAlleg
    , ANE_SeriousIncEnvirHazAlleg       = SeriousIncEnvirHazAlleg
    , ANE_DeathUnexp                    = DeathUnexp
    , ANE_AbuseAddedByDHI_19            = AbuseAddedByDHI_19
    , ANE_NeglectAddedByDHI_20          = NeglectAddedByDHI_20
    , ANE_ExploitationAddedByDHI_21     = ExploitationAddedByDHI_21
    , ANE_IncidentDateTime              = IncidentDateTime
    , ANE_Injury_23                     = Injury_23
    , ANE_InjuryAlleg                   = InjuryAlleg
    , ANE_PhysicalAbuseAlleg            = PhysicalAbuseAlleg
    , ANE_AbuseAddedByDHI_67            = AbuseAddedByDHI_67
    , ANE_NeglectAddedByDHI_68          = NeglectAddedByDHI_68
    , ANE_ExploitationAddedByDHI_69     = ExploitationAddedByDHI_69
    , ANE_Injury_70                     = Injury_70
    , ANE_ResponsibleProviderAgencyName = ResponsibleProviderAgencyName
    , ANE_AccusedPersonAgencyName       = AccusedPersonAgencyName
    ) |>
    dplyr::mutate(
      Client_SSN            = Client_SSN          |> as.numeric()
    , ANE_Severity          = ANE_Severity        |> factor()
    , ANE_AllegationType    = ANE_AllegationType  |> factor()
    , ANE_Substantiated     =
        dplyr::case_when(
          !is.na(ANE_DateCPAP) ~ 1
        ,  is.na(ANE_DateCPAP) ~ 0
        )
    , Date                  = ANE_IncidentDate
    , dat_client_IMB_ANE = TRUE
    ) |>
    dplyr::relocate(
      Client_SSN
    , Date
    , ANE_Substantiated
    , ANE_DateCPAP
    , ANE_Severity
    , ANE_AllegationType
    )

  #dat_client_IMB_ANE |> str()


  #name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_IMB_ANE
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_IMB_ANE)

} # dd_read_client_IMB_ANE
