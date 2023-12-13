#' Read Client GER
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_GER
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
#' dd_read_client_GER(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_GER"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_GER <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_GER"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_GER"

  dat_sheet <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "xlsx$"
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[2]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , excel_sheets            = 1
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    , sw_delim                = c(FALSE, "|")[1]
    , sw_read_package_csv_txt = c("readr", "utils")[1]
    )

  #str(dat_sheet)

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
  #   if [Restraint Status]                     <> null                  then "Restraint"                 else
  #   if Text.StartsWith([Other and Sub Event], "OUT OF HOME PLACEMENT") then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALAD")            then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "HOSPITALRE")            then "Hospitalization"           else
  #   if Text.StartsWith([Other and Sub Event], "SUICIDE")               then "Suicide Related Behavior"  else
  #   if Text.StartsWith([Other and Sub Event], "PRN PSYCHOTROPIC USE")  then "PRN Psych Use"             else
  #   if Text.StartsWith([Injury Cause]       , "Fall")                  then "Fall"                      else null),

  dat_client_GER <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "Provider"
        #, "Form_ID"
        #, "GER_Form_Status"
        #, "Program_Site"                 # Justin says "garbage in, "garbage out"
        , "Individual_Last_Name"
        , "Individual_First_Name"
        , "ID_Type"
        , "ID_Number"
        #, "Additional_ID_Type"
        #, "Additional_ID_Number"
        , "Individual_Birth_Date"
        , "Event_Date"
        , "Event_Type"
        #, "Notification_Level"
        , "Abuse_Suspected"
        , "Abuse_Type"
        , "Neglect_Suspected"
        , "Neglect_Type"
        , "Exploitation_Suspected"
        , "Exploitation_Type"
        #, "Internal_Only"
        #, "Total_Reviews"
        #, "Entered_By"
        #, "Reported_By"
        #, "Reported_By_Other"
        #, "Report_Date"
        #, "Entered_Date"
        , "Reporter_Relationship"
        #, "Approve_Date"
        #, "Approved_By"
        #, "Street_1"
        #, "Street2"
        #, "City"
        #, "State"
        #, "Zip"
        #, "Country"
        #, "Phone"
        #, "Fax"
        #, "Injury_Time"
        , "Injury_Type"
        , "Injury_Cause"
        , "Injury_Severity"
        #, "Injury_Summary"
        , "Restraint_Duration"
        , "Restraint_Status"
        #, "Injury_Caused_by_Restraint"
        #, "Monitoring_every_30_minutes"
        #, "Exercise_every_10_mins_hourly"
        #, "Restraint_Bhv_Summary"
        #, "Death_Time"
        , "Death_Cause"
        #, "Death_Comments"
        , "Restraint_Other_Duration"
        , "Restraint_Other_Type"
        #, "Restraint_Other_Summary"
        #, "Med_Err_Discovered_Time"
        , "Med_Err_Type"
        #, "Med_Err_Cause"
        , "Med_Err_Attn_Reqd"
        #, "Med_Err_Reason"
        #, "Other_Event_Time"
        , "Other_Event"
        , "Event_Subtype"
        , "Altercation"
        , "Individual_Involvement"
        #, "Other_Summary"
        #, "What_happened_before_incident"
        #, "Corrective_Action_Taken"
        #, "Plan_of_Future_CA"
        #, "Event_Month"
        #, "Event_Year"
        #, "Event_Day"
        #, "Event_Time"
        )
      )
    ) |>
    dplyr::rename(
      GER_Provider                 = Provider
    , GER_Individual_Last_Name     = Individual_Last_Name
    , GER_Individual_First_Name    = Individual_First_Name
    , GER_ID_Type                  = ID_Type
    , GER_ID_Number                = ID_Number
    , GER_Individual_Birth_Date    = Individual_Birth_Date
    , GER_Event_Date               = Event_Date
    , GER_Event_Type               = Event_Type
    , GER_Abuse_Suspected          = Abuse_Suspected
    , GER_Abuse_Type               = Abuse_Type
    , GER_Neglect_Suspected        = Neglect_Suspected
    , GER_Neglect_Type             = Neglect_Type
    , GER_Exploitation_Suspected   = Exploitation_Suspected
    , GER_Exploitation_Type        = Exploitation_Type
    , GER_Reporter_Relationship    = Reporter_Relationship
    , GER_Injury_Type              = Injury_Type
    , GER_Injury_Cause             = Injury_Cause
    , GER_Injury_Severity          = Injury_Severity
    , GER_Restraint_Duration       = Restraint_Duration
    , GER_Restraint_Status         = Restraint_Status
    , GER_Death_Cause              = Death_Cause
    , GER_Restraint_Other_Duration = Restraint_Other_Duration
    , GER_Restraint_Other_Type     = Restraint_Other_Type
    , GER_Med_Err_Type             = Med_Err_Type
    , GER_Med_Err_Attn_Reqd        = Med_Err_Attn_Reqd
    , GER_Other_Event              = Other_Event
    , GER_Event_Subtype            = Event_Subtype
    , GER_Altercation              = Altercation
    , GER_Individual_Involvement   = Individual_Involvement
    ) |>
    #tidyr::separate_wider_delim(
    #  cols = Program_Site
    #  # Nested parentheses is a problem
    #, delim = "("
    #, names = c("Program", "Site")
    #, too_many = "merge"
    #, cols_remove = FALSE #TRUE
    #) |>
    tidyr::separate_wider_delim(
      cols = GER_ID_Type
    , delim = ", "
    , names = c("GER_State", "GER_Region", "GER_Waiver")
    , too_many = "merge"
    , cols_remove = TRUE
    ) |>
    dplyr::mutate(
    #  Provider                 = Provider                   |> as.character()
    #, Program_Site             = Program_Site             |> as.character()
    #  Site             = Site             |> stringr::str_replace(pattern = "\\)$", replacement = "")
    #, Individual_Last_Name     = Individual_Last_Name     |> as.character()
    #, Individual_First_Name    = Individual_First_Name    |> as.character()
    #, ID_Type                  = ID_Type                  |> as.character()
      GER_Region                  = GER_Region |>
                                forcats::fct_recode(
                                  Metro   = "Metro Region"
                                , NE      = "Northeast Region"
                                , NW      = "Northwest Region"
                                , SE      = "Southeast Region"
                                , SW      = "Southwest Region"
                                , DOH     = "Department of Health"
                                , USA     = "United States"
                                #, Unknown =
                                )
    , GER_Waiver                  = GER_Waiver |> factor()
                                  # "DDSD - DD Waiver"
                                  # "DDSD - Mi Via Waiver"
                                  # "DDSD - NM State General Fund"
                                  # "DDSD, Waiver"
                                  # "Other"
                                  # "Private Pay"
                                  # "Supports Waiver"

    , GER_ID_Number                = GER_ID_Number                |> toupper()
    , GER_Individual_Birth_Date    = GER_Individual_Birth_Date    |> lubridate::dmy()
    , GER_Event_Date               = GER_Event_Date               |> lubridate::mdy()
    , GER_Event_Type               = GER_Event_Type               |> factor()
    , GER_Abuse_Suspected          = GER_Abuse_Suspected          |> factor()
      # XXX maybe collapse categories
    #, GER_Abuse_Type               = GER_Abuse_Type               |> factor()
    , GER_Neglect_Suspected        = GER_Neglect_Suspected        |> factor()
      # XXX maybe collapse categories
    #, GER_Neglect_Type             = GER_Neglect_Type             |> factor()
    , GER_Exploitation_Suspected   = GER_Exploitation_Suspected   |> factor()
      # XXX maybe collapse categories
    #, GER_Exploitation_Type        = GER_Exploitation_Type        |> factor()
    #, GER_Reporter_Relationship    = GER_Reporter_Relationship    |> factor()
    #, GER_Injury_Type              = GER_Injury_Type              |> factor()
    #, GER_Injury_Cause             = GER_Injury_Cause             |> factor()
    , GER_Injury_Severity          = GER_Injury_Severity          |> factor()
    #, GER_Restraint_Duration       = GER_Restraint_Duration       |> as.character()
    #, GER_Restraint_Status         = GER_Restraint_Status         |> as.character()
    #, GER_Death_Cause              = GER_Death_Cause              |> as.character()
    #, GER_Restraint_Other_Duration = GER_Restraint_Other_Duration |> as.character()
    #, GER_Restraint_Other_Type     = GER_Restraint_Other_Type     |> as.character()
    #, GER_Med_Err_Type             = GER_Med_Err_Type             |> as.character()
    , GER_Med_Err_Attn_Reqd        = GER_Med_Err_Attn_Reqd        |> factor()
    #, GER_Med_Err_Reason           = GER_Med_Err_Reason           |> as.character()
    #, GER_Other_Event              = GER_Other_Event              |> as.character()
    #, GER_Event_Subtype            = GER_Event_Subtype            |> as.character()
    , GER_Altercation                = GER_Altercation                |> factor()
    , GER_Individual_Involvement   = GER_Individual_Involvement   |> factor()
    , Date                         = GER_Event_Date
    , dat_client_GER = TRUE
    ) |>
    # some IDs are SSN, need to parse bad characters
    #  dat_client_GER$ID_Number |> str_subset("^[[:digit:]]") |> sort()
    dplyr::mutate(
      Client_TherapID =
        ifelse(
          GER_ID_Number |> stringr::str_detect("^[[:digit:]]", negate = TRUE)
        , GER_ID_Number
        , NA
        )
    , Client_SSN =
        ifelse(
          GER_ID_Number |> stringr::str_detect("^[[:digit:]]")
        , GER_ID_Number
        , NA
        )
    # plan:
    #   remove hyphens.
    #   If longer than 9 digits, then remove the leading digits (usually 96...)
    #   Some are still not SSNs since they start with 000, and SSNs start with 001 or greater.
    , Client_SSN =
        Client_SSN |>
        stringr::str_replace_all(pattern = stringr::fixed("-"), replacement = "") |>
        stringr::str_sub(start = -9) |>
        as.numeric()
    ) |>
    # Replace "N/A" values
    dplyr::mutate(
      GER_Abuse_Type               = GER_Abuse_Type               |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Neglect_Type             = GER_Neglect_Type             |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Exploitation_Type        = GER_Exploitation_Type        |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Injury_Type              = GER_Injury_Type              |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Injury_Cause             = GER_Injury_Cause             |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Restraint_Duration       = GER_Restraint_Duration       |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Death_Cause              = GER_Death_Cause              |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Restraint_Other_Duration = GER_Restraint_Other_Duration |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Restraint_Other_Type     = GER_Restraint_Other_Type     |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Med_Err_Type             = GER_Med_Err_Type             |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Med_Err_Attn_Reqd        = GER_Med_Err_Attn_Reqd        |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    , GER_Altercation              = GER_Altercation              |> stringr::str_replace(pattern = stringr::fixed("N/A"), replacement = NA)
    ) |>
    dplyr::relocate(
      Client_TherapID
    , Client_SSN
    , Date
    )

  #dat_client_GER |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_GER
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_GER)

} # dd_read_client_GER
