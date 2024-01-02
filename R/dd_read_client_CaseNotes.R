#' Read Client CaseNotes
#'
#' @param fn_list           NULL, no list of files to read (read all in the path)
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_CaseNotes
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
#' dd_read_client_CaseNotes(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_CaseNotes"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_CaseNotes <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_CaseNotes"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_CaseNotes"

  # Detail sheets
  dat_sheet_detail <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = file.path(path_data, "CaseNotes_Detail")
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


  dat_client_CaseNotes_detail <-
    dat_sheet_detail |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
        #  "Provider_Code"
          "Form_ID"
        , "Status"
        #, "Template_Name"
        , "Oversight_ID"
        #, "Individual"
        #, "Individual_Status"
        , "Service_Date"
        #, "Submit_Date"
        #, "Entered_By"
        #, "Service_Provider"
        #, "Time_From"
        #, "Time_To"
        #, "Time_Duration_Minutes"
        #, "Time_Duration_Hours"
        #, "Service_Description_Code"
        #, "Unit_Rate"
        #, "Billing_Diagnosis_Code"
        #, "Activity_Type"
        #, "Location"
        #, "Billable"
        #, "Face_to_Face"
        #, "Person_Contacted"
        #, "Note"
        #, "Billed"
        )
      )
    ) |>
    dplyr::rename(
      CaseNotes_Form_ID   = Form_ID
    , CaseNotes_Status    = Status
    , Client_TherapID     = Oversight_ID
    , CaseNotes_Date      = Service_Date
    ) |>
    dplyr::mutate(
      CaseNotes_Date = CaseNotes_Date |> lubridate::mdy()
    ) |>
    dplyr::relocate(
      Client_TherapID
    )


  # Questionnaire sheets
  dat_sheet_questionnaire <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = file.path(path_data, "CaseNotes_Questionnaire")
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


  dat_client_CaseNotes_questionnaire <-
    dat_sheet_questionnaire |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "Form_ID"
        #, "Individual_Last_First_name"
        #, "Provider_Agency"
        , "Question"
        , "Answer"
        #, "IDF_Form_ID"
        #, "Oversight_ID"
        #, "Service_Date"
        )
      )
    ) |>
    dplyr::rename(
      CaseNotes_Form_ID  = Form_ID
    , CaseNotes_Question = Question
    , CaseNotes_Answer   = Answer
    )


  dat_client_CaseNotes_Q <-
    dat_client_CaseNotes_detail |>
    dplyr::left_join(
      dat_client_CaseNotes_questionnaire
    , by = dplyr::join_by(CaseNotes_Form_ID)
    ) |>
    dplyr::filter(
      CaseNotes_Status == "Submitted"
    ) |>
    dplyr::select(
      -CaseNotes_Status
    )


  # Weights
  dat_sheet_weights <-
    readxl::read_xlsx(
      path = file.path(path_data, "DD Waiver PRM Questions with Weights.xlsx")
    )

  list_client_CaseNotes_Weights <- list()
  dat_client_CaseNotes_Weights  <- list()

  for (i_row in seq_len(nrow(dat_sheet_weights))) {
    ## i_row = 1

    list_client_CaseNotes_Weights[[ i_row ]] <- list()
    dat_client_CaseNotes_Weights [[ i_row ]] <- list()

    list_client_CaseNotes_Weights[[ i_row ]]["Question"] <-
      dat_sheet_weights[[ "Question" ]][i_row]

    for (i_col in 2:ncol(dat_sheet_weights)) {
      ## i_col = 2

      this_col_name <- names(dat_sheet_weights)[i_col]

      this_list <-
        dat_sheet_weights[[ this_col_name ]][i_row] |>
        stringr::str_split(
          pattern = stringr::fixed("\r\n")
        ) |>
        unlist()

      ind_keep <-
        !(this_list == "")

      list_client_CaseNotes_Weights[[ i_row ]][[ this_col_name ]] <-
        this_list[ind_keep]

      dat_client_CaseNotes_Weights[[ i_row ]][[ i_col ]] <-
        tibble::tibble(
          CaseNotes_Question = list_client_CaseNotes_Weights[[ i_row ]][[ "Question" ]]
        , CaseNotes_Answer   = list_client_CaseNotes_Weights[[ i_row ]][[ this_col_name ]]
        , CaseNotes_AtRisk   = this_col_name |> e_extract_numbers_from_string() |> as.numeric()
        )
    }

    dat_client_CaseNotes_Weights[[ i_row ]] <-
      dat_client_CaseNotes_Weights[[ i_row ]] |>
      dplyr::bind_rows()

  }

  dat_client_CaseNotes_Weights <-
    dat_client_CaseNotes_Weights |>
    dplyr::bind_rows() |>
    tidyr::drop_na()


  # Join with weights and sum the weights
  dat_client_CaseNotes <-
    dat_client_CaseNotes_Q |>
    dplyr::left_join(
      dat_client_CaseNotes_Weights
    , by = dplyr::join_by(CaseNotes_Question, CaseNotes_Answer)
    ) |>
    tidyr::replace_na(
      replace = list(CaseNotes_AtRisk = 0)
    ) |>
    dplyr::group_by(
      Client_TherapID
    , CaseNotes_Form_ID
    , CaseNotes_Date
    ) |>
    dplyr::summarize(
      CaseNotes_AtRisk = CaseNotes_AtRisk |> sum(na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -CaseNotes_Form_ID
    )


  #dat_client_CaseNotes |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_CaseNotes
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_CaseNotes)

} # dd_read_client_CaseNotes
