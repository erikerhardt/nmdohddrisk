#' Read Client Syncronys
#'
#' @param fn_list           NULL, no list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_Syncronys
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
#' dd_read_client_Syncronys(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_Syncronys"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_Syncronys <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_Syncronys"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_Syncronys"

  dat_sheet <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "txt$"
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    , sw_delim                = c(FALSE, "|")[2]
    )

  length(dat_sheet)

  # a few are tab delimited
  dat_sheet2 <-
    erikmisc::e_read_data_subdir_into_lists(
      fn_path                 = path_data
    , fn_detect               = "txt$"
    , sw_fn_or_dat            = c("fn", "dat")[2]
    , sw_exclude_empty_dir    = c(TRUE, FALSE)[1]
    , sw_dat_add_col_path_fn  = c(TRUE, FALSE)[1]
    , sw_dat_print_fn_read    = c(TRUE, FALSE)[1]
    , sw_clean_names          = c(TRUE, FALSE)[1]
    , sw_list_or_flat         = c("list", "flat")[2]
    , sw_delim                = c(FALSE, "\t")[2]
    )

  length(dat_sheet2)

  ind_tab_delim <- which(unlist(lapply(dat_sheet2, ncol)) > 3)
  ind_tab_delim

  for (i_tab in ind_tab_delim) {
    ## i_tab = ind_tab_delim[4]

    #str(dat_sheet2[[ i_tab ]])

    dat_sheet[[ i_tab ]] <-
      dat_sheet2[[ i_tab ]] |>
      dplyr::mutate(
        DOB        = DOB        |> lubridate::parse_date_time(orders = c("mdy"   , "%A %m %d %Y", "%m/%d/%Y"))
      , AdmitDate  = AdmitDate  |> lubridate::parse_date_time(orders = c("mdy HM", "%A %m %d %Y"))
      , DischgDate = DischgDate |> lubridate::parse_date_time(orders = c("mdy HM", "%A %m %d %Y"))
      )
  }

  #lapply(dat_sheet, nrow) |> unlist() |> hist(50)

  # remove empty sheets in reverse because of decrement affect if forward
  ind_0_rows <- sort(which(unlist(lapply(dat_sheet, nrow)) == 0), decreasing = TRUE)
  for (i_0_rows in ind_0_rows) {
    dat_sheet[[ i_0_rows ]] <- NULL
  }

  # Standardize data
  for (i_sheet in seq_len(length(dat_sheet))) {
    ## i_sheet = 1
    ## i_sheet = "2022-9 Sept.Admits-DDSD_REG_NM-20220926.txt"

    # if all Gender is "F", the class becomes logical for FALSE, fix this
    if ( inherits(dat_sheet[[ i_sheet ]]$Gender, "logical") ) {
      dat_sheet[[ i_sheet ]]$Gender <- as.character(dat_sheet[[ i_sheet ]]$Gender)
      dat_sheet[[ i_sheet ]]$Gender[(dat_sheet[[ i_sheet ]]$Gender == "FALSE")] <- "F"
    }

    # if DischgDate has no values, then character, change to datetime
    if (class(dat_sheet[[ i_sheet ]]$DischgDate)[1] == "character") {
      # only one: "2022-3 March.Admits-DDSD_REG_NM-20220309.txt"

      dat_sheet[[ i_sheet ]] <-
        dat_sheet[[ i_sheet ]] |>
        dplyr::mutate(
          DischgDate = DischgDate |> lubridate::mdy_hm()
        )
    }
  }

  dat_client_Syncronys <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::select(
      -DIR__
    , -FILE__
    ) |>
    dplyr::rename(
      Client_System_ID      = MemberID
    , Syncronys_CarrierID   = CarrierID
    , Syncronys_MemberName  = MemberName
    , Syncronys_DOB         = DOB
    , Syncronys_Gender      = Gender
    , Syncronys_Facility    = Facility
    , Syncronys_AdmitDate   = AdmitDate
    , Syncronys_AdmitReason = AdmitReason
    , Syncronys_Type        = Type
    , Syncronys_DischgDate  = DischgDate
    , Syncronys_DischgDx    = DischgDx
    , Syncronys_NumED30     = NumED30
    , Syncronys_NumED365    = NumED365
    , Syncronys_NumIP30     = NumIP30
    , Syncronys_NumIP365    = NumIP365
    , Syncronys_County      = County
    , Syncronys_Region      = Region
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      Syncronys_CarrierID     = Syncronys_CarrierID   |> factor()
    #, Syncronys_MemberName    = Syncronys_MemberName
    , Syncronys_DOB           = Syncronys_DOB         |> lubridate::as_date()
    , Syncronys_Gender        = Syncronys_Gender      |> factor(levels = c("M", "F"))
      # Replace n-tilde unicode
    , Syncronys_Facility      = Syncronys_Facility    |> str_replace(pattern = "\xf1", replacement = "n")
    , Syncronys_Facility      = Syncronys_Facility    |> str_replace(pattern = "\xb1", replacement = "n")
    , Syncronys_Facility      = Syncronys_Facility    |> factor()
    , Syncronys_AdmitDate     = Syncronys_AdmitDate   |> lubridate::as_date()
    #, Syncronys_AdmitReason   = Syncronys_AdmitReason
    , Syncronys_Type          = Syncronys_Type        |> factor()
    , Syncronys_DischgDate    = Syncronys_DischgDate  |> lubridate::as_date()
    , Syncronys_DischgDx      = Syncronys_DischgDx
    #, Syncronys_NumED30       = Syncronys_NumED30
    #, Syncronys_NumED365      = Syncronys_NumED365
    #, Syncronys_NumIP30       = Syncronys_NumIP30
    #, Syncronys_NumIP365      = Syncronys_NumIP365
    , Syncronys_County        = Syncronys_County      |> factor()
    , Syncronys_Region        = Syncronys_Region      |> factor()
    , Date                    = Syncronys_AdmitDate
    , dat_client_Syncronys = TRUE
    ) |>
    dplyr::relocate(
      Client_System_ID
    , Date
    ) |>
    dplyr::arrange(
      Client_System_ID
    , Date
    )

  #dat_client_Syncronys |> str()

  name_dat |> dd_save_to_RData()
  # if (!is.null(path_results_dat)) {
  #   save(
  #     list = ls(pattern = name_dat)
  #   , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
  #   )
  # }

  if (sw_plot_missing) {
    dd_plot_missing_codebook(
        dat_this         = dat_client_Syncronys
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_Syncronys)

} # dd_read_client_Syncronys
