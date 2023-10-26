#' Read Client Synchonys
#'
#' @param fn_list           NULL, no list of files to read
#' @param path_data         path to data
#' @param path_results_dat  path to write .RData file
#' @param sw_plot_missing   T/F plot missing values
#' @param sw_codebook       T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}
#'
#' @return dat_client_Synchonys
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
#' dd_read_client_Synchonys(
#'     fn_list           = NULL
#'   , path_data         = "../Data_in/Client_Synchonys"
#'   , path_results_dat  = path_results_dat
#'   , sw_plot_missing   = c(TRUE, FALSE)[1]
#'   , sw_codebook       = c(TRUE, FALSE)[1]
#'   )
#' }
dd_read_client_Synchonys <-
  function(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_Synchonys"
  , path_results_dat  = NULL
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  ) {

  name_dat <- "dat_client_Synchonys"

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
    if (class(dat_sheet[[ i_sheet ]]$Gender) == "logical") {
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

  dat_client_Synchonys <-
    dat_sheet |>
    dplyr::bind_rows() |>
    dplyr::select(
      -DIR__
    , -FILE__
    ) |>
    dplyr::rename(
      Client_SSN            = MemberID
    , Synchonys_CarrierID   = CarrierID
    , Synchonys_MemberName  = MemberName
    , Synchonys_DOB         = DOB
    , Synchonys_Gender      = Gender
    , Synchonys_Facility    = Facility
    , Synchonys_AdmitDate   = AdmitDate
    , Synchonys_AdmitReason = AdmitReason
    , Synchonys_Type        = Type
    , Synchonys_DischgDate  = DischgDate
    , Synchonys_DischgDx    = DischgDx
    , Synchonys_NumED30     = NumED30
    , Synchonys_NumED365    = NumED365
    , Synchonys_NumIP30     = NumIP30
    , Synchonys_NumIP365    = NumIP365
    , Synchonys_County      = County
    , Synchonys_Region      = Region
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      Synchonys_CarrierID     = Synchonys_CarrierID   |> factor()
    #, Synchonys_MemberName    = Synchonys_MemberName
    , Synchonys_DOB           = Synchonys_DOB         |> lubridate::as_date()
    , Synchonys_Gender        = Synchonys_Gender      |> factor(levels = c("M", "F"))
      # Replace n-tilde unicode
    , Synchonys_Facility      = Synchonys_Facility    |> str_replace(pattern = "\xf1", replacement = "n")
    , Synchonys_Facility      = Synchonys_Facility    |> str_replace(pattern = "\xb1", replacement = "n")
    , Synchonys_Facility      = Synchonys_Facility    |> factor()
    , Synchonys_AdmitDate     = Synchonys_AdmitDate   |> lubridate::as_date()
    #, Synchonys_AdmitReason   = Synchonys_AdmitReason
    , Synchonys_Type          = Synchonys_Type        |> factor()
    , Synchonys_DischgDate    = Synchonys_DischgDate  |> lubridate::as_date()
    , Synchonys_DischgDx      = Synchonys_DischgDx
    #, Synchonys_NumED30       = Synchonys_NumED30
    #, Synchonys_NumED365      = Synchonys_NumED365
    #, Synchonys_NumIP30       = Synchonys_NumIP30
    #, Synchonys_NumIP365      = Synchonys_NumIP365
    , Synchonys_County        = Synchonys_County      |> factor()
    , Synchonys_Region        = Synchonys_Region      |> factor()
    , dat_client_Synchonys = TRUE
    ) |>
    dplyr::relocate(
      Client_SSN
    )

  #dat_client_Synchonys |> str()

  if (!is.null(path_results_dat)) {
    save(
        dat_client_Synchonys
      , file = file.path(path_results_dat, paste0(name_dat, ".RData"))
      )
  }

  if (sw_plot_missing) {
    nmdohddrisk::dd_plot_missing_codebook(
        dat_this         = dat_client_Synchonys
      , name_dat         = name_dat
      , path_results_dat = path_results_dat
      #, sw_width         = 10
      #, sw_height        = 10
      , sw_codebook      = sw_codebook
      )
  }

  return(dat_client_Synchonys)

} # dd_read_client_Synchonys
