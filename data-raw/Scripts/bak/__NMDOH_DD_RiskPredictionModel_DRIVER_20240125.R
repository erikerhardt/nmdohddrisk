# NMDOH DD Risk Prediction Model
# Read and format data, Run Model, Summarize model results
# Prof. Erik Erhardt, PhD, StatAcumen.com
# 2024-01-25
#
# This file is used to set the parameters and run the model
#
# NMDOH Data Team:
#
# Step 0: Open this file in RStudio                 -------
# Step 1: Set these parameters                      -------
# Step 2: To run, press Ctrl-Alt-R  (about 1 hour)  -------
# Step 3: Close RStudio, review pdf output          -------
#

date_Current      = "2024-01-01"
DD_Group          = "All"
date_today        = lubridate::today()
path_prefix_out   = "out_V16x"
sw_read_data      = FALSE
sw_run_model      = FALSE
working_directory = "D:/Dropbox/StatAcumen/consult/Industry/2023_NM_DOH_DDWaiver/data/Zdrive/Risk_Prediction_Model/Scripts"

#
#
# ===========================================================================
# Please do not edit anything below -----------------------------------------

setwd(working_directory)

# filenames
fn_root <-
  "NMDOH_DD_RiskPredictionModel_Data-Model-Summary"
fn_in_qmd <-
  paste0("__", fn_root, "_SOURCE_V16_20240125.qmd")
fn_out <-
  paste0(fn_root, "_", date_today, "_", path_prefix_out, "_", date_Current, "_", DD_Group)
fn_out_qmd <- paste0(fn_out, ".qmd")
fn_out_pdf <- paste0(fn_out, ".pdf")
fn_out_md  <- paste0(fn_out, ".knit.md")
fn_out_tex <- paste0(fn_out, ".tex")

# set placeholder values
PARAMS <-
  tibble::tibble(
    PARAMS_DD_Group          = DD_Group
  , PARAMS_date_today        = date_today
  , PARAMS_date_Current      = date_Current
  , PARAMS_path_prefix_out   = path_prefix_out
  , PARAMS_sw_read_data      = sw_read_data
  , PARAMS_sw_run_model      = sw_run_model
  , PARAMS_working_directory = working_directory
  , PARAMS_output_file       = fn_out_pdf
  )

# read template
template_code <-
  readr::read_file(
    file = fn_in_qmd
  )

# replace placeholders with values
for (i_col in seq_len(ncol(PARAMS))) {
  ## i_col = 1

  template_code <-
    template_code |>
    stringr::str_replace(
      pattern     = stringr::fixed(names(PARAMS)[i_col])
    , replacement = as.character(PARAMS[[ i_col ]])
    )
} # i_col

# write qmd file
readr::write_file(
    x     = template_code
  , file  = fn_out_qmd
  )

# render (run the model)
quarto::quarto_render(
    input = fn_out_qmd
  )

# remove files that should be deleted
unlink(fn_out_qmd )
unlink(fn_out_md  )
unlink(fn_out_tex )
unlink(".Rhistory")

## END
