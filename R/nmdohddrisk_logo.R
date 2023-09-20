#' The nmdohddrisk logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param sw_unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @importFrom purrr map2
#' @importFrom crayon make_style
#' @importFrom crayon blue
#' @export
#' @examples
#' nmdohddrisk_logo()
nmdohddrisk_logo <-
  function
  (
    sw_unicode = l10n_info()$`UTF-8`
  ) {
# https://patorjk.com/software/taag/#p=display&f=Small&t=NM%20DOH%20DD%20Risk


  logo <-
    c(
      ""
    , "   0       1          2       9              3  4     "
    , " _  _ __  __   ___   ___  _  _   ___  ___    ___ _    _    "
    , "| \\| |  \\/  | |   \\ / _ \\| || | |   \\|   \\  | _ (_)__| |__ "
    , "| .` | |\\/| | | |) | (_) | __ | | |) | |) | |   / (_-< / / "
    , "|_|\\_|_|  |_| |___/ \\___/|_||_| |___/|___/  |_|_\\_/__/_\\_\\ "
    , "    4 1       9            5    0                    6  "
    , ""
    )

  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")

  if (sw_unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <-
    c("red", "yellow", "green", "magenta", "cyan", "yellow",
      "green", "white", "magenta", "cyan")

  col_hexa <-
    purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  #structure(crayon::blue(logo), class = "tidyverse_logo")
  structure(crayon::blue(logo), class = "nmdohddrisk_logo")

}


#' Print for nmdohddrisk_logo
#'
#' @param x the \code{nmdohddrisk_logo()}
#' @param ... additional parameters passed to \code{cat()}
#'
#' @return the logo, invisibly
#' @export
print.nmdohddrisk_logo <-
  function(
    x
  , ...
  ) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
