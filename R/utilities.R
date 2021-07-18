#' Create methods node from .md file
#'
#' @param f (character) Full path to methods.md file
#'
#' @return (list) EML methodStep node for markdown as supported by the Environmental Data Initiative Data Repository
#'
set_methods_md <- function(f) {
  txt <- readr::read_file(f)
  res <- list(
    methodStep = list(
      description = list(
        markdown = txt)))
  return(res)
}









#' Write template to file
#'
#' @param tmplt (data.frame) Template
#' @param name (character) Template file name (including extension)
#' @param path (character) Path to write to
#' @param force (logical) Overwrite existing template?
#'
#' @return (file, logical) Template and TRUE if written
#' 
#' @details Only works for tabular templates.
#' 
write_template <- function(tmplt, name, path, force = FALSE) {
  f <- paste0(path, "/", enc2utf8(name))
  if (file.exists(f) & !isTRUE(force)) {
    warning(f, " exists and will not be overwritten", call. = FALSE)
    return(FALSE)
  } else {
    data.table::fwrite(x = tmplt, file = f, sep = "\t", quote = FALSE)
    return(TRUE)
  }
}