#' @importFrom shiny addResourcePath
#' @noRd
.onAttach <- function(libname, pkgname){
  shiny::addResourcePath(
    "wwwSP",
    system.file("www", package = "shinyPrettier")
  )
}

#' @importFrom shiny removeResourcePath
#' @noRd
.onDetach <- function(libpath){
  shiny::removeResourcePath("wwwSP")
}
