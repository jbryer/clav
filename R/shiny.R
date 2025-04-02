#' Run the Shiny application.
#'
#' @export
cluster_shiny <- function() {
	shiny::runApp(paste0(find.package('clav'), '/shiny/'))
}
