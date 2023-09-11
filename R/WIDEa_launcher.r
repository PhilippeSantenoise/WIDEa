#' WIDEa launcher

#' @description
#' `f_widea` is the WIDEa launcher

#' @export f_widea

#' @encoding UTF-8

f_widea <- function () {
	o_app <- shiny::shinyApp(f_ui(), f_server)
	return(shiny::runApp(o_app, launch.browser = T))
}
