#' Add custom lines to a plotly graph

#' @description
#' List of funtions used to add vertical (`f_vline`), horizontal (`f_hline`) and ab
#' (`f_abline`) lines to a plotly graph.

#' @param x is the x-axis (numeric) value.
#' @param y is the y-axis (numeric) value.
#' @param xmin is the x-axis minimal (numeric) value.
#' @param xmax is the x-axis maximal (numeric) value.
#' @param a is the intercept (numeric) value.
#' @param b is the slope (numeric) value.
#' @param color is a string value corresponding to the line color.
#' @param width is a numeric value corresponding to the line width.
#' @param dash is a string value equal to "dash" if the line is dashed (NULL as
#' default).

#' @encoding UTF-8

f_vline <- function(x = 0, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		y0 = 0, 
		y1 = 1, 
		yref = "paper",
		x0 = x, 
		x1 = x, 
		line = list(color = color, width = width, dash = dash)
	)
}

#' @rdname f_vline
f_hline <- function(y = 0, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		x0 = 0, 
		x1 = 1, 
		xref = "paper",
		y0 = y, 
		y1 = y, 
		line = list(color = color, width = width, dash = dash)
	)
}

#' @rdname f_vline
f_abline <- function(xmin = 0, xmax = 1, a = 0, b = 1, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		x0 = xmin, 
		x1 = xmax, 
		y0 = a + b * xmin, 
		y1 = a + b * xmax, 
		line = list(color = color, width = width, dash = dash)
	)
}
