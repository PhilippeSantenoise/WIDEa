#' @importFrom shiny isolate
NULL

#' Editing the range of the main plotly graph (X, Y, Z) axes or the camera position
#' for the 3D scatter-plot

#' @description
#' `f_edit_axis_layout` returns a list including information on the (X, Y, Z) axes
#' or the camera position.

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_zoom is a reactive value including zoom coordinates of the main plotly
#' graph.
#' @param v_freq_range is the vector including the spectra frequency range (ir data
#' type).

#' @encoding UTF-8

f_edit_axis_layout <- function (s_data_type = "normal", o_parameter, o_zoom, v_freq_range = NULL) {
	if (s_data_type == "normal") {
		l_axis_layout <- NULL
		
		if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
			if (!is.null(isolate(o_zoom$coord))) {
				if (!is.na(isolate(o_parameter$dim_num)) & isolate(o_parameter$dim_num) == "3d") {
					l_axis_layout <- isolate(o_zoom$coord)
				}
				else {
					if (length(which(!is.na(isolate(o_zoom$coord)[[1]]))) > 0) {
						v_x_range <- isolate(o_zoom$coord)[[1]]
					}
					else {
						v_x_range <- NULL
					}
					
					if (length(which(!is.na(isolate(o_zoom$coord)[[2]]))) > 0) {
						v_y_range <- isolate(o_zoom$coord)[[2]]
					}
					else {
						v_y_range <- NULL
					}
					
					l_axis_layout <- list(v_x_range, v_y_range)
				}
			}
		}
	}
	else { # temporal & ir
		if (!is.null(isolate(o_zoom$coord))) {
			v_x_range <- isolate(o_zoom$coord)
		}
		else {
			if (s_data_type == "temporal") {
				v_x_range <- NULL
			}
			else { # ir
				v_x_range <- rev(v_freq_range)
			}
		}
		
		l_axis_layout <- list(v_x_range, NULL)
	}
	
	return(l_axis_layout)
}
