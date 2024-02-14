#' @importFrom shiny isolate
#' @importFrom plotly layout plotlyProxyInvoke
NULL

#' Adding the main plotly graph layout

#' @description
#' `f_add_axis_layout` returns a plotly object including (x, y, z) axis layout. This
#' function only concerns 2D/3D plot of the normal/ir data type.

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param s_data_type is the data type: "normal" or "ir".
#' @param df_click_legend are legend item data information (name and status).
#' @param df_min_max is a data.frame including min-max values of each item 
#' (`df_click_legend`) added in the main graph.
#' @param o_label_text is a reactive value including data information on the graph
#' label edition.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels
#' @param v_bg_grid_color is a vector including the background/grid color.
#' @param l_camera is a list given the camera position of plot 3d (data type:
#' normal). The camera position is saved by the reactive value `o_zoom$coord`.
#' @param s_option has two values: "layout", "relayout". "layout" is used when the
#' display button is clicked and "relayout" is used when legend items are clicked. 

#' @encoding UTF-8

f_add_axis_layout <- function (ply_1, s_data_type, df_click_legend, o_label_text, df_min_max, o_parameter, v_bg_grid_color, l_camera = NULL, s_option = "layout") {
	v_row <- which(df_click_legend$statut == "T")
	v_row <- eval(parse(text = ifelse(length(v_row) > 0, "which(df_min_max$name %in% df_click_legend[v_row, \"name\"])", "1:nrow(df_min_max)")))
	s_ylab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")], isolate(o_parameter$ylab)), isolate(o_parameter$ylab))
	v_ylim <- c(min(df_min_max[v_row, "ymin"]), max(df_min_max[v_row, "ymax"]))
	v_ylim <- c(v_ylim[1] - 0.06 * abs(diff(v_ylim)), v_ylim[2] + 0.06 * abs(diff(v_ylim)))
	
	if (s_data_type == "normal") {
		s_xlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")], isolate(o_parameter$xlab)), isolate(o_parameter$xlab))
		v_xlim <- c(min(df_min_max[v_row, "xmin"]), max(df_min_max[v_row, "xmax"]))
		v_xlim <- c(v_xlim[1] - 0.06 * abs(diff(v_xlim)), v_xlim[2] + 0.06 * abs(diff(v_xlim)))
		
		if (isolate(o_parameter$dim_num) == "2d") {
			if (s_option == "layout") {
				ply_1 <- layout(p = ply_1, xaxis = list(title = s_xlab, range = v_xlim, gridcolor = v_bg_grid_color[2]), yaxis = list(title = s_ylab, range = v_ylim, gridcolor = v_bg_grid_color[2]))
			}
			else { # relayout
				ply_1 <- plotlyProxyInvoke(p = ply_1, "relayout", list(xaxis = list(title = s_xlab, range = v_xlim, gridcolor = v_bg_grid_color[2]), yaxis = list(title = s_ylab, range = v_ylim, gridcolor = v_bg_grid_color[2])))
			}
		}
		else { # plot 3d
			s_zlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "z")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "z")], isolate(o_parameter$zlab)), isolate(o_parameter$zlab))
			v_zlim <- c(min(df_min_max[v_row, "zmin"]), max(df_min_max[v_row, "zmax"]))
			v_zlim <- c(v_zlim[1] - 0.06 * abs(diff(v_zlim)), v_zlim[2] + 0.06 * abs(diff(v_zlim)))
			l_scene <- list(xaxis = list(title = s_xlab, range = v_xlim, backgroundcolor = v_bg_grid_color[1], showbackground = T, gridcolor = v_bg_grid_color[2]), yaxis = list(title = s_ylab, range = v_ylim, backgroundcolor = v_bg_grid_color[1], showbackground = T, gridcolor = v_bg_grid_color[2]), zaxis = list(title = s_zlab, range = v_zlim, backgroundcolor = v_bg_grid_color[1], showbackground = T, gridcolor = v_bg_grid_color[2]))
			if (!is.null(l_camera)) {l_scene$camera <- list(up = l_camera[[1]], center = l_camera[[2]], eye = l_camera[[3]])}
			
			if (s_option == "layout") {
				ply_1 <- layout(p = ply_1, scene = l_scene)
			}
			else { # relayout
				ply_1 <- plotlyProxyInvoke(p = ply_1, "relayout", list(scene = l_scene))
			}
		}
	}
	else {
		if (s_option == "layout") {
			ply_1 <- layout(p = ply_1, yaxis = list(title = s_ylab, range = v_ylim, gridcolor = v_bg_grid_color[2]))
		}
		else { # relayout
			ply_1 <- plotlyProxyInvoke(p = ply_1, "relayout", list(yaxis = list(title = s_ylab, range = v_ylim, gridcolor = v_bg_grid_color[2])))
		}
		# todo
	}
	
	return(ply_1)
}