#' @importFrom shiny isolate
#' @importFrom plotly add_trace
#' @importFrom grDevices adjustcolor 
#' @importFrom stats sd dnorm
NULL

#' Adding normal density curves in the main plotly graph

#' @description
#' `f_add_norm_dens_curve` returns a plotly object including normal density curves. 
#' This function only concerns histplot of the normal data type.

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param v_group is the Group variable levels used to applied the corresponding
#' method. If no Group variable is selected, then `v_group` = "all".
#' @param df_click_legend are legend item data information (name and status).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param v_color is the graph color vector.

#' @encoding UTF-8

f_add_norm_dens_curve <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, v_color) {
	df_all <- isolate(o_plot$data)
	v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), isolate(o_parameter$group)), names(df_all))
	names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "group")[which(!is.na(v_pos))]
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group_all <- as.vector(unique(df_all$group))
		v_group_all <- v_group_all[order(v_group_all)]
		v_subcolor <- v_color[which(v_group_all %in% v_group)]
		l_val <- lapply(v_group, function(x) {
			v_x <- df_all[which(df_all$group == x), "x"]
			n_mean <- mean(v_x)
			n_sd <- sd(v_x)
			
			if (n_sd == 0) {
				return(list())
			}
			else {
				v_x_norm <- seq(n_mean - 3.5 * n_sd, n_mean + 3.5 * n_sd, length.out = 1000)
				v_y_norm <- dnorm(v_x_norm, n_mean, n_sd)
				return(list(v_x_norm, v_y_norm))
			}
		})
		
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (normal curve)")), "statut"]
		eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = l_val[[", 1:length(v_group), "]][[1]], y = l_val[[", 1:length(v_group), "]][[2]], name = \"", v_group, " (normal curve)\", type = \"scatter\", mode = \"lines\", line = list(color = adjustcolor(\"", v_subcolor, "\", alpha.f = 1), dash = \"dash\"), hoverlabel = list(bgcolor = adjustcolor(\"", v_subcolor, "\", alpha.f = 1)), showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
	}
	else {
		v_x <- as.vector(df_all$x)
		n_mean <- mean(v_x)
		n_sd <- sd(v_x)
		v_x_norm <- seq(n_mean - 3.5 * n_sd, n_mean + 3.5 * n_sd, length.out = 1000)
		v_y_norm <- dnorm(v_x_norm, n_mean, n_sd)
		v_visible <- df_click_legend[which(df_click_legend$name == "all (normal curve)"), "statut"]
		ply_1 <- add_trace(p = ply_1, x = v_x_norm, y = v_y_norm, name = "all (normal curve)", type = "scatter", mode = "lines", line = list(color = adjustcolor(v_color, alpha.f = 1), dash = "dash"), hoverlabel = list(bgcolor = adjustcolor(v_color, alpha.f = 1)), showlegend = T, visible = eval(parse(text = v_visible)))
	}
	
	return (ply_1)
}
