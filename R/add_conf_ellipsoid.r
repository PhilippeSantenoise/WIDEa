#' @importFrom shiny isolate
#' @importFrom plotly add_trace
#' @importFrom grDevices adjustcolor 
NULL

#' Adding 95% confidence ellipsoids in the main plotly graph

#' @description
#' `f_add_conf_ellipsoid` returns a plotly object including 95% confidence
#' ellipsoids. This function only 2D plot of the normal data type (disabled if a
#' calibration model is applied).

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param v_group is the Group variable levels used to applied the corresponding
#' method. If no Group variable is selected, then `v_group` = "all".
#' @param df_click_legend are legend item data information (name and status).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param l_graph_opt is the list of graph option (color, opacity, point type/size,
#' sorting) created by the `f_create_graph_opt_vector` function.

#' @encoding UTF-8

f_add_conf_ellipsoid <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, l_graph_opt) {
	df_all <- isolate(o_plot$data)
	v_min_max <- c()
	
	if (isolate(o_parameter$model) == "valid") { # validation model
		df_model <- isolate(o_plot$model)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		df_all <- as.data.frame(df_all[, v_pos[!is.na(v_pos)]])
		names(df_all) <- c("y", "group")[which(!is.na(v_pos))]
		df_all$x <- as.vector(df_model$fit)
		rm(list = "df_model")
	}
	else { # no model
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "y", "group")[which(!is.na(v_pos))]
	}
	
	if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
		v_group_all <- as.vector(unique(df_all$group))
		v_group_all <- v_group_all[order(v_group_all)]
		
		l_val <- lapply(v_group, function(x) {
			df_group <- df_all[which(df_all$group == x),]
			df_out <- car::dataEllipse(x = as.vector(df_group$x), y = as.vector(df_group$y), levels = 0.95, draw = F, segments = 100)
			return(df_out)
		})
		
		v_pos <- which(v_group_all %in% v_group) 
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group_all[v_pos], " (ellipsoid)")), "statut"]
		v_order <- order(l_graph_opt$sorting[v_pos])
		eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = l_val[[", v_order, "]][, 1], y = l_val[[", v_order, "]][, 2], name = \"", v_group[v_order], " (ellipsoid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_pos][v_order], "\", alpha.f = 1), width = 1.5), fill = \"toself\", fillcolor = list(color = adjustcolor(\"", l_graph_opt$color[v_pos][v_order], "\", alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_pos][v_order], "\", alpha.f = 1)), hoverinfo = \"name\", showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
	}
	else {
		s_color <- ifelse(isolate(o_parameter$quant_group), "black", l_graph_opt$color)
		df_val <- car::dataEllipse(x = as.vector(df_all$x), y = as.vector(df_all$y), levels = 0.95, draw = F, segments = 100)
		eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (ellipsoid)"), "statut"])))
		ply_1  <- add_trace(p = ply_1, x = df_val[, 1], y = df_val[, 2], name = "all (ellipsoid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "lines", line = list(color = adjustcolor(s_color, alpha.f = 1), width = 1.5), fill = "toself", fillcolor = list(color = adjustcolor(s_color, alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(s_color, alpha.f = 1)), hoverinfo = "name", showlegend = T, visible = v_visible)
		if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(df_val[, 1]), range(df_val[, 2]))}
	}
	
	if (length(v_min_max) > 0) {
		return(list(ply_1, v_min_max))
	}
	else {
		return (ply_1)
	}
}
