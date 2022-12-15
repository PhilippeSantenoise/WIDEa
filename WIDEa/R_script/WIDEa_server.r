####################################################################################
# Copyright 2021 - INRAE - Philippe Santenoise
#
# This R script is a part of WIDEa
#
# WIDEa is a free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# WIDEa is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with WIDEa (see the file License.txt). If not, see
# <http://www.gnu.org/licenses/>
#
#
# Description: Creating the server
#
# Creation date: November 2022
####################################################################################


f_server <- function(input, output, session) {
	# Suppress warning message
	options(warn = -1)
	
	# =====
	# Start
	# =====
	
	# 1.1. Initial parameters
	# =======================
	
	# environment including all data loaded from "Data loading" and "Model parameter loading" sections (left panel). Flag data saved in this environmment (from "with flags" checking box) are considered as previous flags  
	e_data <- new.env() 
	
	# environment including detailed data of previous flags (only used for the temporal data type) 
	e_previous_flag <- new.env() 
	
	# environment including flag data saved from a click event on the graph ("plotly_click" with event_data function). Flag data are considered as current flags 
	e_current_flag <- new.env() 
	
	# reactive values used to enable/disable inputs
	o_on_off <- reactiveValues(val = NULL, tp = list("id" = NULL, "status" = NULL)) # list of inputs to be enabled/disabled. o_on_off$tp is used to enable/disable top panel inputs when the update button is clicked
	o_input_status <- reactiveValues(val = f_init_input_status_data(), display = data.frame()) # data frame with UI's input status (0: disabled, 1: enabled). o_input_status$display: part of o_input_status$val data frame saved when the display button is clicked
	o_special_input <- reactiveValues(val = list()) # list of inputs associated to a special enabling/desabling
	
	# reactive value used to update inputs
	o_selectize_input <- reactiveValues(max_item = list(), max_item_current = list("var_x" = 9999, "var_y" = 9999)) # maximum number of items information associated to selectize inputs
	o_update <- reactiveValues(val = NULL) # value used to reset o_cond$update reactive value
	
	o_reset <- reactiveValues(code = 0, model1 = 0, model2 = 0, subdata = 0, concat = 0, label = 0, color_opacity = 0, point_type_size = 0) # reactuve value used to reset all reactive values
	
	# parameters associated to load data fields
	o_path <- reactiveValues(name1_prev = NA, name2_prev = NA, name3_prev = NA, name1 = NA, name2 = NA, name3 = NA, color1 = "white", color2 = "transparent", color3 = "transparent") # path, color assigned to fields
	o_load_error <- reactiveValues(code = rep(0, 3)) # error message returned (1) or not (0)
	o_data_info <- reactiveValues(mtime = rep(NA, 3), code = NA) # data modification time and code range (ir data type)
	
	o_flag <- reactiveValues(name = NA) # flag data name
	o_click_button <- reactiveValues(left_panel = 0, top_panel = 0, both_panel = 0, both_panel_prev = 0, browse1 = 0, browse2 = 0, browse3 = 0, load1 = 0, load2 = 0, load3 = 0, display = 0, create = 0) # parameters associated to several buttons: clicked (1) or not (0)
	o_click_graph <- reactiveValues(prev_date = NULL, prev_var = NULL) # parameters associated to current flag data for the temporal data type (used to draw an interval) 
	o_click_legend <- reactiveValues(item = NULL) # parameter corresponding to graph legend items (including name, statut)
	o_zoom <- reactiveValues(coord = NULL) # zoom coordinates
	
	# parameters associated to sub-data creation
	o_sdata_cond <- reactiveValues(var_name = c(), var_type = c(), var_rel = c(), value = list(), formula = NA, row_num = c())
	o_sdata <- reactiveVal()
	
	o_cond <- reactiveValues(
		top_panel = 0, update = 0, sdata = rep(0, 2), create = 0, concat1 = 0, concat2 = 0,
		display = 0, flag = 0, qc1 = 0, qc2 = 0, flag_msg = 0, save1 = 0, save2 = 0, 
		select_graph1 = 0, select_graph2 = 0, reset2 = 0, stat = NA, legend = 0,
		webgl = 0, mode = 0, ok_color_opacity = 0, ok_point_type_size = 0
	)
	
	o_plot <- reactiveValues(
		data = data.frame(), model = NULL, code_freq = NA, id_group = NA, y_coord = NULL,
		add_pt = T, pt_pos = NA, var_pt = NA,
		data_qc1 = NA, data_qc2 = NA, var_qc1 = NA, var_qc2 = NA, leg_name_qc = NA, 
		elt = NA, elt_pt_pos = NA
	) # parameters associated to the display button
	
	o_picture_info <- reactiveValues(filename = "Picture_name", format = "png", height = 800, width = 1000) # parameters associated to the graph picture
	o_lreg_info <- reactiveValues(xpos = 0, ypos = 1, elt = NA) # parameters associated to the linear regression legend
	
	# reactive value used to update a plotly object in a modal dialog created by clicking on a correlation matrix cell (normal data type: corplot)
	o_click_corplot <- reactiveValues(num = NULL, data = NULL, plotly = NULL) 
	
	o_parameter <- reactiveValues(
		data_name = NA, plot_type = NA, dim_num = NA, model = NA, 
		id = NA, ref = NA, concat1 = F, concat1_group = NA, x = NA, f = NA, date_format = NA, y = NA, g = NA, z = NA, h = NA, wres_group = NA, wres_vfun = NA, concat2 = F, concat2_group = NA, group = NA,
		webgl = "yes", mode = "marker", autobw = T, bw = NA, y_scale = "none", autodec_num = T, dec_num = NA, xlab = "x", ylab = "y", zlab = "z",
		lreg = F, conf_ellipsoid = F, centroid = F, boxmean = "NULL", dens_curve = F, norm_dens_curve = F, mean_spect = F,
		select_graph = NA, corplot_group = NA
	) # parameters associated to the left panel (all sections after data loading) and top panel tabs
	
	# parameters associated to graph label edition
	o_label_text <- reactiveValues(del = list("x" = 0, "y" = 0, "z" = 0), label = c(), text = c(), label_temp = c(), text_temp = c())
	
	# parameters associated to graph option (color, opacity, point type/size) edition
	o_name_option <- reactiveValues(name = c(), var = c(), warning = 0, color = c(), color_temp = c(), color_default = c(), opacity = c(), opacity_temp = c(), opacity_default = c(), point_type = c(), point_type_temp = c(), point_type_default = c(), point_size = c(), point_size_temp = c(), point_size_default = c())
	
	o_expand <- reactiveValues(info_cond_data = NULL, info_var_data = NULL, code_var_data = NULL) # reactive value used to update expand modal dialog datatable 
	o_option <- reactiveValues(choices = c(), data = NULL, plotly = NULL) # reactive value used to update "edit_option" selectize input (label, color/opacity, point type/size) and graph option modal dialog datatable/plotly 
	o_legend_group <- reactiveValues(lreg = c(), conf_ellipsoid = c(), dens_curve = c(), norm_dens_curve = c()) # graph legend items associated to several statistical methods 
	o_w_message <- reactiveValues(lreg = 0, conf_ellipsoid = 0, dens_curve = 0, norm_dens_curve = 0) # parameters used to display (0) or not (1) a warning message returned by several statistical methods
	
	# 1.2. Add events of hide/show panel buttons
	# ==========================================
	
	# left panel
	
	observeEvent(input$hs_lpanel_button, {
		b_cond <- o_click_button$display == 1 & (input$plot_type == "corplot" | input$model != "none") # condition associated to the top main panel (T: show, F: hide)
		
		if (isolate(o_click_button$left_panel) == 1) {
			shinyjs::show(id = "lpanel")
			shinyjs::removeClass("tpanel", class = "tpanel_class2")
			shinyjs::addClass("tpanel", class = "tpanel_class1")
			o_cond$top_panel <- 0
			
			if (isolate(o_click_button$top_panel) == 1) {
				if (isolate(o_click_button$both_panel_prev) == 1) {o_click_button$both_panel_prev <- 0}
				o_on_off$val <- list("hs_bpanel_button" = 0)
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class4")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class3")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 7, 3)))
			}
			else {
				o_on_off$val <- list("hs_bpanel_button" = 1)
				o_click_button$both_panel <- 0
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class2")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class1")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 6, 2)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
			}
			
			o_click_button$left_panel <- 0
		}
		else {
			shinyjs::hide(id = "lpanel")
			shinyjs::removeClass("tpanel", class = "tpanel_class1")
			shinyjs::addClass("tpanel", class = "tpanel_class2")
			o_cond$top_panel <- 1
			
			if (isolate(o_click_button$top_panel) == 1) {
				o_on_off$val <- list("hs_bpanel_button" = 1)
				o_click_button$both_panel <- 1
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class3")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class4")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 7, 3)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
			}
			else {
				o_on_off$val <- list("hs_bpanel_button" = 0)
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class1")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class2")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 6, 2)))
			}
			
			o_click_button$left_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
	})
	
	# top panel
	
	observeEvent(input$hs_tpanel_button, {
		b_cond <- o_click_button$display == 1 & (input$plot_type == "corplot" | input$model != "none") # condition associated to the top main panel (T: show, F: hide)
		
		if (isolate(o_click_button$top_panel) == 1) {
			if (isolate(o_click_button$left_panel) == 1) {
				if (isolate(o_click_button$both_panel_prev) == 1) {
					shinyjs::removeClass("tpanel", class = "tpanel_class1")
					shinyjs::addClass("tpanel", class = "tpanel_class2")
					shinyjs::show(id = "tpanel")
					o_click_button$both_panel_prev <- 0
				}
				else {
					shinyjs::show(id = "tpanel")
				}
				
				o_on_off$val <- list("hs_bpanel_button" = 0)
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class4")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class2")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 6, 2)))
			}
			else {
				shinyjs::show(id = "tpanel")
				o_on_off$val <- list("hs_bpanel_button" = 1)
				o_click_button$both_panel <- 0
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class3")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class1")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 7, 3)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
			}
			
			o_click_button$top_panel <- 0
		}
		else {
			shinyjs::hide(id = "tpanel")
			
			if (isolate(o_click_button$left_panel) == 1) {
				o_on_off$val <- list("hs_bpanel_button" = 1)
				o_click_button$both_panel <- 1
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class2")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class4")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 6, 2)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
			}
			else {
				o_on_off$val <- list("hs_bpanel_button" = 0)
				shinyjs::removeClass("tmainpanel", class = "tmainpanel_class1")
				shinyjs::addClass("tmainpanel", class = "tmainpanel_class3")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 7, 3)))
			}
			
			o_click_button$top_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
	})
	
	# both panels
	
	observeEvent(input$hs_bpanel_button, {
		b_cond <- o_click_button$display == 1 & (input$plot_type == "corplot" | input$model != "none") # condition associated to the top main panel (T: show, F: hide)
		
		if (isolate(o_click_button$both_panel) == 1) {
			if (isolate(o_cond$top_panel) == 1) {
				shinyjs::removeClass("tpanel", class = "tpanel_class2")
				shinyjs::addClass("tpanel", class = "tpanel_class1")
				o_cond$top_panel <- 0
			}
			
			shinyjs::show(id = "lpanel")
			shinyjs::show(id = "tpanel")
			shinyjs::removeClass("tmainpanel", class = "tmainpanel_class4")
			shinyjs::addClass("tmainpanel", class = "tmainpanel_class1")
			shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
			shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
			o_click_button$both_panel_prev <- 0
			o_click_button$both_panel <- 0
			o_click_button$left_panel <- 0
			o_click_button$top_panel <- 0
		}
		else {
			shinyjs::hide(id = "lpanel")
			shinyjs::hide(id = "tpanel")
			shinyjs::removeClass("tmainpanel", class = "tmainpanel_class1")
			shinyjs::addClass("tmainpanel", class = "tmainpanel_class4")
			shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 5, 1)))
			shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(b_cond, 8, 4)))
			o_click_button$both_panel_prev <- 1
			o_click_button$both_panel <- 1
			o_click_button$left_panel <- 1
			o_click_button$top_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
	})
	
	# 1.3. Add events on o_on_off$val reactive value
	# ==============================================
	# Enable/desable UI's inputs
	
	observeEvent(o_on_off$val, {
		if (!is.null(o_on_off$val)) {
			l_id_status <- o_on_off$val
			o_on_off$val <- NULL
			o_special_input$val <- f_update_sp_input_value_list(o_special_input$val, l_id_status, o_input_status$val)
			
			if (length(o_selectize_input$max_item) > 0 | length(which(paste0("var_", c("x", "y", "group")) %in% names(l_id_status))) > 0) {
				l_results <- f_update_input_maxitem_list(o_selectize_input, l_id_status)
				eval(parse(text = paste(paste0("o_selectize_input$", c("max_item", "max_item_current"), " <- l_results[[", 1:2, "]]"), collapse = "; ")))
			}
			
			if (length(which(names(l_id_status) %in% as.vector(o_input_status$val$id))) > 0) {o_input_status$val <- f_update_input_status_data(l_id_status, o_input_status$val, o_special_input$val)}
			eval(parse(text = paste0("l_selectize_option <- list(sub_var_name = input$vname, concat = c(", paste(paste0("o_cond$concat", 1:2), collapse = ", "), "), max_item = o_selectize_input$max_item, sub_data = o_click_button$create, update = o_cond$update)")))
			eval(parse(text = paste0("s_cmd <- f_on_off_inputs(l_id_status, ", ifelse("all" %in% ls(e_data), "e_data$all", "NULL"), ", l_selectize_option, o_special_input$val)")))
			eval(parse(text = s_cmd))
			o_selectize_input$max_item <- list()
			o_special_input$val <- list()
		}
	})
	
	# 1.4. Add events on o_cond$update reactive value
	# ===============================================
	# Update UI's inputs in the variable selection section of the left panel
	
	observeEvent(o_cond$update, {
		if (o_cond$update == 1) {
			l_id_value <- list()
			l_id_status <- f_update_input_status_list(f_create_input_status_list(c(as.vector(o_input_status$display$id), o_on_off$tp$id), c(as.vector(o_input_status$display$status), o_on_off$tp$status)), o_input_status$val)
			if (!is.null(o_on_off$tp$id)) {eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- NULL"), collapse = "; ")))} # reset o_on_off$tp reactive value
			v_id <- as.vector(o_input_status$display$id)
			v_pos <- which(o_input_status$display$id %in% c(paste0(c("f", "g", "h"), "_text"), paste0("var_", c("id", "group")), "concat2", "ref", paste0("wres_", c("cbox", "group", "vfun"))) & o_input_status$display$status == 0)
			if (length(v_pos) > 0) {v_id <- v_id[-v_pos]}
			df_match_id <- f_create_match_id_data(o_parameter$concat1, o_parameter$concat2)
			
			# add selectize input id to be updated 
			v_id_new <- c(paste0("var_", c("x", "y", "z")), "date_format", ifelse(c(paste0("var_", c("id", "group")), "ref", "wres_group") %in% v_id, c(paste0("var_", c("id", "group")), "ref", "wres_group"), NA))
			v_id_new <- v_id_new[!is.na(v_id_new)]
			eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("length(which(!is.na(isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new, "\", \"parameter\"])]]))))"), collapse = ", "), ") > 0)")))
			
			if (length(v_pos) > 0) {
				v_id_new <- v_id_new[v_pos]
				eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("length(input$", v_id_new, ") != length(isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new, "\", \"parameter\"])]])) | length(which(!input$", v_id_new, " %in% isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new, "\", \"parameter\"])]]))) > 0"), collapse = ", "), "))")))
				if (length(v_pos) > 0) {eval(parse(text = paste(paste0("l_id_value$", v_id_new[v_pos], " <- isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new[v_pos], "\", \"parameter\"])]])"), collapse = "; ")))}
			}
			
			# add text field input id to be updated
			v_pos <- which(v_id %in% c(paste0(c("f", "g", "h"), "_text"), "wres_vfun"))
			
			if (length(v_pos) > 0) {
				v_id_new <- v_id[v_pos]
				eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("input$", v_id_new, " != isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new, "\", \"parameter\"])]])"), collapse = ", "), "))")))
				if (length(v_pos) > 0) {eval(parse(text = paste(paste0("l_id_value$", v_id_new[v_pos], " <- isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", v_id_new[v_pos], "\", \"parameter\"])]])"), collapse = "; ")))}
			}
			
			# add radio button input id to be updated
			v_id_new <- ifelse(c(paste0(c("f", "g", "h"), "_text"), "ref", "wres_vfun", paste0("var_", c("id", "group"))) %in% as.vector(o_input_status$display$id), c(paste0(c("f", "g", "h", "ref", "wres"), "_radio"), c("id", "group")), NA)
			v_pos <- which(is.na(v_id_new))
			
			if (length(v_pos) < length(v_id_new)) {
				v_id_new <- v_id_new[-v_pos]
				eval(parse(text = paste0("v_cond <- c(", paste(paste0("length(which(!is.na(isolate(o_parameter[[as.vector(df_match_id[df_match_id$input == \"", c(paste0(c("f", "g", "h"), "_text"), "ref", "wres_vfun", paste0("var_", c("id", "group")))[-v_pos], "\", \"parameter\"])]]))))"), collapse = ", "), ") > 0")))
				v_value <- ifelse(v_cond, "yes", "no")
				eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("input$", v_id_new, " != \"", v_value, "\""), collapse = ", "), "))")))
				if (length(v_pos) > 0) {eval(parse(text = paste(paste0("l_id_value$", v_id_new[v_pos], " <- \"", v_value[v_pos], "\""), collapse = "; ")))}
			}
			
			# add check box input id to be updated
			v_id_new <- c(ifelse(input$plot_type %in% c("boxplot", "barplot"), "concat1", NA), ifelse(c("concat2", "wres_vfun") %in% v_id, c("concat2", "wres_cbox"), NA))
			v_pos <- which(is.na(v_id_new))
			
			if (length(v_pos) < length(v_id_new)) {
				v_id_new <- v_id_new[-v_pos]
				v_value <- c(o_parameter$concat1, o_parameter$concat2, ifelse(length(which(!is.na(o_parameter$wres_group))) > 0, T, F))[-v_pos]
				eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("input$", v_id_new, " != ", v_value), collapse = ", "), "))")))
				if (length(v_pos) > 0) {eval(parse(text = paste(paste0("l_id_value$", v_id_new[v_pos], " <- ", v_value[v_pos]), collapse = "; ")))}
			}
			
			if (o_cond$save1 == 1 & input$data_type %in% c("normal", "ir")) {l_id_value <- c(l_id_value, list("flag" = T))} # update flag input (= T) when a new flag data is created
			o_on_off$val <- l_id_status # execute enable/disable commands
			
			if (length(l_id_value) > 0) {
				if ("concat1" %in% names(l_id_value)) {o_selectize_input$max_item_current$var_x <- ifelse(l_id_value$concat1, 9999, 1)}
				
				if (input$data_type == "normal" & input$model == "none" & !input$plot_type %in% c("corplot", "barplot")) {
					v_id <- as.vector(o_input_status$display[o_input_status$display$id %in% paste0(c("f", "g", "h"), "_radio"), "id"]) 
					o_input_status$val[which(o_input_status$val$id %in% v_id), "special"] <- as.vector(o_input_status$display[which(o_input_status$display$id %in% v_id), "special"])
				}
				
				v_pos <- which(names(l_id_value) %in% c("id", paste0(c("ref", "f", "g", "h", "wres"), "_radio"), "wres_cbox", "group", paste0("concat", 1:2), paste0("var_", c("x", "y", "z"))))
				
				if (length(v_pos) == 0) {
					o_cond$update <- 0
				}
				else {
					o_update$val <- names(l_id_value)[max(v_pos)]
				}
				
				s_cmd <- f_update_inputs(l_id_value, e_data$all, list(concat = c(o_cond$concat1, o_cond$concat2), sub_data = o_click_button$create), ifelse(!is.null(l_id_status), 100, 0))
				
				if (o_cond$save1 == 1 & input$data_type %in% c("normal", "ir")) { # disable id/var_id input when a new flag is created
					s_cmd <- paste0(s_cmd, "; ", ifelse("id" %in% names(l_id_value), paste0("shinyjs::delay(", ifelse(!is.null(l_id_status), 200, 100), ", disable(\"id\"))"), "shinyjs::disable(\"id\")"))
					
					if(!is.na(o_parameter$id)) {
						s_cmd <- paste0(s_cmd, "; ", ifelse("var_id" %in% names(l_id_value), paste0("shinyjs::delay(", ifelse(!is.null(l_id_status), 200, 100), ", disable(\"var_id\"))"), "shinyjs::disable(\"var_id\")"))
						o_input_status$val[which(o_input_status$id %in% c("id", "var_id")), c("status", "special")] <- matrix(data = 1, ncol = 2, nrow = 2)
					}
					else {
						o_input_status$val[which(o_input_status$id == "id"), "status"] <- 0
					}
					
					o_input_status$display <- o_input_status$display[which(o_input_status$display$id != "var_id"),]
				}
				
				eval(parse(text = s_cmd)) # execute update commands
			}
			else {
				o_cond$update <- 0
			}
		}
	})
	
	# 1.5. Add events of reset1 button  
	# ================================
	
	observeEvent(input$reset1_button, {
		df_rv_id_value <- f_create_rv_inventory()
		
		if (o_reset$subdata == 1) {
			if (length(isolate(o_sdata_cond$var_name)) > 0) {
				eval(parse(text = paste(paste0("o_sdata_cond$", c("var_name", "var_type", "var_rel", "value") , " <- ", c(rep("c()", 3), "list()")), collapse = "; ")))
				o_sdata <- NULL
				if (isolate(o_cond$create) == 1) {o_cond$create <- 0}
			}
			
			if (isolate(o_click_button$create) == 1) {
				eval(parse(text = f_update_rv(list("rv" = c("o_click_button", rep("o_sdata_cond", 2)), "id" = c("create", "row_num", "formula"), "value" = c("0", "c()", "NA")))))
				if ("sub" %in% ls(e_data)) {rm(list = "sub", envir = e_data)}
			}
			
			o_reset$subdata <- 0
		}
		
		if (o_reset$concat == 1) {
			o_reset$concat <- 0
			s_name <- ifelse("sub" %in% ls(e_data), "sub", "all")
			if (isolate(o_cond$concat1) == 0 & ".concat1." %in% names(e_data[[s_name]])) {e_data[[s_name]] <- e_data[[s_name]][, -which(names(e_data[[s_name]]) == ".concat1.")]}
			if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data[[s_name]])) {e_data[[s_name]] <- e_data[[s_name]][, -which(names(e_data[[s_name]]) == ".concat2.")]}
		}
		
		if (o_reset$label == 1) {eval(parse(text = f_update_rv(list("rv" = c("o_reset", rep("o_label_text", 5)), "id" = c(rep("label", 2), "text", "label_temp", "text_temp", "del"), "value" = c("0", rep("c()", 4), "list(\"x\" = 0, \"y\" = 0, \"z\" = 0)")))))} # delete information on custom labels
		if (o_reset$color_opacity == 1) {eval(parse(text = f_update_rv(list("rv" = c("o_reset", rep("o_name_option", 6)), "id" = c("color_opacity", "name", "color", "color_temp", "opacity", "opacity_temp", "var"), "value" = c("0", rep("c()", 6))))))} # delete information on custom colors/opacities
		if (o_reset$point_type_size == 1) {eval(parse(text = f_update_rv(list("rv" = c("o_reset", rep("o_name_option", 4)), "id" = c("point_type_size", "point_type", "point_type_temp", "point_size", "point_size_temp"), "value" = c("0", rep("c()", 4))))))} # delete information on point characteristics (type/size)
		
		if (o_click_button$display == 1) {
			shinyjs::disable("graph_clear_button")
			eval(parse(text = f_update_rv(f_create_rv_id_value_list(df_rv_id_value, "display"))))
			js$resetClick_leg()
			
			# reset picture information inputs
			if (input$picture_name != "Picture_name") {updateTextInput(session, "picture_name", value = "Picture_name")}
			if (input$picture_height != 800) {updateNumericInput(session, "picture_height", value = 800)}
			if (input$picture_width != 1000) {updateNumericInput(session, "picture_width", value = 1000)}
			if (input$picture_format != "png") {updateSelectizeInput(session, "picture_format", selected = "png")}
			
			# reset linear regression information inputs
			if (input$lreg_info_xpos != 0) {updateRadioButtons(session, "lreg_info_xpos", selected = "left")}
			if (input$lreg_info_ypos != 1) {updateRadioButtons(session, "lreg_info_ypos", selected = "top")}
			updateSelectizeInput(session, "lreg_info_elt", choices = " ")
			shinyjs::disable("reset3_button")
			
			if (input$select_graph != " ") {
				eval(parse(text = f_update_rv(list("rv" = c("o_parameter", "o_cond"), "id" = paste0("select_graph", c("", "1")), "value" = c("NA", "0")))))
				updateSelectizeInput(session, "select_graph", choices = " ")
				shinyjs::hide(id = "tmainpanel")
				shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 5, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 7, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 6, 8)))))
				shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 1, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 3, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 2, 4)))))
			}
			
			if (length(ls(e_previous_flag)) > 0) {rm(list = ls(e_previous_flag), envir = e_previous_flag)}		
			
			if (length(ls(e_current_flag)) > 0) {
				rm(list = ls(e_current_flag), envir = e_current_flag)
				eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
				js$resetClick()
			}
			
			output$graphic <<- NULL
			shinyjs::delay(100, hide(id = "graphic"))
			shinyjs::delay(100, hide(id = "sh_popup1"))
			shinyjs::delay(100, hide(id = "sh_popup2"))
			shinyjs::delay(100, hide(id = "sh_reset"))
		}
		
		if (o_reset$code == 1) {
			if (!"all" %in% ls(e_data)) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = paste0("concat", 1:2), "value" = rep("0", 2)))))}
			eval(parse(text = f_update_rv(f_create_rv_id_value_list(df_rv_id_value, "lp_s1_s2"))))
		}
	})
	
	# 1.6. Add event on o_cond$flag_msg reactive value
	# =================================================
	# Warning message corresponding to flag tab inputs desabling
	
	observeEvent(o_cond$flag_msg, { 
		if (o_cond$flag_msg == 1) {showNotification("The flag tab is disabled because a function (f, g, h) is added or a statistical method is selected (Statistics tab)", duration = 15, type = "warning")}
	})
	
	# 1.7. Automatic commands used to enable/disable buttons  
	# ======================================================
	
	observe({
		shinyjs::toggleState("del1_button", length(which(!is.na(input$vvalue1))) > 0 & input$vtype == "qualit" & input$subdata_option)
		shinyjs::toggleState("del2_button", length(which(!is.na(input$ref))) > 0 & input$ref_radio == "yes")
		shinyjs::toggleState("del3_button", length(which(!is.na(input$var_x))) > 0 & o_click_button$load1 == 1 & input$data_type == "normal" & (input$plot_type %in% c("plot", "histplot") | input$concat1))
		shinyjs::toggleState("del4_button", length(which(!is.na(input$var_y))) > 0 & o_click_button$load1 == 1 & input$plot_type %in% c("plot", "boxplot", "corplot"))
		shinyjs::toggleState("del5_button", length(which(!is.na(input$var_z))) > 0 & o_click_button$load1 == 1 & input$dim_num == "3d")
		shinyjs::toggleState("del6_button", length(which(!is.na(input$wres_group))) > 0 & input$wres_cbox)
		shinyjs::toggleState("del7_button", length(which(!is.na(input$var_group))) > 0 && input$concat2)
		shinyjs::toggleState("expand2_button", input$f_radio == "yes")
		shinyjs::toggleState("expand3_button", input$g_radio == "yes")
		shinyjs::toggleState("expand4_button", input$h_radio == "yes")
		shinyjs::toggleState("expand5_button", input$wres_radio == "yes")
		shinyjs::toggleState("c_clear_button", length(input$condInfo_rows_selected) > 0)
		shinyjs::toggleState("c_deselect_all_button", length(input$condInfo_rows_selected) > 0)
		shinyjs::toggleState("c_select_all_button", length(input$condInfo_rows_selected) < length(isolate(input$condInfo_rows_all)))
		shinyjs::toggleState("option_clear_button", length(input$option_inventory_rows_selected) > 0)
		shinyjs::toggleState("option_add_button", length(input$option_inventory_rows_selected) > 0 & ((input$edit_option == "label" & length(which(input$custom_label == "")) == 0 & length(input$custom_label) > 0) | (input$edit_option == "color/opacity" & length(which(input$color_opacity == "color")) > 0 & length(which(input$custom_color == "")) == 0 & length(input$custom_color) > 0) | (input$edit_option == "color/opacity" & length(which(input$color_opacity == "opacity")) > 0 & length(which(!is.na(input$custom_opacity))) > 0) | (input$edit_option == "point type/size" & length(which(input$point_type_size == "type")) > 0 & length(which(input$custom_point_type != "")) > 0) | (input$edit_option == "point type/size" & length(which(input$point_type_size == "size")) > 0 & length(which(!is.na(input$custom_point_size))) > 0)))
		shinyjs::toggleState("option_deselect_all_button", length(input$option_inventory_rows_selected) > 0)
		shinyjs::toggleState("option_select_all_button", length(input$option_inventory_rows_selected) < length(isolate(input$option_inventory_rows_all)))
	})
	
	# ==========
	# Left panel 
	# ==========
	
	# -------------------
	# Data type selection
	# -------------------
	
	# 2.1. Add events of data type input
	# ==================================
	
	observeEvent(input$data_type, {
		eval(parse(text = f_update_rv(list("rv" = c(rep("o_path", 3), rep("o_data_info", 2)), "id" = c(paste0("name", 1:2, "_prev"), "name2", "mtime", "code"), "value" = c(rep("NA", 3), "rep(NA, 3)", "NA")))))
		
		if (isolate(o_click_button$load1) == 1 | isolate(o_click_button$load2) == 1) {
			rm(list = ls(e_data), envir = e_data)
			js$backgroundCol("data_path1", "white")
			eval(parse(text = f_update_rv(list("rv" = c("o_path", "o_flag", "o_load_error", rep("o_click_button", 3)), "id" = c("color1", "name", "code", paste0("load", 1:2), "browse2"), "value" = c("\"white\"", "NA", "rep(0, 3)", rep("0", 3))))))
			
			if (isolate(o_path$color3) != "transparent") {
				js$backgroundCol("data_path3", "transparent")
				eval(parse(text = f_update_rv(list("rv" = c(rep("o_path", 3), rep("o_click_button", 2)), "id" = c("name3_prev", "name3", "color3", "load3", "browse3"), "value" = c(rep("NA", 2), "\"transparent\"", rep("0", 2))))))
			}
			
			if (input$subdata_option) {o_reset$subdata <- 1}
			eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
			click("reset1_button")
		}
		
		if (input$data_type != "ir") {
			js$backgroundCol("data_path2", "transparent")
			o_path$color2 <- "transparent"
		}
		else {
			js$backgroundCol("data_path2", "white")
			o_path$color2 <- "white"
		}
		
		v_cond <- as.vector((o_input_status$val$id == "load1_button" & !is.null(input$data_path1) & input$data_path1 != "") | (o_input_status$val$id %in% c("browse2_button", "data_path2") & input$data_type == "ir"))
		o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val$id), ifelse(v_cond, 1, 0)), o_input_status$val)
	})
	
	# ------------
	# Data loading
	# ------------
	
	# 2.2. Add events of flag checkbox
	# ================================
	
	observeEvent(input$flag, {
		if (isolate(o_click_button$load1) == 1 & isolate(o_cond$save1) == 0) {
			js$backgroundCol("data_path1", "white")
			eval(parse(text = f_update_rv(list("rv" = c("o_flag", "o_load_error", rep("o_path", 2), "o_click_button", rep("o_data_info", 2)), "id" = c("name", "code[1]", "name1_prev", "color1", "load1", "mtime[1]", "code"), "value" = c("NA", "0", "NA", "\"white\"", "0", rep("NA", 2))))))
			
			if (isolate(o_path$color3) != "transparent") {
				js$backgroundCol("data_path3", "transparent")
				eval(parse(text = f_update_rv(list("rv" = c(rep("o_path", 3), rep("o_click_button", 2), "o_data_info"), "id" = c("name3_prev", "name3", "color3", "load3", "browse3", "mtime[3]"), "value" = c(rep("NA", 2), "\"transparent\"", rep("0", 2), "NA")))))
			}
			
			if ("all" %in% ls(e_data)) {rm(list = "all", envir = e_data)}
			if ("flag" %in% ls(e_data)) {rm(list = "flag", envir = e_data)}
			if (input$subdata_option) {o_reset$subdata <- 1}
			eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
			click("reset1_button")
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[-which(o_input_status$val$panel == "lp_s1"), "id"]), 0), o_input_status$val)
		}
		else {
			if (isolate(o_cond$save1) == 1) {o_cond$save1 <- 0}
		}
	})
	
	# 2.3. Add events on data path inputs 
	# ===================================
	
	# data_path1
	
	observeEvent(input$data_path1, {
		if (!is.na(isolate(o_path$name1))) {o_path$name1_prev <- isolate(o_path$name1)}
		
		if (!is.na(isolate(o_path$name1_prev))) {
			if (isolate(o_click_button$browse1) == 1 & input$data_path1 == "") {
				updateTextInput(session, "data_path1", value = isolate(o_path$name1))
			}
			else {
				o_path$name1 <- input$data_path1
			}
		}
		else {
			if (input$data_path1 != "") {o_path$name1 <- input$data_path1}
		}
		
		if (isolate(o_click_button$browse1) == 1) {o_click_button$browse1 <- 0}
		v_pos <- c()
		
		if (isolate(o_click_button$load1) == 1) {
			if (length(unique(c(isolate(o_path$name1_prev), isolate(o_path$name1)))) == 1 & !is.na(isolate(o_path$name1_prev))) {
				js$backgroundCol("data_path1", isolate(o_path$color1))
			}
			else {
				js$backgroundCol("data_path1", "white")
				eval(parse(text = f_update_rv(list("rv" = c("o_flag", "o_load_error", "o_path", "o_click_button", rep("o_data_info", 2)), "id" = c("name", "code[1]", "color1", "load1", "mtime[1]", "code"), "value" = c("NA", "0", "\"white\"", "0", rep("NA", 2))))))
				
				if (isolate(o_path$color3) != "transparent") {
					js$backgroundCol("data_path3", "transparent")
					eval(parse(text = f_update_rv(list("rv" = c(rep("o_path", 3), rep("o_click_button", 2), "o_data_info"), "id" = c("name3_prev", "name3", "color3", "load3", "browse3", "mtime[3]"), "value" = c(rep("NA", 2), "\"transparent\"", rep("0", 2), "NA")))))
				}
				
				if ("all" %in% ls(e_data)) {rm(list = "all", envir = e_data)}
				if ("flag" %in% ls(e_data)) {rm(list = "flag", envir = e_data)}
				if (input$subdata_option) {o_reset$subdata <- 1}
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
				v_pos <- which(o_input_status$val$panel %in% c(paste0("lp_s", 2:5), paste0("tp_t", 1:3)))
			}
		}
		
		v_pos <- sort(c(v_pos, which(o_input_status$val$id == "load1_button")))
		o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[v_pos, "id"]), ifelse(o_input_status$val[v_pos, "id"] == "load1_button" & !is.null(input$data_path1) & input$data_path1 != "", 1, 0)), o_input_status$val)
	})
	
	# data_path2
	
	observeEvent(input$data_path2, {
		if (!is.na(isolate(o_path$name2))) {o_path$name2_prev <- isolate(o_path$name2)}
		
		if (!is.na(isolate(o_path$name2_prev))) {
			if (isolate(o_click_button$browse2) == 1 & input$data_path2 == "") {
				updateTextInput(session, "data_path2", value = isolate(o_path$name2))
			}
			else {
				o_path$name2 <- input$data_path2
			}
		}
		else {
			if (input$data_path2 != "") {o_path$name2 <- input$data_path2}
		}
		
		if (isolate(o_click_button$browse2) == 1) {o_click_button$browse2 <- 0}
		v_pos <- c()
		
		if (isolate(o_click_button$load2) == 1) {
			if (length(unique(c(isolate(o_path$name2_prev), isolate(o_path$name2)))) == 1 & !is.na(isolate(o_path$name2_prev))) {
				js$backgroundCol("data_path2", isolate(o_path$color2))
			}
			else {
				js$backgroundCol("data_path2", "white")
				eval(parse(text = f_update_rv(list("rv" = c("o_load_error", "o_path", "o_click_button", "o_data_info"), "id" = c("code[2]", "color2", "load2", "mtime[2]"), "value" = c("0", "\"white\"", "0", "NA")))))
				if ("code_freq" %in% ls(e_data)) {rm(list = "code_freq", envir = e_data)}
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
				v_pos <- which(o_input_status$val$panel %in% c(paste0("lp_s", 2:5), paste0("tp_t", 1:3)))
			}
		}
		
		v_pos <- sort(c(v_pos, which(o_input_status$val$id == "load2_button")))
		o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[v_pos, "id"]), ifelse(o_input_status$val[v_pos, "id"] == "load2_button" & !is.null(input$data_path2) & input$data_path2 != "", 1, 0)), o_input_status$val)
	})
	
	# 2.4. Add events of browse buttons
	# =================================
	
	# browse1 button
	
	observeEvent(input$browse1_button, {
		o_click_button$browse1 <- 1
		o_volumes <- getVolumes()()
		shinyFileChoose(input, 'browse1_button', roots = o_volumes, filetypes = c('txt', 'csv'))
		s_data_path <- as.character(parseFilePaths(o_volumes, input$browse1_button)$datapath)
		updateTextInput(session, "data_path1", value = s_data_path)
    })
	
	# browse2 button
	
	observeEvent(input$browse2_button, {
		o_click_button$browse2 <- 1
		o_volumes <- getVolumes()()
		shinyFileChoose(input, 'browse2_button', roots = o_volumes, filetypes = c('txt', 'csv'))
		s_data_path <- as.character(parseFilePaths(o_volumes, input$browse2_button)$datapath) 
		updateTextInput(session, "data_path2", value = s_data_path)
    })
	
	# 2.5. Add events of load buttons 
	# ===============================
	
	# load1 button
	
	observeEvent(input$load1_button, {
		b_cond <- F
		
		if (isolate(o_path$color1) == "lightgreen") {
			n_time <- as.numeric(file.info(input$data_path1)$mtime)
			b_cond <- ifelse(n_time == isolate(o_data_info$mtime)[1], F, T) 
		}
		
		if ((length(unique(c(isolate(o_path$name1_prev), isolate(o_path$name1)))) > 1 & !is.na(isolate(o_path$name1_prev))) | isolate(o_click_button$load1) == 0 | isolate(o_load_error$code)[1] == 1 | b_cond == T) {
			if (b_cond) {
				if ("all" %in% ls(e_data)) {rm(list = "all", envir = e_data)}
				if ("flag" %in% ls(e_data)) {rm(list = "flag", envir = e_data)}
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[-which(o_input_status$val$panel == "lp_s1"), "id"]), 0), o_input_status$val)
			}
			
			o_click_button$load1 <- 1
			l_results <- f_load_data("load1_button", input$data_type, input$data_path1, input$flag)
			
			if (!is.na(l_results[[5]])) { # error message
				f_showNotification(l_results[[5]], duration = 15, type = "error")
				js$backgroundCol("data_path1", "salmon")
				o_path$color1 <- "salmon"
				o_load_error$code[1] <- 1
				o_data_info$mtime[1] <- NA
				if (input$data_type == "ir") {o_data_info$code <- NA}
			}
			else {
				if (!is.na(l_results[[6]])) {f_showNotification(l_results[[6]], duration = 15, type = "warning")} # warning message
				e_data$all <- l_results[[1]]
				if (input$data_type != "ir") {o_selectize_input$max_item <- f_create_input_maxitem_list(input$data_type)}
				v_id <- c(ifelse(input$data_type == "normal", "subdata_option", NA), f_create_input_id_vector(s_data = input$data_type, b_load2 = ifelse("code_freq" %in% ls(e_data), T, F)))
				v_id <- v_id[!is.na(v_id)]
				
				if (input$flag) {
					e_data$flag <- l_results[[2]]
					o_flag$name <- l_results[[3]]
					
					if (l_results[[7]]) { # add a special enabling to the id radio button input
						o_special_input$val <- list("id" = "yes")
						if (input$data_type != "temporal") {v_id <- c(v_id, "id")}
					}
				}
				else {
					if (input$data_type != "temporal") {v_id <- c(v_id, "id")}
				}
				
				eval(parse(text = paste0("v_pos <- which(", ifelse(input$data_type == "normal", "o_input_status$val$panel == \"lp_s3\" |", ""), "o_input_status$val$id %in% v_id)")))
				v_id <- as.vector(o_input_status$val[v_pos, "id"])
				o_on_off$val <- f_create_input_status_list(v_id, 1)
				if (input$data_type == "ir") {o_data_info$code <- l_results[[4]]}
				js$backgroundCol("data_path1", "lightgreen")
				o_path$color1 <- "lightgreen"
				o_load_error$code[1] <- 0
				o_data_info$mtime[1] <- as.numeric(file.info(input$data_path1)$mtime)
				o_option$choices <- c("label", "color", "pch")
			}
		}
	})
	
	# load2 button (only IR data)
	
	observeEvent(input$load2_button, {
		b_cond <- F 
		
		if (isolate(o_path$color2) == "lightgreen") {
			n_time <- as.numeric(file.info(input$data_path2)$mtime)
			b_cond <- ifelse(n_time == isolate(o_data_info$mtime)[2], F, T)
		}
		
		if ((length(unique(c(isolate(o_path$name2_prev), isolate(o_path$name2)))) > 1 & !is.na(isolate(o_path$name2_prev))) | isolate(o_click_button$load2) == 0 | isolate(o_load_error$code)[2] == 1 | b_cond) {
			if (b_cond) {
				if ("code_freq" %in% ls(e_data)) {rm(list = "code_freq", envir = e_data)}
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[-which(o_input_status$val$panel == "lp_s1"), "id"]), 0), o_input_status$val)
			}
			
			o_click_button$load2 <- 1
			l_results <- f_load_data("load2_button", "ir", input$data_path2)
			
			if (!is.na(l_results[[2]])) { # error message
				f_showNotification(l_results[[2]], duration = 15, type = "error")
				js$backgroundCol("data_path2", "salmon")
				o_path$color2 <- "salmon"
				o_load_error$code[2] <- 1
				o_data_info$mtime[2] <- NA
			}
			else {
				e_data$code_freq <- l_results[[1]]
				if ("all" %in% ls(e_data)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[which(o_input_status$val$id %in% f_create_input_id_vector(s_data = "ir", b_load2 = T)), "id"]), 1), o_input_status$val)}
				js$backgroundCol("data_path2", "lightgreen")
				o_path$color2 <- "lightgreen"
				o_load_error$code[2] <- 0
				o_data_info$mtime[2] <- as.numeric(file.info(input$data_path2)$mtime)
			}
		}
	})
	
	# -----------------
	# Sub-data creation
	# -----------------
	
	# 2.6. Add events of condition checkbox
	# =====================================
	
	observeEvent(input$subdata_option, {
		if ("all" %in% ls(e_data)) {
			o_cond$sdata[1] <- 1
			v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 2:5), paste0("tp_t", 1:3))), "id"])
			v_id_all <- v_id_all[-which(v_id_all == "subdata_option")]
			v_id_on <- c()
			
			if (input$subdata_option) {
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("concat", "code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				v_id_on <- c(v_id_on, "vname", "rel_symbol", "c_add_button")
				
				if (is.numeric(as.vector(e_data$all[, 1])) & length(grep("[.]", as.vector(e_data$all[, 1]))) == 0) {
					v_id_on <- c(v_id_on, "vtype", "vvalue1")
				}
				else {
					if (is.numeric(as.vector(e_data$all[, 1]))) {
						o_special_input$val <- list("vtype" = "quant")
						o_cond$sdata[2] <- 1
						v_id_on <- c(v_id_on, "vvalue2")
					}
					else {
						v_id_on <- c(v_id_on, "vvalue1")
					}
				}
			}
			else {
				o_reset$subdata <- 1
				
				if (input$vtype == "quant") {
					shinyjs::show(id = "vvalue1")
					o_cond$sdata[2] <- 1
				}
				
				if (isolate(o_click_button$create) == 1) {
					if (length(o_name_option$name) > 0 & length(o_name_option$var) > 0) { # update graph options (color, opacity, point type/size) for normal/ir data type
						eval(parse(text = paste0("v_var_name <- ", ifelse(input$plot_type %in% c("boxplot", "barplot"), "input$var_x", ifelse(input$group == "no", "NA", "input$var_group")))))
						l_results <- f_update_graph_option_rv(o_name_option, input$edit_option, o_click_button$display, input$data_type, input$plot_type, v_var_name, e_data$all, F, T)
						if (length(l_results[[1]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", names(l_results[[1]]), " <- l_results[[1]][[", 1:length(l_results[[1]]), "]]"), collapse = "; ")))}
						if (length(l_results[[2]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", l_results[[2]], " <- c()"), collapse = "; ")))}
					}
					
					if (input$model %in% c("calib", "valid") & isolate(o_path$color3) == "lightgreen") {
						v_id_all <- v_id_all[-which(v_id_all %in% as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:4), paste0("tp_t", c(1, 3)))), "id"]))]
						js$backgroundCol("data_path3", "white")
						eval(parse(text = f_update_rv(list("rv" = c("o_path", "o_load_error", "o_data_info", "o_click_button"), "id" = c("color3", "code[3]", "mtime[3]", "load3"), "value" = c("\"white\"", "0", "NA", "0")))))
						rm(list = "m_param", envir = e_data)
					}
					else {
						v_id_all <- v_id_all[-which(v_id_all %in% as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:5), paste0("tp_t", c(1, 3)))), "id"]))]
					}
				}
				else {
					if ("flag" %in% ls(e_data)) {
						if (names(e_data$flag)[1] %in% names(e_data$all)) {o_special_input$val <- list("id" = "yes")}
					}
					
					if (input$data_type == "normal") {
						o_selectize_input$max_item <- f_create_input_maxitem_list("normal")
						v_id_on <- c(v_id_on, as.vector(o_input_status$val[which(o_input_status$val$panel == "lp_s3"), "id"]), ifelse("flag" %in% ls(e_data), ifelse(names(e_data$flag)[1] %in% names(e_data$all), "id", NA), "id"), f_create_input_id_vector("normal"))
					}
					else { # ir
						v_id_on <- c(v_id_on, ifelse("flag" %in% ls(e_data), ifelse(names(e_data$flag)[1] %in% names(e_data$all), "id", NA), "id"), f_create_input_id_vector(s_data = "ir", b_load2 = T))
					}
					
					v_id_on <- v_id_on[!is.na(v_id_on)]
					o_option$choices <- c("label", "color", "pch")
				}
			}
			
			click("reset1_button")
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val)
		}
	})
	
	# 2.7. Add events of variable name selectize input
	# ================================================
	
	observeEvent(input$vname, {
		if ("all" %in% ls(e_data)) {
			if (o_cond$sdata[1] == 0) {
				if (is.numeric(as.vector(e_data$all[, input$vname])) & length(grep("[.]", as.vector(e_data$all[, input$vname]))) == 0) {
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list("vtype", 1), o_input_status$val)
					
					if (input$vtype == "quant") {
						updateTextInput(session, "vvalue2", value = character(0))
					}
					else {
						updateSelectizeInput(session, "vvalue1", choices = f_create_input_value_list("selectize", e_data$all, c(o_cond$concat1, o_cond$concat2), o_click_button$create, 2, input$vname))
					}
				}
				else {
					if (is.numeric(as.vector(e_data$all[, input$vname]))) {
						if (input$vtype == "quant") {
							if (o_input_status$val[which(o_input_status$val$id == "vtype"), "special"] == 0) {
								o_special_input$val <- list("vtype" = "quant")
								o_on_off$val <- f_update_input_status_list(f_create_input_status_list("vtype", 0), o_input_status$val)
							}
							
							updateTextInput(session, "vvalue2", value = character(0))
						}
						else {
							o_special_input$val <- list("vtype" = "quant")
							o_cond$sdata[2] <- 1
							updateSelectizeInput(session, "rel_symbol", choices = c("=", "!=", "<", ">", "<=", ">="))
							v_id <- as.vector(o_input_status$val[which(o_input_status$val$id %in% c("vtype", paste0("vvalue", 1:2))), "id"])
							o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, ifelse(v_id %in% c("vtype", "vvalue1"), 0, 1)), o_input_status$val)
						}
					}
					else {
						if (input$vtype == "quant") {
							o_cond$sdata[2] <- 1
							updateSelectizeInput(session, "rel_symbol", choices = c("%in%", "!%in%"))
							v_id <- as.vector(o_input_status$val[which(o_input_status$val$id %in% c("vtype", paste0("vvalue", 1:2))), "id"])
							o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, ifelse(v_id %in% c("vtype", "vvalue2"), 0, 1)), o_input_status$val)
						}
						else {
							updateSelectizeInput(session, "vvalue1", choices = f_create_input_value_list("selectize", e_data$all, c(o_cond$concat1, o_cond$concat2), o_click_button$create, 2, input$vname))
							o_on_off$val <- f_update_input_status_list(f_create_input_status_list("vtype", 0), o_input_status$val)
						}
					}
				}
			}
			else {
				o_cond$sdata[1] <- 0
			}
		}
	})
	
	# 2.8. Add events of variable type radio button
	# =============================================
	
	observeEvent(input$vtype, {
		if ("all" %in% ls(e_data)) {
			if (input$subdata_option) {
				if (input$vtype == "quant") {
					if (o_cond$sdata[2] == 0) {
						updateSelectizeInput(session, "rel_symbol", choices = c("=", "!=", "<", ">", "<=", ">="))
						o_on_off$val <- f_update_input_status_list(f_create_input_status_list(paste0("vvalue", 1:2), c(0, 1)), o_input_status$val)
					}
					else {
						o_cond$sdata[2] <- 0
					}
					
					shinyjs::show("vvalue2")
				}
				else {
					if (o_cond$sdata[2] == 0) {
						updateSelectizeInput(session, "rel_symbol", choices = c("%in%", "!%in%"))
						o_on_off$val <- f_update_input_status_list(f_create_input_status_list(paste0("vvalue", 1:2), c(1, 0)), o_input_status$val)
					}
					else {
						o_cond$sdata[2] <- 0
					}
					
					shinyjs::show("vvalue1")
				}
			}
		}
	})
	
	# 2.9. Add events of info/clear button (creating a modal dialog) 
	# ==================================== 
	
	output$condInfo <- DT::renderDataTable(o_sdata(), server = F)
	condInfo_proxy <- DT::dataTableProxy("condInfo")
	
	observeEvent(input$c_info_clear_button, {
		if ("all" %in% ls(e_data)) {
			o_sdata(f_create_cond_data(o_sdata_cond))
			
			showModal(modalDialog(
				title = "Conditions added",
				easyClose = F,
				size = "l",
				DT::dataTableOutput("condInfo"),
				footer = tagList(disabled(actionButton("c_deselect_all_button", "Deselect all")), actionButton("c_select_all_button", "Select all"), disabled(actionButton("c_clear_button", "Clear")), actionButton("c_close_button", "Close"))
			))
		}
	})
	
	observeEvent(input$c_select_all_button, {DT::selectRows(condInfo_proxy, input$condInfo_rows_all)})
	observeEvent(input$c_deselect_all_button, {DT::selectRows(condInfo_proxy, NULL)})
	
	observeEvent(input$c_close_button, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			o_sdata <- NULL
		}
	})
	
	# 2.10. Add events of clear button (available from info/clear modal dialog)
	# ================================
	
	observeEvent(input$c_clear_button, {
		if ("all" %in% ls(e_data)) {
			v_row <- input$condInfo_rows_selected
			b_cond <- length(v_row) == length(isolate(o_sdata_cond$var_name))
			eval(parse(text = paste(paste0("o_sdata_cond$", c("var_name", "var_type", "var_rel", "value"), " <- isolate(o_sdata_cond$", c("var_name", "var_type", "var_rel", "value"), ")[-v_row]"), collapse = "; ")))
			
			if (b_cond) {
				v_id <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:5), paste0("tp_t", 1:3)) | o_input_status$val$id %in% c("c_info_clear_button", "expand1_button", "c_formula", "create_button")), "id"])
				o_sdata <- NULL
				removeModal()
				f_showNotification("All conditions deleted", duration = 15, type = "warning")
			}
			else {
				v_id <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:5), paste0("tp_t", 1:3))), "id"])
				o_sdata(f_create_cond_data(o_sdata_cond))
			}
			
			if (isolate(o_cond$create) == 1) {
				if (!"create_button" %in% v_id) {updateButton(session, "create_button", style = "default")}
				o_cond$create <- 0
			}
			
			if (isolate(o_click_button$create) == 1) {
				eval(parse(text = f_update_rv(list("rv" = c("o_click_button", rep("o_sdata_cond", 2)), "id" = c("create", "row_num", "formula"), "value" = c("0", "c()", "NA")))))
				rm(list = "sub", envir = e_data)
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
			}
			
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, 0), o_input_status$val)
		}
	})
	
	# 2.11. Add events of add button
	# ==============================
	
	observeEvent(input$c_add_button, {
		if ("all" %in% ls(e_data)) {
			if ((input$vtype == "qualit" & is.null(input$vvalue1)) | (input$vtype == "quant" & (length(which(input$vvalue2 == "")) == 1 | length(input$vvalue2) == 0))) {
				f_showNotification("Please complete the field corresponding to the variable value (sub-data creation)", duration = 15, type = "error")
			}
			else {
				s_e_message <- ifelse(input$vtype == "quant", ifelse(try(suppressWarnings(is.na(as.numeric(input$vvalue2)))), "The variable value must be numeric (sub-data creation)", NA), NA)
				
				if (!is.na(s_e_message)) {
					f_showNotification(s_e_message, duration = 15, type = "error")
				}
				else {
					s_e_message <- character(0)
					if (length(isolate(o_sdata_cond$var_name)) > 0) {eval(parse(text = paste0("s_e_message <- f_check_cond_data(o_sdata_cond, list(input$vname, input$vtype, input$rel_symbol, input$vvalue", ifelse(input$vtype == "qualit", 1, 2), "))")))}
					
					if (length(s_e_message) > 0) {
						f_showNotification(s_e_message, duration = 15, type = "error")
					}
					else {
						eval(parse(text = paste(paste0("o_sdata_cond$", c("var_name", "var_type", "var_rel"), " <- c(isolate(o_sdata_cond$", c("var_name", "var_type", "var_rel"), "), input$", c("vname", "vtype", "rel_symbol"), ")"), collapse = "; ")))
						eval(parse(text = paste0("o_sdata_cond$value[[length(o_sdata_cond$value) + 1]] <- ", ifelse(input$vtype == "qualit", "input$vvalue1", "as.numeric(input$vvalue2)"))))
						f_showNotification("New condition added", duration = 5, type = "message")
						v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:5), paste0("tp_t", 1:3)) | o_input_status$val$id %in% c("c_info_clear_button", "expand1_button", "c_formula", "create_button")), "id"])
						v_id_on <- c()
						
						if (length(isolate(o_sdata_cond$var_name)) == 1) {
							v_id_on <- c("c_info_clear_button", "expand1_button", "c_formula", "create_button")
						}
						else {
							v_id_all <- v_id_all[-which(v_id_all %in% c("c_info_clear_button", "expand1_button", "c_formula", "create_button"))]
						}
						
						if (isolate(o_cond$create) == 1) {
							if (!"create_button" %in% v_id_on) {updateButton(session, "create_button", style = "default")}
							o_cond$create <- 0
						}
						
						if (isolate(o_click_button$create) == 1) {
							eval(parse(text = f_update_rv(list("rv" = c("o_click_button", rep("o_sdata_cond", 2)), "id" = c("create", "row_num", "formula"), "value" = c("0", "c()", "NA")))))
							rm(list = "sub", envir = e_data)
							eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
							click("reset1_button")
						}
						
						o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val)
					}
				}
			}
		}
	})
	
	# 2.12. Add events on condition formula text input
	# ================================================
	
	observeEvent(input$c_formula, {
		if (isolate(o_cond$create) == 1) {
			updateButton(session, "create_button", style = "default")
			
			if (isolate(o_click_button$create) == 1) {
				eval(parse(text = f_update_rv(list("rv" = c("o_click_button", rep("o_sdata_cond", 2)), "id" = c("create", "row_num", "formula"), "value" = c("0", "c()", "NA")))))
				rm(list = "sub", envir = e_data)
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("code", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
			}
			
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[which(o_input_status$val$panel %in% c(paste0("lp_s", 3:5), paste0("tp_t", 1:3))), "id"]), 0), o_input_status$val)
			o_cond$create <- 0
		}
	})
	
	# 2.13. Add events of create button
	# =================================
	
	observeEvent(input$create_button, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$create) == 0) {
				o_cond$create <- 1 # condition to manage button color
				l_results <- f_create_sub_data(e_data$all, o_sdata_cond, input$c_formula)
				
				if (length(l_results[[3]]) > 0) {
					updateButton(session, "create_button", style = "danger") 
					f_showNotification(l_results[[3]], duration = 15, type = "error")
				}
				else {
					updateButton(session, "create_button", style = "success") 
					o_sdata_cond$row_num <- l_results[[1]]
					e_data$sub <- l_results[[2]]
					rm(list = "l_results")
					o_sdata_cond$formula <- input$c_formula
					o_click_button$create <- 1
					
					if ("flag" %in% ls(e_data)) {
						if (names(e_data$flag)[1] %in% names(e_data$all)) {o_special_input$val <- list("id" = "yes")}
					}
					
					if (input$data_type == "normal") {
						o_selectize_input$max_item <- f_create_input_maxitem_list("normal")
						v_id <- c(as.vector(o_input_status$val[which(o_input_status$val$panel == "lp_s3"), "id"]), ifelse("flag" %in% ls(e_data), ifelse(names(e_data$flag)[1] %in% names(e_data$all), "id", NA), "id"), f_create_input_id_vector("normal"))
					}
					else { # ir
						v_id <- c(ifelse("flag" %in% ls(e_data), ifelse(names(e_data$flag)[1] %in% names(e_data$all), "id", NA), "id"), f_create_input_id_vector(s_data = "ir", b_load2 = T))
					}
					
					o_option$choices <- c("label", "color", "pch")
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id[!is.na(v_id)], 1), o_input_status$val)
				}
			}
		}
	})
	
	# ---------------------
	# Normal plot selection
	# ---------------------
	
	# 2.14. Add events of plot type radio button 
	# ==========================================
	
	observeEvent(input$plot_type, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {
			v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c("lp_s4", "lp_s5", paste0("tp_t", 1:3)) | o_input_status$val$id %in% c("dim_num", "model")), "id"])
			v_id_all <- v_id_all[-which(v_id_all %in% c("var_group", "concat2"))]
			
			if (input$plot_type == "plot") {
				o_selectize_input$max_item <- f_create_input_maxitem_list(s_graph = paste(input$plot_type, input$dim_num, sep = "_"), s_model = input$model)
				v_id_on <- c(f_create_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_"), s_model = input$model), "dim_num", "model")
				
				if ("flag" %in% ls(e_data)) {
					if (substr(isolate(o_flag$name), nchar(isolate(o_flag$name)) - 9, nchar(isolate(o_flag$name)) - 4) == "withID") {
						o_special_input$val <- list("id" = "yes")
						v_id_on <- c(v_id_on, "id")
					}
				}
				else {
					v_id_on <- c(v_id_on, "id")
				}
			}
			else {
				if (o_input_status$val[o_input_status$val$id == "webgl", "special"] == 0) {o_special_input$val <- list("webgl" = "no")}
				o_selectize_input$max_item <- f_create_input_maxitem_list(s_graph = input$plot_type, b_concat1 = input$concat1)
				v_id_on <- f_create_input_id_vector(s_graph = input$plot_type, i_concat = o_cond$concat1)
			}
			
			l_results <- f_update_option_selectize_input(input$data_type, input$plot_type, input$mode, list("choices" = o_option$choices, "selected" = input$edit_option)) # update "edit_option" selectize input
			
			if (!is.null(l_results[[2]])) {
				o_option$choices <- l_results[[1]]
				eval(parse(text = l_results[[2]]))
			}
			
			v_graphic_opt <- c("bw", "dec_num")
			if (length(which(paste0(v_graphic_opt, "_radio") %in% v_id_on)) > 0) {eval(parse(text = paste0("v_id_all <- v_id_all[-which(v_id_all %in% unique(c(", paste(paste0("ifelse(\"", v_graphic_opt, "_radio\" %in% v_id_on, \"", v_graphic_opt, "\", NA)"), collapse = ", "), ")))]")))}
			l_results <- f_update_input_maxitem_list(o_selectize_input, NULL, o_input_status$val)
			if (input$plot_type %in% c("plot", "histplot") & input$concat1) {updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = NULL)}
			if (input$plot_type == "corplot" | (input$plot_type %in% c("plot", "boxplot") & o_input_status$val[o_input_status$val$id == "g_radio", "status"] == 0 & o_selectize_input$max_item_current[["var_y"]] == 9999)) {updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = NULL)}
			eval(parse(text = paste(paste0("o_selectize_input$", c("max_item", "max_item_current"), " <- l_results[[", 1:2, "]]"), collapse = "; ")))
			if (!is.null(l_results[[3]])) {eval(parse(text = f_update_maxitem_selectize_input(l_results[[3]], names(e_data$all))))}
			l_id_status <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val, T)
			v_id_sp <- as.vector(o_input_status$val[o_input_status$val$id %in% paste0(c("f", "g", "h"), "_radio"), "id"])
			v_pos <- which(v_id_sp %in% v_id_all & !v_id_sp %in% v_id_on & o_input_status$val[o_input_status$val$id %in% v_id_sp, "special"] == 1) 
			
			if (length(v_pos) > 0) { # (f_radio, g_radio, h_radio) input special desabling
				if (is.null(l_id_status)) {
					l_id_status <- f_create_input_status_list(v_id_sp[v_pos], 0)
				}
				else {
					eval(parse(text = paste(paste0("l_id_status$", v_id_sp[v_pos], " <- 0"), collapse = "; ")))
				}
			}
			
			if (length(o_label_text$label) > 0) { # reset custom labels
				o_label_text$text[which(o_label_text$label == "title")] <- ""
				if ("x" %in% o_label_text$label & (o_label_text$del$x == 1 | (input$plot_type %in% c("boxplot", "barplot", "corplot") | input$model != "none"))) {o_label_text$text[which(o_label_text$label == "x")] <- ""}
				if ("y" %in% o_label_text$label & (o_label_text$del$y == 1 | input$plot_type %in% c("histplot", "barplot", "corplot"))) {o_label_text$text[which(o_label_text$label == "y")] <- ""}
				if ("z" %in% o_label_text$label) {o_label_text$text[which(o_label_text$label == "z")] <- ""}
				eval(parse(text = paste(paste0("o_label_text$del$", c("x", "y", "z"), " <- 0"), collapse = "; ")))
				if (!is.null(o_label_text$text) & length(which(o_label_text$text != "")) == 0) {eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- c()"), collapse = "; ")))}
			}
			
			eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), "_temp <- c()"), collapse = "; ")))
			o_reset$concat <- 1
			o_reset$point_type_size <- 1 # reset custom point characteristics
			if (input$plot_type == "corplot" | "concat1" %in% names(l_id_status)) {o_reset$color_opacity <- 1} # reset custom colors/opacities
			click("reset1_button")
			o_on_off$val <- l_id_status
		}
	})
	
	# 2.15. Add events of dimension number radio button
	# =================================================
	
	observeEvent(input$dim_num, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {
			if (input$plot_type == "plot" & (input$dim_num == "3d" | (input$dim_num == "2d" & input$model == "none"))) {
				if (input$model != "none") {
					updateRadioButtons(session, "model", selected = "none")
				}
				
				if (input$dim_num == "3d") {
					o_special_input$val <- list("webgl" = "no")
				}
				
				if (length(o_label_text$label) > 0) { # reset custom labels
					o_label_text$text[which(o_label_text$label == "title")] <- ""
					if ("x" %in% o_label_text$label & (o_label_text$del$x == 1 | input$model != "none")) {o_label_text$text[which(o_label_text$label == "x")] <- ""}
					if ("y" %in% o_label_text$label & o_label_text$del$y == 1) {o_label_text$text[which(o_label_text$label == "y")] <- ""}
					if ("z" %in% o_label_text$label) {o_label_text$text[which(o_label_text$label == "z")] <- ""}
					eval(parse(text = paste(paste0("o_label_text$del$", c("x", "y", "z"), " <- 0"), collapse = "; ")))
					if (!is.null(o_label_text$text) & length(which(o_label_text$text != "")) == 0) {eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- c()"), collapse = "; ")))}
				}
				
				eval(parse(text = f_update_rv(list("rv" = c(rep("o_label_text", 2), rep("o_name_option", 2)), "id" = c("label_temp", "text_temp", "point_type", "point_type_temp"), "value" = rep("c()", 4)))))
				o_reset$concat <- 1
				click("reset1_button")
				v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c("lp_s4", "lp_s5", paste0("tp_t", 1:3))), "id"])
				v_id_all <- v_id_all[-which(v_id_all %in% c("id", "var_id", "var_group", "concat2"))]
				o_selectize_input$max_item <- f_create_input_maxitem_list(s_graph = paste("plot", input$dim_num, sep = "_"))
				v_id_on <- f_create_input_id_vector(s_graph = paste("plot", input$dim_num, sep = "_"))
				v_graphic_opt <- c("bw", "dec_num")
				if (length(which(paste0(v_graphic_opt, "_radio") %in% v_id_on)) > 0) {eval(parse(text = paste0("v_id_all <- v_id_all[-which(v_id_all %in% unique(c(", paste(paste0("ifelse(\"", v_graphic_opt, "_radio\" %in% v_id_on, \"", v_graphic_opt, "\", NA)"), collapse = ", "), ")))]")))}
				l_results <- f_update_input_maxitem_list(o_selectize_input, NULL, o_input_status$val)
				eval(parse(text = paste(paste0("o_selectize_input$", c("max_item", "max_item_current"), " <- l_results[[", 1:2, "]]"), collapse = "; ")))
				if (!is.null(l_results[[3]])) {eval(parse(text = f_update_maxitem_selectize_input(l_results[[3]], names(e_data$all))))}
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val, T)
			}
		}
	})
	
	# 2.16. Add events of model radio button 
	# ======================================
	
	observeEvent(input$model, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {
			o_path$name3_prev <- NA
			js$backgroundCol("data_path3", ifelse(input$model == "none", "transparent", "white"))
			
			if (!o_path$color3 %in% c("white", "transparent")) {
				eval(parse(text = f_update_rv(list("rv" = c("o_load_error", "o_data_info", "o_click_button"), "id" = c("code[3]", "mtime[3]", "load3"), "value" = c("0", "NA", "0")))))
				if ("m_param" %in% ls(e_data)) {rm(list = "m_param", envir = e_data)}
			}
			
			o_path$color3 <- ifelse(input$model == "none", "transparent", "white")
			
			if (input$model == "none") {
				eval(parse(text = f_update_rv(list("rv" = c("o_path", "o_click_button"), "id" = c("name3", "browse3"), "value" = c("NA", "0")))))
				if (input$dim_num == "3d" | input$plot_type %in% c("histplot", "plot")) {updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = NULL)}
				if ((input$dim_num == "3d" | input$plot_type %in% c("boxplot", "plot")) & input$g_radio == "yes") {updateRadioButtons(session, "g_radio", selected = "no")}
			}
			
			if (input$plot_type == "plot" & (input$dim_num == "2d" | (input$dim_num == "3d" & input$model != "none"))) {
				if (input$dim_num == "3d") {updateRadioButtons(session, "dim_num", selected = "2d")}
				o_reset$concat <- 1
				o_reset$label <- 1
				if (input$model != "none") {eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 2), "id" = c("color_opacity", "point_type_size"), "value" = rep("1", 2)))))} # reset custom colors/opacities/point characteristics
				click("reset1_button")
				v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel %in% c("lp_s4", "lp_s5", paste0("tp_t", 1:3))), "id"])
				v_id_on <- f_create_input_id_vector(s_model = input$model)
				
				if (input$model == "none") {
					if ("flag" %in% ls(e_data)) {
						if (substr(isolate(o_flag$name), nchar(isolate(o_flag$name)) - 9, nchar(isolate(o_flag$name)) - 4) == "withID") {
							o_special_input$val <- list("id" = "yes")
							v_id_on <- c(v_id_on, "id")
						}
					}
					else {
						v_id_on <- c(v_id_on, "id")
					}
					
					v_id_all <- v_id_all[-which(v_id_all %in% c("var_group", "concat2"))] 
					o_selectize_input$max_item <- f_create_input_maxitem_list(s_graph = "plot_2d")
					l_results <- f_update_input_maxitem_list(o_selectize_input, NULL, o_input_status$val)
					eval(parse(text = paste(paste0("o_selectize_input$", c("max_item", "max_item_current"), " <- l_results[[", 1:2, "]]"), collapse = "; ")))
					if (!is.null(l_results[[3]])) {eval(parse(text = f_update_maxitem_selectize_input(l_results[[3]], names(e_data$all))))}
				}
				else {
					v_id_on <- c(v_id_on, "data_path3", "browse3_button")
					if (input$data_path3 != "") {v_id_on <- c(v_id_on, "load3_button")}
				}
				
				v_graphic_opt <- c("bw", "dec_num")
				if (length(which(paste0(v_graphic_opt, "_radio") %in% v_id_on)) > 0) {eval(parse(text = paste0("v_id_all <- v_id_all[-which(v_id_all %in% unique(c(", paste(paste0("ifelse(\"", v_graphic_opt, "_radio\" %in% v_id_on, \"", v_graphic_opt, "\", NA)"), collapse = ", "), ")))]")))}
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val)
			}
		}
	})
	
	# -----------------------
	# Model parameter loading
	# -----------------------
	
	# 2.17. Add events of model parameter input/buttons 
	# =================================================
	
	# data_path3
	
	observeEvent(input$data_path3, {
		if (!is.na(isolate(o_path$name3))) {o_path$name3_prev <- isolate(o_path$name3)}
		
		if (!is.na(isolate(o_path$name3_prev))) {
			if (isolate(o_click_button$browse3) == 1 & input$data_path3 == "") {
				updateTextInput(session, "data_path3", value = isolate(o_path$name3))
			}
			else {
				o_path$name3 <- input$data_path3
			}
		}
		else {
			if (is.na(isolate(o_path$name3_prev)) & input$data_path3 != "") {o_path$name3 <- input$data_path3}
		}
		
		if (isolate(o_click_button$browse3) == 1) {o_click_button$browse3 <- 0}
		v_pos <- c()
		
		if (isolate(o_click_button$load3) == 1) {
			if (length(unique(c(isolate(o_path$name3_prev), isolate(o_path$name3)))) == 1 & !is.na(isolate(o_path$name3_prev))) {
				js$backgroundCol("data_path3", isolate(o_path$color3))
			}
			else {
				js$backgroundCol("data_path3", ifelse(input$model == "none", "transparent", "white"))
				o_path$color3 <- ifelse(input$model == "none", "transparent", "white")
				eval(parse(text = f_update_rv(list("rv" = c("o_load_error", "o_data_info", "o_click_button"), "id" = c("code[3]", "mtime[3]", "load3"), "value" = c("0", "NA", "0")))))
				if ("m_param" %in% ls(e_data)) {rm(list = "m_param", envir = e_data)}
				eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("concat", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
				click("reset1_button")
				v_pos <- which(o_input_status$val$panel == "lp_s5")
			}
		}
		
		v_pos <- sort(c(v_pos, which(o_input_status$val$id == "load3_button")))
		o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[v_pos, "id"]), ifelse(o_input_status$val[v_pos, "id"] == "load3_button" & !is.null(input$data_path3) & input$data_path3 != "", 1, 0)), o_input_status$val)
	})
	
	# browse3
	
	observeEvent(input$browse3_button, {
		o_click_button$browse3 <- 1
		o_volumes <- getVolumes()()
		shinyFileChoose(input, 'browse3_button', roots = o_volumes, filetypes = c('txt', 'csv'))
		s_data_path <- as.character(parseFilePaths(o_volumes, input$browse3_button)$datapath)
		updateTextInput(session, "data_path3", value = s_data_path)
    })
	
	# load3
	
	observeEvent(input$load3_button, {
		if ("all" %in% ls(e_data)) {
			b_cond <- F
			
			if (isolate(o_path$color3) == "lightgreen") {
				n_time <- as.numeric(file.info(input$data_path3)$mtime)
				b_cond <- ifelse(n_time == isolate(o_data_info$mtime)[3], F, T)
			}
			
			if ((length(unique(c(isolate(o_path$name3_prev), isolate(o_path$name3)))) > 1 & !is.na(isolate(o_path$name3_prev))) | isolate(o_click_button$load3) == 0 | isolate(o_load_error$code)[3] == 1 | b_cond == T) {
				if (b_cond) {
					if ("m_param" %in% ls(e_data)) {rm(list = "m_param", envir = e_data)}
					eval(parse(text = f_update_rv(list("rv" = rep("o_reset", 4), "id" = c("concat", "label", "color_opacity", "point_type_size"), "value" = rep("1", 4)))))
					click("reset1_button")
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[which(o_input_status$val$panel == "lp_s5"), "id"]), 0), o_input_status$val)
				}
				
				o_click_button$load3 <- 1
				l_results <- f_load_data(s_id = "load3_button", s_path = input$data_path3, s_model = input$model)
				
				if (is.na(l_results[[2]])) { # no error message
					if (!is.na(l_results[[3]])) {f_showNotification(l_results[[3]], duration = 15, type = "warning")} # warning message
					e_data$m_param <- l_results[[1]]
					js$backgroundCol("data_path3", "lightgreen")
					o_path$color3 <- "lightgreen"
					o_load_error$code[3] <- 0
					o_data_info$mtime[3] <- as.numeric(file.info(input$data_path3)$mtime)
					v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c(f_create_input_id_vector(s_model = input$model, b_load3 = T), "f_radio"), "id"])
					if (!is.na(l_results[[3]])) {v_id <- v_id[!v_id == "wres_radio"]}
					o_selectize_input$max_item <- f_create_input_maxitem_list(s_model = input$model)
					o_special_input$val <- list("f_radio" = "yes")
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, ifelse(v_id == "f_radio", 0, 1)), o_input_status$val)
				}
				else {
					js$backgroundCol("data_path3", "salmon")
					o_path$color3 <- "salmon"
					o_load_error$code[3] <- 1
					o_data_info$mtime[3] <- NA
					showNotification(l_results[[2]], duration = 15, type = "error")
				}
			}
		}
	})
	
	# ------------------
	# Variable selection
	# ------------------
	
	# 2.18. Add events on variables/functions radio buttons and checkboxes 
	# ====================================================================
	
	# ID: enable/disable the corresponding field
	
	observeEvent(input$id, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & input$plot_type == "plot" & !(input$subdata_option & o_click_button$create == 0)) {
				if (input$id == "yes" & "flag" %in% ls(e_data)) {o_special_input$val <- list("var_id" = names(e_data$flag)[1])}
				o_on_off$val <- f_create_input_status_list("var_id", ifelse(input$id == "yes", 1, 0))
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "id") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# random effects: enable/disable the corresponding field
	
	observeEvent(input$ref_radio, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list("ref", ifelse(input$ref_radio == "yes", 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "ref_radio") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# f(x): enable/disable the corresponding field
	
	observeEvent(input$f_radio, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list("f_text", ifelse(input$f_radio == "yes", 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "f_radio") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# g(y): enable/disable the corresponding field
	
	observeEvent(input$g_radio, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list("g_text", ifelse(input$g_radio == "yes", 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "g_radio") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# h(z): enable/disable the corresponding field
	
	observeEvent(input$h_radio, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list("h_text", ifelse(input$h_radio == "yes", 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "h_radio") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# weighted residuals: enable/disable variance function field
	
	observeEvent(input$wres_radio, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list(as.vector(o_input_status$val[o_input_status$val$id %in% c("wres_cbox", "wres_vfun"), "id"]), ifelse(input$wres_radio == "yes", 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "wres_radio") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# weighted residuals: add or not group variables on residual variance function
	
	observeEvent(input$wres_cbox, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {o_on_off$val <- f_update_input_status_list(f_create_input_status_list("wres_group", ifelse(input$wres_cbox, 1, 0)), o_input_status$val)}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "wres_cbox") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# Group: enable/disable the corresponding field
	
	observeEvent(input$group, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {
				v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c("concat2", "var_group"), "id"])
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, ifelse(v_id %in% c("concat2", "var_group") & input$group == "yes", 1, 0)), o_input_status$val)
				
				if (!input$plot_type %in% c("corplot", "boxplot", "barplot") & o_click_button$display == 0) {
					o_reset$color_opacity <- 1
					if (input$plot_type == "plot" & (input$data_type == "normal" | (input$data_type == "ir" & input$mode != "line"))) {o_reset$point_type_size <- 1} 
					click("reset1_button")
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "group") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# 2.19. Add events of del buttons
	# ===============================
	
	observeEvent(input$del1_button, {
		if ("all" %in% ls(e_data)) {
			v_choices <- unique(as.vector(e_data$all[, input$vname]))
			updateSelectizeInput(session, "vvalue1", choices = v_choices[order(v_choices)], selected = NULL)
		}
	})
	
	observeEvent(input$del2_button, {
		if ("all" %in% ls(e_data)) {updateSelectizeInput(session, "ref", choices = names(e_data$all), selected = NULL)}
	})
	
	observeEvent(input$del3_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = NULL)
			
			if (!input$concat1 &  input$model == "none") {
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list("f_radio", 1), o_input_status$val)
				if (input$f_radio == "yes") {updateRadioButtons(session, "f_radio", selected = "no")}
				o_label_text$del$x <- ifelse("x" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "x")] != "", 1, 0), 0) # reset x custom label
			}
			
			if (input$plot_type %in% c("boxplot", "barplot") & o_click_button$display == 0) {
				o_reset$color_opacity <- 1
				if (input$plot_type == "boxplot") {o_reset$point_type_size <- 1} 
				click("reset1_button")
			}
		}
	})
	
	observeEvent(input$del4_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = NULL)
			
			if (input$plot_type != "corplot") {
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list("g_radio", 1), o_input_status$val)
				if (input$g_radio == "yes") {updateRadioButtons(session, "g_radio", selected = "no")}
				o_label_text$del$y <- ifelse("y" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", 1, 0), 0) # reset y custom label
			}
			
			if (input$data_type == "temporal" & o_click_button$display == 0) {
				o_reset$color_opacity <- 1
				if (input$mode != "line") {o_reset$point_type_size <- 1} 
				click("reset1_button")
			}
		}
	})
	
	observeEvent(input$del5_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_z", choices = names(e_data$all), selected = NULL)
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list("h_radio", 1), o_input_status$val)
			if (input$h_radio == "yes") {updateRadioButtons(session, "h_radio", selected = "no")}
			o_label_text$del$z <- ifelse("z" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "z")] != "", 1, 0), 0) # reset z custom label
		}
	})
	
	observeEvent(input$del6_button, {
		if ("all" %in% ls(e_data)) {updateSelectizeInput(session, "wres_group", choices = names(e_data$all), selected = NULL)}
	})
	
	observeEvent(input$del7_button, {
		if ("all" %in% ls(e_data)) {updateSelectizeInput(session, "var_group", choices = names(e_data$all), selected = NULL)}
		
		if (!input$plot_type %in% c("corplot", "boxplot", "barplot") & o_click_button$display == 0) {
			o_reset$color_opacity <- 1
			if (input$plot_type == "plot" & (input$data_type == "normal" | (input$data_type == "ir" & input$mode != "line"))) {o_reset$point_type_size <- 1} 
			click("reset1_button")
		}
	})
	
	# 2.20. Add events of expand buttons
	# ==================================
	
	output$condInfo_temp <- DT::renderDataTable(o_expand$info_cond_data, server = F)
	output$modal_info_var <- DT::renderDataTable(o_expand$info_var_data, server = F)
	output$modal_code_var <- DT::renderDataTable(o_expand$code_var_data, server = F)
	
	observeEvent(input$expand1_button, {
		if ("all" %in% ls(e_data)) {
			o_expand$info_cond_data <- f_create_cond_data(o_sdata_cond, "none")
			eval(parse(text = f_build_modal_dialog_expand(s_title = "Condition formula:", s_id = "c_formula", i_window_num = 1, b_info_cond = T)))
		}
	})
	
	observeEvent(input$expand2_button, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = paste0("l_var <- list(", ifelse(input$model != "none" & input$ref_radio == "yes", "input$var_x, input$ref", "input$var_x"), ")"))) 
			l_results <- f_create_modal_code_var_list(input$data_type, input$model, 2, l_var)
			if (!is.null(l_results[[1]])) {o_expand$info_var_data <- l_results[[1]]}
			if (!is.null(l_results[[2]])) {o_expand$code_var_data <- l_results[[2]]}
			eval(parse(text = f_build_modal_dialog_expand(s_title = "f(x):", s_id = "f_text", i_window_num = 2, b_info_var = ifelse(!is.null(l_results[[1]]), T, F), b_code_var = ifelse(!is.null(l_results[[2]]), T, F))))
		}
	})
	
	observeEvent(input$expand3_button, {
		if ("all" %in% ls(e_data)) {
			l_results <- f_create_modal_code_var_list(input$data_type, input$model, 3, list(input$var_y))
			if (!is.null(l_results[[1]])) {o_expand$info_var_data <- l_results[[1]]}
			if (!is.null(l_results[[2]])) {o_expand$code_var_data <- l_results[[2]]}
			eval(parse(text = f_build_modal_dialog_expand(s_title = "g(y):", s_id = "g_text", i_window_num = 3, b_info_var = ifelse(!is.null(l_results[[1]]), T, F), b_code_var = ifelse(!is.null(l_results[[2]]), T, F))))
		}
	})
	
	observeEvent(input$expand4_button, {
		if ("all" %in% ls(e_data)) {
			l_results <- f_create_modal_code_var_list(input$data_type, input$model, 4, list(input$var_z))
			if (!is.null(l_results[[1]])) {o_expand$info_var_data <- l_results[[1]]}
			if (!is.null(l_results[[2]])) {o_expand$code_var_data <- l_results[[2]]}
			eval(parse(text = f_build_modal_dialog_expand(s_title = "h(z):", s_id = "h_text", i_window_num = 4, b_info_var = ifelse(!is.null(l_results[[1]]), T, F), b_code_var = ifelse(!is.null(l_results[[2]]), T, F))))
		}
	})
	
	observeEvent(input$expand5_button, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = paste0("l_var <- list(", ifelse(input$wres_cbox, "input$var_x, input$wres_group", "input$var_x"), ")"))) 
			l_results <- f_create_modal_code_var_list(input$data_type, input$model, 5, l_var)
			if (!is.null(l_results[[1]])) {o_expand$info_var_data <- l_results[[1]]}
			if (!is.null(l_results[[2]])) {o_expand$code_var_data <- l_results[[2]]}
			eval(parse(text = f_build_modal_dialog_expand(s_title = "variance function:", s_id = "wres_vfun", i_window_num = 5, b_info_var = ifelse(!is.null(l_results[[1]]), T, F), b_code_var = ifelse(!is.null(l_results[[2]]), T, F))))
		}
	})
	
	# 2.21. Add events of Ok/Close buttons (modal dialog: expand button)
	# ====================================
	
	observeEvent(input$ok_button_exp1, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = f_update_text_field_input("c_formula", input$c_formula_modal, T)))
			o_expand$info_cond_data <- NULL
		}
	})
	
	observeEvent(input$close_button_exp1, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			o_expand$info_cond_data <- NULL
		}
	})
	
	observeEvent(input$ok_button_exp2, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = f_update_text_field_input("f_text", input$f_text_modal, T)))
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$close_button_exp2, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$ok_button_exp3, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = f_update_text_field_input("g_text", input$g_text_modal, T)))
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$close_button_exp3, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$ok_button_exp4, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = f_update_text_field_input("h_text", input$h_text_modal, T)))
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$close_button_exp4, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$ok_button_exp5, {
		if ("all" %in% ls(e_data)) {
			eval(parse(text = f_update_text_field_input("wres_vfun", input$wres_vfun_modal, T)))
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	observeEvent(input$close_button_exp5, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			if (!is.null(o_expand$info_var_data)) {o_expand$info_var_data <- NULL}
			if (!is.null(o_expand$code_var_data)) {o_expand$code_var_data <- NULL}
		}
	})
	
	# 2.22. Add events on concatenation checkbox for X/Group variables
	# ================================================================
	
	# concat1
	
	observeEvent(input$concat1, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {
				v_name <- f_create_input_value_list("selectize", e_data$all, c(o_cond$concat1, o_cond$concat2), o_click_button$create, 1)
				
				if (input$plot_type %in% c("boxplot", "barplot")) {
					o_selectize_input$max_item_current$var_x <- ifelse(input$concat1, 9999, 1)
					eval(parse(text = f_update_maxitem_selectize_input(o_selectize_input$max_item_current["var_x"], v_name)))
					
					if (o_click_button$display == 0) {
						o_reset$color_opacity <- 1
						if (input$plot_type == "boxplot") {o_reset$point_type_size <- 1} 
						click("reset1_button")
					}
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "concat1") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# concat2
	
	observeEvent(input$concat2, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0)) {
				v_name <- f_create_input_value_list("selectize", e_data$all, c(o_cond$concat1, o_cond$concat2), o_click_button$create, 1)
				if (input$group == "yes") {eval(parse(text = f_update_maxitem_selectize_input(list("var_group" = ifelse(input$concat2, 9999, 1)), v_name)))}
				
				if (!input$plot_type %in% c("corplot", "boxplot", "barplot") & input$group == "yes" & o_click_button$display == 0) {
					o_reset$color_opacity <- 1
					if (input$plot_type == "plot" & (input$data_type == "normal" | (input$data_type == "ir" & input$mode != "line"))) {o_reset$point_type_size <- 1} 
					click("reset1_button")
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "concat2") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
			}
		}
	})
	
	# 2.23. Add events on (X, Y, Z) input fields
	# ==========================================
	
	observeEvent(input$var_x, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & input$data_type == "normal" & input$model == "none") {
				if (input$plot_type %in% c("plot", "histplot")) {
					o_label_text$del$x <- ifelse("x" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "x")] != "", 1, 0), 0) # reset x custom label
					i_num <- length(which(!is.na(input$var_x)))
					
					if ((i_num == 2 & o_input_status$val[o_input_status$val$id == "f_radio", "special"] == 0) | (i_num < 2 & o_input_status$val[o_input_status$val$id == "f_radio", "special"] == 1)) {
						if (i_num == 2) {o_special_input$val <- list("f_radio" = "yes")}
						o_on_off$val <- f_update_input_status_list(f_create_input_status_list("f_radio", ifelse(i_num < 2, 1, 0)), o_input_status$val)
					}
				}
				
				if (input$plot_type %in% c("boxplot", "barplot") & o_click_button$display == 0) {
					o_reset$color_opacity <- 1
					if (input$plot_type == "boxplot") {o_reset$point_type_size <- 1} 
					click("reset1_button")
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "var_x") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
				if (o_label_text$del$x == 1) {o_label_text$del$x <- 0} # cancel the x custom label reseting
			}
		}
	})
	
	observeEvent(input$var_y, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0) & input$data_type == "normal" & input$plot_type %in% c("plot", "boxplot")) {
				o_label_text$del$y <- ifelse("y" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", 1, 0), 0) # reset y custom label
				i_num <- length(which(!is.na(input$var_y)))
				
				if ((i_num == 2 & o_input_status$val[o_input_status$val$id == "g_radio", "special"] == 0) | (i_num < 2 & o_input_status$val[o_input_status$val$id == "g_radio", "special"] == 1)) {
					if (i_num == 2) {o_special_input$val <- list("g_radio" = "yes")}
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list("g_radio", ifelse(i_num < 2, 1, 0)), o_input_status$val)
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "var_y") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
				if (o_label_text$del$y == 1) {o_label_text$del$y <- 0} # cancel the y custom label reseting
			}
		}
	})
	
	observeEvent(input$var_z, {
		if (o_cond$update == 0) {
			if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0)) {
				o_label_text$del$z <- ifelse("z" %in% o_label_text$label, ifelse(o_label_text$text[which(o_label_text$label == "z")] != "", 1, 0), 0) # reset z custom label
				i_num <- length(which(!is.na(input$var_z)))
				
				if ((i_num == 2 & o_input_status$val[o_input_status$val$id == "h_radio", "special"] == 0) | (i_num < 2 & o_input_status$val[o_input_status$val$id == "h_radio", "special"] == 1)) {
					if (i_num == 2) {o_special_input$val <- list("h_radio" = "yes")}
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list("h_radio", ifelse(i_num < 2, 1, 0)), o_input_status$val)
				}
			}
		}
		else {
			if (!is.null(o_update$val)) {
				if (o_update$val == "var_z") {eval(parse(text = f_update_rv(list("rv" = c("o_cond", "o_update"), "id" = c("update", "val"), "value" = c("0", "NULL")))))}
				if (o_label_text$del$z == 1) {o_label_text$del$z <- 0} # cancel the z custom label reseting
			}
		}
	})
	
	observeEvent(input$var_group, {
		if ("all" %in% ls(e_data) & !(input$subdata_option & o_click_button$create == 0) & !(input$model != "none" & o_click_button$load3 == 0) & !input$plot_type %in% c("corplot", "boxplot", "barplot") & input$group == "yes" & o_click_button$display == 0) {
			o_reset$color_opacity <- 1
			if (input$plot_type == "plot" & (input$data_type == "normal" | (input$data_type == "ir" & input$mode != "line"))) {o_reset$point_type_size <- 1} 
			click("reset1_button")
		}
	})
	
	# 2.24. Add events of display button
	# ==================================
	
	observeEvent(input$display_button, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_cond$display) == 0 & is.list(input$traces)) {
				js$resetClick_leg()
				i_ms <- 100
			}
			else {
				if (isolate(o_cond$select_graph2) == 1) {
					o_cond$select_graph2 <- 0
					js$resetClick_leg()
					i_ms <- 100
				}
				else {
					i_ms <- 0
				}
			}
			
			if (o_cond$display == 0 & o_click_button$display == 1) { # click on the display button when a graph is already created
				if (length(o_label_text$label) > 0) { # update custom labels
					if (o_label_text$del$x == 1) {o_label_text$text[which(o_label_text$label == "x")] <- ""}
					if (o_label_text$del$y == 1) {o_label_text$text[which(o_label_text$label == "y")] <- ""}
					if (o_label_text$del$z == 1) {o_label_text$text[which(o_label_text$label == "z")] <- ""}
					if (sum(as.vector(unlist(o_label_text$del))) > 0) {o_label_text$text[which(o_label_text$label == "title")] <- ""}
					eval(parse(text = paste(paste0("o_label_text$del$", c("x", "y", "z"), " <- 0"), collapse = "; ")))
					if (!is.null(o_label_text$text) & length(which(o_label_text$text != "")) == 0) {eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- c()"), collapse = "; ")))}
				}
				
				if (length(o_name_option$name) > 0) { # update custom colors/opacities/point characteristics
					eval(parse(text = f_update_rv(list("rv" = rep("o_name_option", 8), "id" = c("color_temp", "color_default", "opacity_temp", "opacity_default", "point_type_temp", "point_type_default", "point_size_temp", "point_size_default"), "value" = rep("c()", 8)))))
					eval(parse(text = paste0("v_var_name <- ", ifelse(input$data_type == "temporal", "input$var_y", ifelse(input$plot_type %in% c("boxplot", "barplot"), "input$var_x", ifelse(input$group == "no", "NA", "input$var_group"))))))
					eval(parse(text = paste0("df_all <- ", ifelse(input$data_type == "temporal", "NULL", ifelse(!"sub" %in% ls(e_data), "e_data$all", "e_data$sub")))))
					l_results <- f_update_graph_option_rv(o_name_option, input$edit_option, o_click_button$display, input$data_type, input$plot_type, v_var_name, df_all, T)
					if (length(l_results[[1]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", names(l_results[[1]]), " <- l_results[[1]][[", 1:length(l_results[[1]]), "]]"), collapse = "; ")))}
					if (length(l_results[[2]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", l_results[[2]], " <- c()"), collapse = "; ")))}
				}
			}
			
			shinyjs::delay(i_ms, 
				if (input$data_type == "normal") {
					# A. Type: Normal
					# ---------------
					
					if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) { # executed with the display button or the Flag tab save button
						eval(parse(text = f_update_rv(list("rv" = c(rep("o_cond", 2), rep("o_plot", 4)), "id" = c("qc2", "legend", "data", "model", "data_qc2", "elt"), "value" = c(rep("0", 2), "data.frame()", "NULL", rep("NA", 2))))))
						
						if (isolate(o_cond$display) == 0) { # executed with the display button
							eval(parse(text = f_update_rv(list("rv" = c(rep("o_w_message", 4), rep("o_legend_group", 4)), "id" = rep(c("lreg", "conf_ellipsoid", "dens_curve", "norm_dens_curve"), 2), "value" = c(rep("0", 4), rep("c()", 4))))))
							
							if (length(e_current_flag) > 0) {
								rm(list = ls(e_current_flag), envir = e_current_flag)
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							v_input_id <- c("id", "var_id", "ref_radio", "ref", "concat1", "var_x", "f_radio", "f_text", "var_y", "g_radio", "g_text", "var_z", "h_radio", "h_text", "wres_radio", "wres_cbox", "wres_group", "wres_vfun", "group", "concat2", "var_group")
							eval(parse(text = paste0("l_input_id_value <- list(", paste(paste0("\"", v_input_id, "\" = input$", v_input_id), collapse = ", "), ")")))
							v_cond <- f_check_fields(input$data_type, input$plot_type, input$dim_num, input$model, l_input_id_value) 
							v_pos <- which(v_cond) 
							
							if (length(v_pos) == 0) {
								# Parameter value assignment: (o_parameter reactive value)
								# ---------------------------
								
								o_parameter$data_name <- ifelse(isolate(o_click_button$create) == 1, "sub", "all")
								o_parameter$plot_type <- input$plot_type
								o_parameter$dim_num <- NA
								o_parameter$model <- NA
								o_parameter$concat1 <- input$concat1
								if (input$concat1) {o_parameter$concat1_group <- input$var_x}
								o_parameter$concat2 <- input$concat2
								if (input$concat2) {o_parameter$concat2_group <- input$var_group}
								
								if (input$plot_type != "corplot") {
									if (input$plot_type != "barplot") {
										if (input$plot_type  == "histplot") {
											o_parameter$f <- ifelse(input$f_radio == "yes", input$f_text, NA)
										}
										else {
											if (input$plot_type == "plot") {
												o_parameter$dim_num <- input$dim_num
												o_parameter$model <- input$model
												o_parameter$id <- ifelse(input$id == "yes", input$var_id, NA)
												o_parameter$f <- ifelse(input$f_radio == "yes", input$f_text, NA)
												
												if (input$dim_num == "3d") {
													o_parameter$z <- input$var_z
													o_parameter$h <- ifelse(input$h_radio == "yes", input$h_text, NA)
												}
												else {
													if (input$model %in% c("calib", "valid")) {
														eval(parse(text = paste0("o_parameter$ref <- ", ifelse(input$ref_radio == "yes", "input$ref", "NA"))))
														
														if (input$model == "calib") {
															if (input$wres_radio == "yes") {
																o_parameter$wres_vfun <- input$wres_vfun
																eval(parse(text = paste0("o_parameter$wres_group <- ", ifelse(input$wres_cbox, "input$wres_group", "NA"))))
															}
															else {
																o_parameter$wres_vfun <- NA
																o_parameter$wres_group <- NA
															}
														}
													}
													else {
														o_parameter$ref <- NA
														o_parameter$wres_vfun <- NA
														o_parameter$wres_group <- NA
													}
												}
											}
											
											o_parameter$g <- ifelse(input$g_radio == "yes", input$g_text, NA)
											o_parameter$y <- input$var_y
										}
									}
									
									if (input$concat1) {
										e_data[[isolate(o_parameter$data_name)]]$.concat1. <- f_create_concat_variable(e_data[[isolate(o_parameter$data_name)]], input$var_x)
										o_parameter$x <- ".concat1."
									}
									else {
										o_parameter$x <- input$var_x
									}
								}
								else {
									o_parameter$y <- input$var_y
								}
								
								if (input$group == "yes") {
									if (input$concat2) {
										e_data[[isolate(o_parameter$data_name)]]$.concat2. <- f_create_concat_variable(e_data[[isolate(o_parameter$data_name)]], input$var_group)
										o_parameter$group <- ".concat2."
									}
									else {
										o_parameter$group <- input$var_group
									}
								}
								else {
									o_parameter$group <- NA
								}
								
								# Checking process: (check selected variables)
								# -----------------
								
								s_e_message <- f_check_variables("normal", ifelse(input$plot_type == "plot" & input$model == "none", T, F), e_data, o_parameter)
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {s_e_message <- character(0)}
						
						if (length(s_e_message) > 0) { # error message returned by checking processes
							f_showNotification(s_e_message, duration = 15, type = "error")
						}
						else { # no error
							# Data preparation: (process 1)
                            # -----------------
							
							eval(parse(text = paste0("l_results <- f_prepare_data(\"normal\", 1, e_data[[isolate(o_parameter$data_name)]], ", ifelse("sub" %in% ls(e_data), "isolate(o_sdata_cond$row_num)", "NULL"), ", NULL, NULL, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", isolate(o_flag$name), NULL, NULL, NULL, NULL, o_parameter, NULL)")))
							
							if (length(l_results[[4]]) > 0) { # error (all values with a qc = 2)
								f_showNotification(l_results[[4]], duration = 15, type = "error")
							}
							else { # no error
								df_all <- l_results[[1]]
								if (dim(l_results[[2]])[1] > 0) {o_plot$data_qc2 <- l_results[[2]]}
								o_cond$qc2 <- l_results[[3]]
								s_w_message <- l_results[[5]] 
								rm(list = "l_results")
								
								if (isolate(o_parameter$plot_type) != "corplot") { # corplot not concerned
									# Graph default option assignment: (color, opacity, point type/size)
									# --------------------------------
									
									l_results <- f_create_default_graph_opt_list("normal", df_all, o_parameter)
									eval(parse(text = paste(paste0("o_name_option$", names(l_results), "_default <- l_results[[", 1:length(l_results), "]]"), collapse = "; ")))
									
									if (isolate(o_parameter$model) %in% c("calib", "valid")) { # calibration/validation model added
										# Checking process: (check model)
										# -----------------
										
										l_results <- f_check_model(df_all, e_data$m_param, o_parameter)
										l_fun_val <- l_results[[1]]
										s_e_message <- l_results[[2]]
										rm(list = "l_results")
									}
									else { # no model added
										if (!is.na(isolate(o_parameter$f)) | !is.na(isolate(o_parameter$g)) | !is.na(isolate(o_parameter$h))) { # (f, g, h) function(s) added
											# Checking process: (check transformed variables)
											# -----------------
											
											l_results <- f_check_trsf_variables("normal", df_all, o_parameter)
											l_fun_val <- l_results[[1]]
											s_e_message <- l_results[[2]]
											rm(list = "l_results")
										}
										else { # no function added
											l_fun_val <- list()
											s_e_message <- NULL
										}
									}
									
									if (length(s_e_message) > 0) { # error with model/transformed variables
										f_showNotification(s_e_message, duration = 15, type = "error")
									}
									else { # no error
										if (isolate(o_parameter$model) %in% c("calib", "valid")) { # calibration/validation model
											o_plot$model <- as.data.frame(l_fun_val[which(names(l_fun_val) %in% c("fit", "variance"))]) # save a data.frame with fit/error variance values
										}
										
										# Data preparation: (process 2)
										# -----------------
										
										o_plot$data <- f_prepare_data(s_data_type = "normal", i_proc_num = 2, df_all = df_all, o_parameter = o_parameter, l_fun_val = l_fun_val)
										rm(list = "l_fun_val")
										v_e_message <- c()
										
										if (isolate(o_cond$display) == 0) {	# executed with the display button		
											o_zoom$coord <- NULL
											l_axis_layout <- NULL
											
											# assign default graph labels
											o_parameter$xlab <- ifelse(input$f_radio == "yes", "f(x)", "x")
											o_parameter$ylab <- ifelse(input$g_radio == "yes", "g(y)", "y")
											o_parameter$zlab <- ifelse(input$h_radio == "yes", "h(z)", "z")
											
											# Checking process: (check tp1 inputs)
											# -----------------
											
											v_pos <- which(c(!isolate(o_parameter$autodec_num), !isolate(o_parameter$autobw)))
											
											if (length(v_pos) > 0) {
												l_opt_name <- list(input$dec_num, tryCatch({suppressWarnings(eval(parse(text = input$bw)))}, error = function(e) FALSE))[v_pos]
												names(l_opt_name) <- c("dec_num", "bw")[v_pos]
												v_e_message <- f_check_tp1_inputs(l_opt_name, o_plot, o_parameter)
												if (length(v_e_message) == 0) {eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))}
												rm(list = "l_opt_name")
											}
										}
										
										if (length(v_e_message) > 0) { # error with graphic options
											f_showNotification(paste(v_e_message, collapse = "<br/>"), duration = 15, type = "error")
											o_plot$data <- data.frame()
										}
										else { # no error
											# Build legend items (process 1) 
											# ------------------
											
											l_results <- f_build_legend_items("normal", 1, o_parameter, o_cond, o_legend_group, o_plot)
											o_legend_group <- l_results[[1]]
											o_click_legend$item <- l_results[[2]]
											rm(list = "l_results")
											
											# Create element data
											# -------------------
											
											if (isolate(o_parameter$plot_type) == "plot") {
												if (isolate(o_parameter$model) == "none") {o_plot$elt <- f_create_element_data(s_data_type = "normal", o_parameter = o_parameter, o_cond = o_cond, o_plot = o_plot)}
											}
										}
									}
								}
								else { # corplot
									if (isolate(o_cond$display) == 0) {o_zoom$coord <- NULL} # executed with the display button
									o_plot$data <- df_all
								}
								
								rm(list = "df_all")
							}
						}
					}
					
					if (dim(isolate(o_plot$data))[1] > 0) {
						if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) {
							if (length(s_w_message) > 0) {
								f_showNotification(s_w_message, duration = 15, type = "warning")
								rm(list = "s_w_message")
							}
						}
						
						if (isolate(o_cond$display) == 1) { # executed if the graph is already displayed and one of options available from Graphic/Statistics tabs is modified
							o_cond$display <- 0
							
							# Edit axis layout (axis range/camera position)
							# ----------------
							
							l_axis_layout <- f_edit_axis_layout("normal", o_parameter, o_zoom)
							
							# Build legend items (processes 2) 
							# ------------------
							
							if (is.list(input$traces)) {
								l_traces <- list(T, names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")])
							}
							else {
								l_traces <- list(F, c())
							}
							
							l_results <- f_build_legend_items("normal", 2, o_parameter, o_cond, o_legend_group, o_plot, isolate(o_click_legend$item), l_traces)
							o_legend_group <- l_results[[1]]
							if (!is.na(o_cond$stat)) {o_cond$stat <- ifelse(o_cond$stat != 0, ifelse(is.null(o_legend_group[[o_cond$stat]]) & o_parameter[[o_cond$stat]], -1, NA), NA)}
							o_click_legend$item <- l_results[[2]]
							rm(list = "l_results")
						}
						else { # no graph displayed
							if (!is.na(isolate(o_parameter$model))) { # model added
								if (isolate(o_parameter$model) == "calib") { # calib (update "select_graph" list items)
									if (!"variance" %in% names(isolate(o_plot$model))) {
										v_group <- c("Residuals vs fitted values", paste0("Residuals vs x", 1:length(isolate(o_parameter$x)))) 
									}
									else {
										v_group <- c("QQplot", "Standardized residuals vs fitted values", paste0("Standardized residuals vs x", 1:length(isolate(o_parameter$x)))) 
									}
									
									if (is.na(isolate(o_parameter$select_graph))) {
										o_cond$select_graph1 <- 0
										o_parameter$select_graph <- v_group[1]
										updateSelectizeInput(session, "select_graph", choices = v_group, selected = v_group[1])
									}
									else {
										if (isolate(o_parameter$select_graph) != v_group[1]) {
											o_cond$select_graph1 <- 0
											o_parameter$select_graph <- v_group[1]
											updateSelectizeInput(session, "select_graph", choices = v_group, selected = v_group[1])
										}
										else {
											o_cond$select_graph1 <- 1
										}
									}
									
									shinyjs::show(id = "tmainpanel")
									shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 1, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 3, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 2, 4)))))
									shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 5, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 7, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 6, 8)))))
								}
								else { # valid (initialize values of "sh_popup2" button)
									if (length(isolate(o_legend_group$lreg)) > 0) {
										o_lreg_info$xpos <- 0
										o_lreg_info$ypos <- 1
										o_lreg_info$elt <- NA
										shinyjs::disable(id = "reset3_button")
										
										if (isolate(o_parameter$model) == "valid") {
											v_choice <- c("lreg parameters", "R2", "t-test on lreg parameters") 
										}
										else {
											v_choice <- c("lreg parameters", "R2", "RMSE")
										}
										
										updateSelectizeInput(session, "lreg_info_elt", choices = v_choice, selected = input$lreg_info_elt)
										shinyjs::show(id = "sh_popup2")
									}
								}
							}
							else { # no model added
								if (isolate(o_parameter$plot_type) == "corplot") { # corplot (update "select_graph" list items)
									if (!is.na(isolate(o_parameter$group))) {
										v_group <- unique(as.vector(isolate(o_plot$data)[, isolate(o_parameter$group)]))
										v_group <- v_group[order(v_group)]
										
										if (is.na(isolate(o_parameter$select_graph))) {
											o_parameter$corplot_group <- v_group
											o_cond$select_graph1 <- 0
											o_parameter$select_graph <- v_group[1]
											updateSelectizeInput(session, "select_graph", choices = v_group, selected = v_group[1])
											shinyjs::show(id = "tmainpanel")
											shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 1, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 3, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 2, 4)))))
											shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 5, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 7, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 6, 8)))))
										}
										else {
											if (length(isolate(o_parameter$corplot_group)) != length(v_group)) {
												o_parameter$corplot_group <- v_group
												o_cond$select_graph1 <- 0
												o_parameter$select_graph <- v_group[1]
												updateSelectizeInput(session, "select_graph", choices = v_group, selected = v_group[1])
											}
											else {
												if (length(which(!isolate(o_parameter$corplot_group) %in% v_group)) > 0) {
													o_parameter$corplot_group <- v_group
													o_cond$select_graph1 <- 0
													o_parameter$select_graph <- v_group[1]
													updateSelectizeInput(session, "select_graph", choices = v_group, selected = v_group[1])
												}
												else {
													o_cond$select_graph1 <- 1
												}
											}
										}
									}
									else {
										if (input$select_graph != " ") {
											o_parameter$select_graph <- NA
											o_cond$select_graph1 <- 0
											updateSelectizeInput(session, "select_graph", choices = " ")
											shinyjs::hide(id = "tmainpanel")
											shinyjs::removeClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 5, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 7, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 6, 8)))))
											shinyjs::addClass("mainpanel", class = paste0("mainpanel_class", ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 0, 1, ifelse(o_click_button$left_panel == 0 & o_click_button$top_panel == 1, 3, ifelse(o_click_button$left_panel == 1 & o_click_button$top_panel == 0, 2, 4)))))
										}
									}
								}
							}
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({
							v_dimension <- input$dimension
							df_click_legend <- isolate(o_click_legend$item)
							eval(parse(text = paste0("l_graph_opt <- ", ifelse(isolate(o_parameter$plot_type) == "corplot", "NULL", "f_create_graph_opt_list(isolate(o_name_option))"))))
							
							# build graph:
							
							ply_1 <- f_build_graph("normal", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info, o_label_text, l_graph_opt)
							
							# add flags:
							
							v_cond <- c(rep(0, 2), isolate(o_cond$qc2))
							ply_1 <- f_add_flag("normal", ply_1, v_cond, df_click_legend, o_plot, o_parameter, NULL, e_current_flag)
							
							# add statistics:
							
							v_stat_method <- c("lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve")[which(c(isolate(o_parameter$lreg), isolate(o_parameter$conf_ellipsoid), isolate(o_parameter$centroid), isolate(o_parameter$dens_curve), isolate(o_parameter$norm_dens_curve)))]
							
							if (length(v_stat_method) > 0) {
								for (i in v_stat_method) {
									eval(parse(text = paste0("v_group <- isolate(o_legend_group$", i, ")")))
									
									if (i == "centroid") {
										ply_1 <- f_add_centroid(ply_1, df_click_legend, o_plot, o_parameter, l_graph_opt)
									}
									else {
										eval(parse(text = paste0("l_results <- f_add_", i, "(ply_1, v_group, df_click_legend, o_plot, o_parameter, ", ifelse(i == "lreg", "o_lreg_info, ", ""), "o_w_message$", i, ", l_graph_opt$color)")))
										
										if (length(l_results[[3]]) > 0) {
											f_showNotification(l_results[[3]], duration = 15, type = "error")
											updateCheckboxInput(session, i, value = F)
										}
										else {
											if (length(l_results[[2]]) > 0) {
												f_showNotification(l_results[[2]], duration = 15, type = "warning")
												eval(parse(text = paste0("o_w_message$", i, " <- 1")))
											}
											
											ply_1 <- l_results[[1]] 
										}
									}
								}
							}
							
							# add java script to save legend item status:
							
							ply_1 %>% onRender(s_js_2)
						})
						
						shinyjs::show(id = "graphic")
						shinyjs::show(id = "sh_popup1")
						if ((!is.na(isolate(o_parameter$dim_num)) & isolate(o_parameter$dim_num) == "2d") | isolate(o_parameter$plot_type) == "histplot") {shinyjs::show(id = "sh_reset")}
					}
				}
				else if (input$data_type == "temporal") {
					# B. Type : Temporal
					# ------------------
					
					if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) {
						eval(parse(text = f_update_rv(list("rv" = c(rep("o_cond", 3), rep("o_plot", 12)), "id" = c("flag", paste0("qc", 1:2), "data", "y_coord", "add_pt", "pt_pos", "var_pt", paste0("data_qc", 1:2), paste0("var_qc", 1:2), "leg_name_qc", "elt", "elt_pt_pos"), "value" = c(rep("0", 3), "data.frame()", "NULL", "T", rep("NA", 9))))))
						if ("data" %in% ls(e_previous_flag)) {rm("data", envir = e_previous_flag)}
						s_w_message <- character(0)
						
						if (isolate(o_cond$display) == 0) { # executed with the display button
							if (length(e_current_flag) > 0) {
								eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
								rm(list = ls(e_current_flag), envir = e_current_flag)
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							v_input_id <- c("var_x", "var_y", "g_radio", "g_text")
							eval(parse(text = paste0("l_input_id_value <- list(", paste(paste0("\"", v_input_id, "\" = input$", v_input_id), collapse = ", "), ")")))
							v_cond <- f_check_fields(input$data_type, input$plot_type, input$dim_num, input$model, l_input_id_value) 
							v_pos <- which(v_cond)

							if (length(v_pos) == 0) {
								# Parameter value assignment: (o_parameter reactive value)
								# ---------------------------
								
								o_parameter$data_name <- "all"
								o_parameter$x <- input$var_x
								o_parameter$date_format <- input$date_format
								o_parameter$y <- input$var_y
								o_parameter$g <- ifelse(input$g_radio == "yes", input$g_text, NA)
							
								# Checking process: (check selected variables)
								# -----------------
								
								l_results <- f_check_variables("temporal", F, e_data, o_parameter)
								s_e_message <- l_results[[1]]
								s_w_message <- l_results[[2]]
								v_date <- l_results[[3]]
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {s_e_message <- NULL}
						
						if (length(s_e_message) > 0) { # error message returned by the checking process
							f_showNotification(s_e_message, duration = 15, type = "error")
							if (length(s_w_message) > 0) {f_showNotification(s_w_message, duration = 15, type = "warning")}
						}
						else { # no error
							# # Graph default option assignment: (color, opacity, point type/size)
							# ----------------------------------
							
							l_results <- f_create_default_graph_opt_list("temporal", data.frame(), o_parameter)
							eval(parse(text = paste(paste0("o_name_option$", names(l_results), "_default <- l_results[[", 1:length(l_results), "]]"), collapse = "; ")))
							v_e_message <- c()
							
							if (isolate(o_cond$display) == 0) {
								o_zoom$coord <- NULL
								l_axis_layout <- NULL
								
								# assign default graph label
								o_parameter$ylab <- ifelse(input$g_radio == "yes", "g(y)", "y")
								
								# Checking process: (check tp1 inputs)
								# -----------------
								
								v_pos <- which(c(!isolate(o_parameter$autodec_num)))
								
								if (length(v_pos) > 0) {
									l_opt_name <- list(input$dec_num)[v_pos]
									names(l_opt_name) <- c("dec_num")[v_pos]
									v_e_message <- f_check_tp1_inputs(l_opt_name, NULL, o_parameter)
									if (length(v_e_message) == 0) {eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))}
									rm(list = "l_opt_name")
								}
							}
							
							if (length(v_e_message) > 0) { # error with graphic options
								f_showNotification(paste(v_e_message, collapse = "<br/>"), duration = 15, type = "error")
								if (length(s_w_message) > 0) {f_showNotification(s_w_message, duration = 15, type = "warning")}
							}
							else { # no error
								s_e_message <- character(0)
								
								if (!is.na(isolate(o_parameter$g))) { # g function added
									# Checking process: (check transformed variables: function filled in text field)
									# -----------------
									
									s_e_message <- f_check_trsf_variables("temporal", NULL, o_parameter)
								}
								
								if (length(s_e_message) > 0) { # error with g function text field
									f_showNotification(s_e_message, duration = 15, type = "error")
									if (length(s_w_message) > 0) {f_showNotification(s_w_message, duration = 15, type = "warning")}
								}
								else { # no error
									v_w_message <- c()
									if (length(s_w_message) > 0) {v_w_message <- s_w_message}
									df_all <- e_data$all[, c(isolate(o_parameter$x), isolate(o_parameter$y))]
									
									if (isolate(o_cond$display) == 0) {
										df_all[, isolate(o_parameter$x)] <- v_date
									}
									else {
										df_all[, isolate(o_parameter$x)] <- f_create_date_variable(df_all, isolate(o_parameter$x), isolate(o_parameter$date_format))
									}
									
									# Data preparation: (process 1)
									# -----------------
									
									eval(parse(text = paste0("l_results <- f_prepare_data(\"temporal\", 1, df_all, NULL, NULL, NULL, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", NULL, ", ifelse("data" %in% ls(e_previous_flag), "e_previous_flag$data", "NULL"), ", NULL, NULL, NULL, o_parameter)")))
									
									if (length(l_results[[9]]) > 0) { # error (all values with a qc = 2)
										f_showNotification(l_results[[9]], duration = 15, type = "error")
										if (length(s_w_message) > 0) {f_showNotification(s_w_message, duration = 15, type = "warning")}
									}
									else { # no error
										df_all <- l_results[[1]]
										e_previous_flag$data <- l_results[[2]]
										eval(parse(text = paste(paste0("o_plot$", c("data_qc1", "data_qc2", "var_qc1", "var_qc2", "leg_name_qc"), " <- l_results[[", c(3, 4, 6:8), "]]"), collapse = "; ")))
										eval(parse(text = paste(paste0("o_cond$", c("flag", "qc1", "qc2"), " <- l_results[[5]][", 1:3, "]"), collapse = "; ")))
										v_w_message <- c(v_w_message, l_results[[10]])
										s_e_message <- character(0)
										
										if (!is.na(isolate(o_parameter$g))) { # only if a g function is added
											# Checking process: (check transformed variables: calculation)
											# -----------------
											
											l_results <- f_check_trsf_variables("temporal", df_all, o_parameter)
											s_e_message <- l_results[[2]]
											
											if (length(s_e_message) == 0) {
												# Data preparation: (process 2)
												# -----------------
												
												df_all <- f_prepare_data(s_data_type = "temporal", i_proc_num = 2, df_all = df_all, o_parameter = o_parameter, l_fun_val = l_results[[1]])
											}
										}
										
										if (length(s_e_message) > 0) { # error with g function calculation
											f_showNotification(s_e_message, duration = 15, type = "error")
											if (length(s_w_message) > 0) {f_showNotification(s_w_message, duration = 15, type = "warning")}
										}
										else { # no error
											if (length(v_w_message) > 0) {f_showNotification(paste(v_w_message, collapse = "<br/>"), duration = 15, type = "warning")}
											o_plot$data <- df_all
											rm(list = c("l_results", "df_all"))
											
											# Data preparation: (process 3)
											# -----------------
											
											l_pt_pos <- f_prepare_data(s_data_type = "temporal", i_proc_num = 3, df_all = isolate(o_plot$data), o_parameter = o_parameter)
											
											# create data corresponding to isolated points of Y variable(s)
											
											v_pos <- which(as.vector(lengths(l_pt_pos)) > 0)
											
											if (length(v_pos) > 0) {
												o_plot$pt_pos <- l_pt_pos
												o_plot$var_pt <- isolate(o_parameter$y)[v_pos]
												o_plot$elt_pt_pos <- c(length(isolate(o_parameter$y)) + 1, length(isolate(o_parameter$y)) + length(isolate(o_plot$var_pt)))
												o_plot$add_pt <- ifelse(input$mode == "line", T, F)
											}
											else {
												o_plot$add_pt <- F
											}
											
											# Build legend items (process 1) 
											# ------------------
											
											o_click_legend$item <- f_build_legend_items("temporal", 1, o_parameter, o_cond, NULL, o_plot)
											
											# Create element data (process 1)
											# -------------------
											
											o_plot$elt <- f_create_element_data("temporal", 1, o_parameter, o_cond, o_plot)
										}
									}
								}
							}
						}
					}
					
					if (dim(isolate(o_plot$data))[1] > 0) {
						if (isolate(o_cond$display) == 1) {
							if (isolate(o_cond$save2) == 0) {
								if (isolate(o_cond$mode) == 1) { # "mode" radio button (Graphic tab) modified
									if (length(isolate(o_plot$elt_pt_pos)) == 2) {
										# Create element data (process 2)
										# -------------------
										
										o_plot$elt <- f_create_element_data("temporal", 2, o_parameter, o_cond, o_plot, input$mode)
										o_plot$add_pt <- ifelse(input$mode == "line", T, F)
									}
									
									o_cond$mode <- 0
								}
							}
							
							# Edit axis layout (axis range)
							# ----------------
							
							l_axis_layout <- f_edit_axis_layout("temporal", NULL, o_zoom)
							
							# Build legend items (process 2)
							# ------------------
							
							if (is.list(input$traces)) {
								l_traces <- list(T, names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")])
							}
							else {
								l_traces <- list(F, c())
							}
							
							o_click_legend$item <- f_build_legend_items("temporal", 2, o_parameter, o_cond, NULL, o_plot, isolate(o_click_legend$item), l_traces)
							o_cond$display <- 0
						}
						
						if (input$y_scale %in% c("auto", "manual")) {
							# Update Y axis range (y_scale radio button: auto or manual)
							# -------------------
							
							eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(\"temporal\", input$y_scale, e_data$all, o_parameter, o_zoom, o_plot, o_cond, isolate(o_click_legend$item), ifelse(input$y_scale == \"auto\", input$fraction, NULL), ", ifelse(input$y_scale == "manual", "c(input$ymin, input$ymax)", "NULL"), ")")))
							l_axis_layout[[2]] <- l_results[[1]]
							
							if (input$y_scale == "auto") {
								o_plot$y_coord <- l_results[[2]] 
							}
							else {
								if (input$ymin != l_results[[1]][1]) {updateTextInput(session, "ymin", value = as.character(l_results[[1]][1]))}
								if (input$ymax != l_results[[1]][2]) {updateTextInput(session, "ymax", value = as.character(l_results[[1]][2]))}
							}
							
							if (length(l_results[[length(l_results)]]) > 0) {f_showNotification(l_results[[length(l_results)]], duration = 15, type = "warning")}
							rm(list = "l_results")
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({
							v_dimension <- input$dimension
							df_click_legend <- isolate(o_click_legend$item)
							l_graph_opt <- f_create_graph_opt_list(o_name_option)
							
							# build graph:
							
							ply_1 <- f_build_graph("temporal", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info, o_label_text, l_graph_opt)
							
							# add flags:
							
							v_cond <- c(isolate(o_cond$flag), isolate(o_cond$qc1), isolate(o_cond$qc2))
							ply_1 <- f_add_flag("temporal", ply_1, v_cond, df_click_legend, o_plot, o_parameter, NULL, e_current_flag)
							
							# add java script to save legend item status:
							
							ply_1 %>% onRender(s_js_2)
						})
						
						shinyjs::show(id = "graphic")
						shinyjs::show(id = "sh_popup1")
					}		
				}
				else {
					# C. Type : IR
					# ------------
					
					if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) {
						eval(parse(text = f_update_rv(list("rv" = c(rep("o_cond", 3), rep("o_plot", 8)), "id" = c("flag", paste0("qc", 1:2), "data", "id_group", "y_coord", "add_pt", "pt_pos", paste0("data_qc", 1:2), "elt"), "value" = c(rep("0", 3), "data.frame()", "NA", "NULL", "F", rep("NA", 4))))))
						
						if (isolate(o_cond$display) == 0) {
							if (length(e_current_flag) > 0) {
								rm(list = ls(e_current_flag), envir = e_current_flag)
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							v_input_id <- c("id", "var_id", "group", "concat2", "var_group")
							eval(parse(text = paste0("l_input_id_value <- list(", paste(paste0("\"", v_input_id, "\" = input$", v_input_id), collapse = ", "), ")")))
							v_cond <- f_check_fields(input$data_type, input$plot_type, input$dim_num, input$model, l_input_id_value) 
							v_pos <- which(v_cond)
							
							if (length(v_pos) == 0) {
								# Parameter value assignment: (o_parameter reactive value)
								# ---------------------------
								
								o_parameter$data_name <- ifelse(isolate(o_click_button$create) == 1, "sub", "all")
								o_parameter$id <- ifelse(input$id == "yes", input$var_id, NA)
								o_parameter$concat2 <- input$concat2
								if (input$concat2) {o_parameter$concat2_group <- input$var_group}
								
								if (input$group == "yes") {
									if (input$concat2) {
										e_data$all$.concat2. <- f_create_concat_variable(e_data$all, input$var_group)
										o_parameter$group <- ".concat2."
									}
									else {
										o_parameter$group <- input$var_group
									}
								}
								else {
									o_parameter$group <- NA
								}
								
								# Checking process: (check selected variables)
								# -----------------
								
								s_e_message <- f_check_variables("ir", F, e_data, o_parameter)
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {s_e_message <- NULL}
						
						if (length(s_e_message) > 0) {
							f_showNotification(s_e_message, duration = 15, type = "error")
						}
						else {
							v_e_message <- c()
							
							if (isolate(o_cond$display) == 0) {
								o_zoom$coord <- NULL
								
								# assign default graph label
								o_parameter$ylab <- "y"
								
								# Checking process: (check tp1 inputs)
								# -----------------
								
								v_pos <- which(c(!isolate(o_parameter$autodec_num)))
								
								if (length(v_pos) > 0) {
									l_opt_name <- list(input$dec_num)[v_pos]
									names(l_opt_name) <- c("dec_num")[v_pos]
									v_e_message <- f_check_tp1_inputs(l_opt_name, NULL, o_parameter)
									if (length(v_e_message) == 0) {eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))}
									rm(list = "l_opt_name")
								}
							}
							
							if (length(v_e_message) > 0) { # error with graphic options
								f_showNotification(paste(v_e_message, collapse = "<br/>"), duration = 15, type = "error")
							} 
							else { # no error
								# Data preparation: (process 1)
								# -----------------
								
								eval(parse(text = paste0("l_results <- f_prepare_data(\"ir\", 1, e_data[[isolate(o_parameter$data_name)]], ", ifelse("sub" %in% ls(e_data), "isolate(o_sdata_cond$row_num)", "NULL"), ", e_data$code_freq, isolate(o_data_info$code), ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", NULL, NULL, NULL, NULL, NULL, o_parameter)")))
								
								if (length(l_results[[8]]) > 0) { # error with data preparation
									f_showNotification(l_results[[8]], duration = 15, type = "error")
								} 
								else { # no error
									eval(parse(text = paste(paste0("df_", c("all", "code_freq", "qc1", "qc2"), " <- l_results[[", 1:4, "]]"), collapse = "; ")))
									eval(parse(text = paste(paste0("o_cond$", c("flag", "qc1", "qc2"), " <- l_results[[5]][", 1:3, "]"), collapse = "; ")))
									l_id <- l_results[[6]]
									o_plot$pt_pos <- l_results[[7]]
									if (isolate(o_cond$display) == 0) {l_axis_layout <- list(rev(range(as.vector(df_code_freq$Frequency))), NULL)}
									
									# Graph default option assignment: (color, opacity, point type/size)
									# --------------------------------
									
									l_results <- f_create_default_graph_opt_list("ir", l_results[[1]], o_parameter)
									eval(parse(text = paste(paste0("o_name_option$", names(l_results), "_default <- l_results[[", 1:length(l_results), "]]"), collapse = "; ")))
									
									# Data preparation: (process 2)
									# -----------------
									
									l_graph_opt <- f_create_graph_opt_list(o_name_option)
									l_results <- f_prepare_data("ir", 2, df_all, NULL, df_code_freq, NULL, NULL, NULL, NULL, list(df_qc1, df_qc2), l_id, NULL, o_parameter, l_graph_opt)
									eval(parse(text = paste(paste0("o_plot$", c("data", "data_qc1", "data_qc2", "id_group"), " <- l_results[[", 1:4, "]]"), collapse = "; ")))
									rm(list = c("l_results", "df_all", "df_code_freq", "df_qc1", "df_qc2", "l_id"))
									
									# Create element data (process 1)
									# -------------------
									
									o_plot$elt <- f_create_element_data("ir", 1, NULL, o_cond, o_plot, input$mode)
									if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0 & input$mode == "line") {o_plot$add_pt <- T}
									
									# Build legend items (process 1)
									# ------------------
									
									o_click_legend$item <- f_build_legend_items("ir", 1, o_parameter, NULL, NULL, o_plot)
								}
							}
						}
					}
					
					if (dim(isolate(o_plot$data))[1] > 0) {
						if (isolate(o_cond$display) == 1) {
							if (isolate(o_cond$save2) == 0) {
								if (isolate(o_cond$mode) == 1) { # "mode" radio button (Graphic tab) modified
									if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
										# Create element data (process 2)
										# -------------------
										
										o_plot$elt <- f_create_element_data("ir", 2, o_parameter, o_cond, o_plot, input$mode)
										o_plot$add_pt <- ifelse(input$mode == "line", T, F)
									}
									
									o_cond$mode <- 0
								}
							}
							
							if (o_cond$ok_color_opacity == 1 | o_cond$ok_point_type_size == 1) {
								# Update graph option (color, opacity, point type/size)
								# -------------------
								
								l_graph_opt <- f_create_graph_opt_list(o_name_option)
								
								if (is.na(o_parameter$group)) {
									if (o_cond$ok_color_opacity == 1) {
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag$", c("color", "opacity"), " <- l_graph_opt$", c("color", "opacity")), collapse = "; ")))
									}
									else {
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag$", c("point_type", "point_size"), " <- l_graph_opt$", c("point_type", "point_size")), collapse = "; ")))
									}
								}
								else {
									if (o_cond$ok_color_opacity == 1) {
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag[which(o_plot$id_group$no_flag$group == \"", o_name_option$name, "\"), \"color\"] <- \"", l_graph_opt$color, "\""), collapse = "; ")))
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag[which(o_plot$id_group$no_flag$group == \"", o_name_option$name, "\"), \"opacity\"] <- ", l_graph_opt$opacity), collapse = "; ")))
									}
									else {
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag[which(o_plot$id_group$no_flag$group == \"", o_name_option$name, "\"), \"point_type\"] <- ", l_graph_opt$point_type), collapse = "; ")))
										eval(parse(text = paste(paste0("o_plot$id_group$no_flag[which(o_plot$id_group$no_flag$group == \"", o_name_option$name, "\"), \"point_size\"] <- ", l_graph_opt$point_size), collapse = "; ")))
									}
								}
								
								eval(parse(text = paste(paste0("o_cond$ok_", c("color_opacity", "point_type_size"), " <- 0"), collapse = "; ")))
							}
							
							# Edit axis layout (axis range)
							# ----------------
							
							l_axis_layout <- f_edit_axis_layout("ir", NULL, o_zoom, range(as.vector(isolate(o_plot$data)$Frequency)))
							
							# Build legend items (process 2)
							# ------------------
							
							if (is.list(input$traces)) {
								l_traces <- list(T, names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")])
							}
							else {
								l_traces <- list(F, c())
							}
							
							o_click_legend$item <- f_build_legend_items("ir", 2, o_parameter, NULL, NULL, NULL, isolate(o_click_legend$item), l_traces)
							o_cond$display <- 0
						}
						
						if (input$y_scale %in% c("auto", "manual")) {
							# Update Y axis range (y_scale radio button: auto or manual)
							# -------------------
							
							eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(\"ir\", input$y_scale, e_data[[isolate(o_parameter$data_name)]], o_parameter, o_zoom, o_plot, NULL, isolate(o_click_legend$item), ifelse(input$y_scale == \"auto\", input$fraction, NULL), ", ifelse(input$y_scale == "manual", "c(input$ymin, input$ymax)", "NULL"), ")")))
							l_axis_layout[[2]] <- l_results[[1]]
							
							if (input$y_scale == "auto") {
								o_plot$y_coord <- l_results[[2]] 
							}
							else {
								if (input$ymin != l_results[[1]][1]) {updateTextInput(session, "ymin", value = as.character(l_results[[1]][1]))}
								if (input$ymax != l_results[[1]][2]) {updateTextInput(session, "ymax", value = as.character(l_results[[1]][2]))}
							}
							
							if (length(l_results[[length(l_results)]]) > 0) {f_showNotification(l_results[[length(l_results)]], duration = 15, type = "warning")}
							rm(list = "l_results")
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({	
							v_dimension <- input$dimension
							df_click_legend <- isolate(o_click_legend$item)
							
							# build graph:
							
							ply_1 <- f_build_graph("ir", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info, o_label_text)
							
							# add flags:
							
							v_cond <- c(isolate(o_cond$flag), isolate(o_cond$qc1), isolate(o_cond$qc2))
							eval(parse(text = paste0("ply_1 <- f_add_flag(\"ir\", ply_1, v_cond, df_click_legend, o_plot, o_parameter, ", ifelse(v_cond[1] == 1, "e_data$flag", "NULL"), ", e_current_flag)")))
							
							# add statistics:
							
							if (isolate(o_parameter$mean_spect)) {ply_1 <- f_add_mean_spect(ply_1, df_click_legend, o_plot, o_parameter)}
							
							# add java script to save legend item status:
							
							ply_1 %>% onRender(s_js_2)
						})
						
						shinyjs::show(id = "graphic")
						shinyjs::show(id = "sh_popup1")
						shinyjs::show(id = "sh_reset")
					}
				}
			)
			
			shinyjs::delay(i_ms, {
				v_graphic_opt <- c("y_scale", "bw", "dec_num")
				v_id_all <- as.vector(o_input_status$val[which(o_input_status$val$panel == "tp_t2" | o_input_status$val$id %in% paste0(v_graphic_opt, "_button")), "id"])
				v_id_on <- c()
				
				if (dim(isolate(o_plot$data))[1] > 0) {
					if (length(e_current_flag) > 0) {v_id_all <- v_id_all[!v_id_all %in% paste0(c("clear1", "clear2", "save"), "_button")]}
					
					if (input$plot_type == "plot" & input$model == "none") {
						i_cond <- 0
						v_id_stat <- f_create_t3_input_id_vector(input$data_type, paste(input$plot_type, input$dim_num, sep = "_"))
						if (length(v_id_stat) > 0) {eval(parse(text = paste0("i_cond <- length(which(c(", paste(paste0("input$", v_id_stat), collapse = ", "), ")))")))}
						
						if (i_cond == 0 & is.na(o_parameter$f) & is.na(o_parameter$g) & is.na(o_parameter$h)) {
							v_id_on <- f_create_input_id_vector(s_data = input$data_type, s_graph = paste(input$plot_type, input$dim_num, sep = "_"), b_display = T)
							if (input$action == "replace_qc") {v_id_on <- v_id_on[v_id_on != "qc"]}
						}
						else {
							if (o_cond$flag_msg == 0) {o_cond$flag_msg <- 1}
						}
					}
					
					if (isolate(o_click_button$display) == 0) {
						eval(parse(text = paste0("v_pos <- which(c(", paste(paste0("input$", c("y_scale", paste0(v_graphic_opt[-1], "_radio"))), collapse = ", "), ") == \"manual\")")))
						if (length(v_pos) > 0) {v_id_on <- c(v_id_on, paste0(v_graphic_opt, "_button")[v_pos])}
						o_click_button$display <- 1
					}
					else {
						v_pos <- which(v_id_all %in% paste0(v_graphic_opt, "_button"))
						if (length(v_pos) > 0) {v_id_all <- v_id_all[-v_pos]}
					}
					
					o_input_status$display <- o_input_status$val[which(o_input_status$val$id %in% f_create_varsel_input_id_vector(input$data_type, ifelse(input$plot_type != "plot", input$plot_type, paste(input$plot_type, input$dim_num, sep = "_")), input$model, ifelse(input$data_type != "temporal" & "flag" %in% ls(e_data), T, F))),] 
					shinyjs::enable("graph_clear_button")
					o_graphic_click$resume() # remove suspension for plotly_click associated to the "graphic" output
					o_graphic_relayout$resume() # remove suspension for plotly_relayout associated to the "graphic" output
				}
				else {
					if (o_click_button$display == 0) {eval(parse(text = paste(paste0("o_name_option$", c("color", "opacity", "point_type", "point_size"), "_default <- c()"), collapse = "; ")))}
					o_reset$concat <- 1
					click("reset1_button")
				}
				
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id_all, ifelse(v_id_all %in% v_id_on, 1, 0)), o_input_status$val)
			})
		}
	})
	
	observeEvent(o_cond$display, {
		if (o_cond$display == 1) {click("display_button")}
	})
	
	# 2.25. Add events of graph clear button 
	# ======================================
	
	observeEvent(input$graph_clear_button, {
		if ("all" %in% ls(e_data)) {
			if (length(o_label_text$label) > 0) { # update custom labels
				if (o_label_text$del$x == 1) {o_label_text$text[which(o_label_text$label == "x")] <- ""}
				if (o_label_text$del$y == 1) {o_label_text$text[which(o_label_text$label == "y")] <- ""}
				if (o_label_text$del$z == 1) {o_label_text$text[which(o_label_text$label == "z")] <- ""}
				if (sum(as.vector(unlist(o_label_text$del))) > 0) {o_label_text$text[which(o_label_text$label == "title")] <- ""}
				eval(parse(text = paste(paste0("o_label_text$del$", c("x", "y", "z"), " <- 0"), collapse = "; ")))
				if (!is.null(o_label_text$text) & length(which(o_label_text$text != "")) == 0) {eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- c()"), collapse = "; ")))}
			}
			
			if (length(o_name_option$name) > 0) { # update custom colors/opacities/point characteristics
				eval(parse(text = f_update_rv(list("rv" = rep("o_name_option", 8), "id" = c("color_temp", "color_default", "opacity_temp", "opacity_default", "point_type_temp", "point_type_default", "point_size_temp", "point_size_default"), "value" = rep("c()", 8)))))
				eval(parse(text = paste0("v_var_name <- ", ifelse(input$data_type == "temporal", "input$var_y", ifelse(input$plot_type %in% c("boxplot", "barplot"), "input$var_x", ifelse(input$group == "no", "NA", "input$var_group"))))))
				eval(parse(text = paste0("df_all <- ", ifelse(input$data_type == "temporal", "NULL", ifelse(!"sub" %in% ls(e_data), "e_data$all", "e_data$sub")))))
				l_results <- f_update_graph_option_rv(o_name_option, input$edit_option, o_click_button$display, input$data_type, input$plot_type, v_var_name, df_all, T)
				if (length(l_results[[1]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", names(l_results[[1]]), " <- l_results[[1]][[", 1:length(l_results[[1]]), "]]"), collapse = "; ")))}
				if (length(l_results[[2]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", l_results[[2]], " <- c()"), collapse = "; ")))}
			}
			
			v_graphic_opt <- c("y_scale", "bw", "dec_num")
			v_id <- as.vector(o_input_status$val[which(o_input_status$val$panel == "tp_t2" | o_input_status$val$id %in% paste0(v_graphic_opt, "_button")), "id"])
			o_reset$concat <- 1
			click("reset1_button")
			o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, 0), o_input_status$val)
		}
	})
	
	# =========
	# Top panel
	# =========
	
	# -------
	# Graphic
	# -------
	
	# 3.1. Add events for the "webgl" radio button 
	# ============================================
	
	observeEvent(input$webgl, {
		if ("all" %in% ls(e_data)) {
			o_parameter$webgl <- input$webgl  
			if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("webgl", "update", "display"), "value" = rep(1, 3)))))}
		}
	})
	
	# 3.2. Add events for the "mode" radio button 
	# ===========================================
	
	observeEvent(input$mode, {
		if ("all" %in% ls(e_data)) {
			if (input$mode == "line" & o_click_button$display == 0) {
				o_reset$point_type_size <- 1
				click("reset1_button")
			}
			
			o_parameter$mode <- input$mode  
			if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("mode", "update", "display"), "value" = rep(1, 3)))))}
			l_results <- f_update_option_selectize_input(input$data_type, input$plot_type, input$mode, list("choices" = o_option$choices, "selected" = input$edit_option)) # update "edit_option" selectize input
			
			if (!is.null(l_results[[2]])) {
				o_option$choices <- l_results[[1]]
				eval(parse(text = l_results[[2]]))
			}
		}
	})
	
	# 3.3. Add events on the bin width radio button
	# =============================================
	
	observeEvent(input$bw_radio, {
		if (isolate(o_click_button$display) == 1 | isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
			if (input$bw_radio == "auto") {
				o_parameter$autobw <- T
				o_parameter$bw <- NA
			}
			else {
				o_parameter$autobw <- F
			}
			
			v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c("bw", "bw_button"), "id"])
			eval(parse(text = paste0("v_status <- ", ifelse(input$bw_radio == "auto", "rep(0, 2)", ifelse(isolate(o_click_button$display) == 1, "rep(1, 2)", "c(1, 0)")))))
			names(v_status) <- c("bw", "bw_button")
			v_status <- as.vector(v_status[v_id])
			
			if (isolate(o_click_button$display) == 1) {
				eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))
				o_cond$update <- 1
				if (input$bw_radio == "auto") {o_cond$display <- 1}
			}
			else {
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, v_status), o_input_status$val)
			}
		}
	})
	
	# 3.4. Add events on "double-arrows" button to change histplot bin width
	# ======================================================================
	
	observeEvent(input$bw_button, {
		if ("all" %in% ls(e_data)) {
			l_opt_name <- list("bw" = tryCatch({suppressWarnings(eval(parse(text = input$bw)))}, error = function(e) FALSE))
			s_e_message <- f_check_tp1_inputs(l_opt_name, o_plot, o_parameter)
			
			if (length(s_e_message) > 0) {
				showNotification(s_e_message, duration = 15, type = "error")
			}
			else {
				o_parameter$bw <- l_opt_name$bw
				eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			}
		}
	})
	
	# 3.5. Add events to Y scale radio button
	# =======================================
	
	observeEvent(input$y_scale, {
		if ("all" %in% ls(e_data)) {
			v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c("fraction", "ymin", "ymax", "y_scale_button"), "id"])
			eval(parse(text = paste0("v_status <- ", ifelse(input$y_scale == "auto", "c(1, rep(0, 3))", ifelse(input$y_scale == "manual", "c(0, rep(1, 3))", "rep(0, 4)")))))
			names(v_status) <- c("fraction", "ymin", "ymax", "y_scale_button")
			v_status <- as.vector(v_status[v_id])
			o_parameter$y_scale <- input$y_scale
			
			if (input$y_scale != "auto") {
				if (input$y_scale == "manual") { 
					if (isolate(o_click_button$display) == 1) {
						if (input$data_type == "temporal") {
							v_y_range <- range(e_data$all[, isolate(o_parameter$y)], na.rm = T)
						}
						else {
							v_code <- as.vector(e_data$code_freq$Code)
							v_y_range <- range(e_data[[isolate(o_parameter$data_name)]][, which(names(e_data$all) %in% v_code)], na.rm = T)
						}
						
						updateTextInput(session, "ymin", value = v_y_range[1])
						updateTextInput(session, "ymax", value = v_y_range[2])
						s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = s_ylab, range = v_y_range)))
						eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))
						o_cond$update <- 1
					}
				}
				else {
					o_plot$y_coord <- NULL
					
					if (isolate(o_click_button$display) == 1) {
						s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = s_ylab, range = NULL)))
						eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))
						o_cond$update <- 1
					}
				}
			}
			else {
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, v_status), o_input_status$val)
			}
		}
	})
	
	# 3.6. Add events on fraction numeric input
	# =========================================
	
	observeEvent(input$fraction, {
		if ("all" %in% ls(e_data)) {
			if (input$y_scale == "auto") {
				s_w_message <- f_check_tp1_inputs(list("fraction" = input$fraction))
				
				if (length(s_w_message) > 0) {
					showNotification(s_w_message, duration = 15, type = "error")
					updateNumericInput(session, "fraction", value = ifelse(length(grep("0.05", s_w_message)) == 1, 0.05, ifelse(length(grep("0.1", s_w_message)) == 1, 0.1, 0)))
				}
				
				if (isolate(o_click_button$display) == 1) {
					if (!is.na(input$fraction) & input$fraction >= 0 & input$fraction <= 0.1) { 
						df_click_legend <- isolate(o_click_legend$item)
						
						if (is.list(input$traces)) {
							v_name <- names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")]
							df_click_legend$statut <- ifelse(df_click_legend$name %in% v_name, "\"legendonly\"", "T")
						}
						
						eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data[[isolate(o_parameter$data_name)]], o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", df_click_legend, input$fraction, NULL)")))
						o_plot$y_coord <- l_results[[2]] 
						s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = s_ylab, range = l_results[[1]])))
						o_cond$update <- 1
						if (length(l_results[[3]]) > 0) {showNotification(l_results[[3]], duration = 15, type = "warning")}
						rm(list = "l_results")
					}
				}
			}
		}
	})
	
	# 3.7. Add events on "double-arrows" button for manual y scale adjustment 
	# =======================================================================
	
	observeEvent(input$y_scale_button, {
		if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
	})
	
	# 3.8. Add events on decimal number radio button
	# ===============================================
	
	observeEvent(input$dec_num_radio, {
		if (isolate(o_click_button$display) == 1 | isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
			if (input$dec_num_radio == "auto") {
				o_parameter$autodec_num <- T
				o_parameter$dec_num <- NA
			}
			else {
				o_parameter$autodec_num <- F
			}
			
			v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c("dec_num", "dec_num_button"), "id"])
			eval(parse(text = paste0("v_status <- ", ifelse(input$dec_num_radio == "auto", "rep(0, 2)", ifelse(isolate(o_click_button$display) == 1, "rep(1, 2)", "c(1, 0)")))))
			names(v_status) <- c("dec_num", "dec_num_button")
			v_status <- as.vector(v_status[v_id])
			
			if (isolate(o_click_button$display) == 1) {
				eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))
				o_cond$update <- 1
				if (input$dec_num_radio == "auto") {o_cond$display <- 1}
			}
			else {
				o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, v_status), o_input_status$val)
			}
		}
	})
	
	# 3.9. Add events on "double-arrows" button corresponding to decimal number
	# ==========================================================================
	
	observeEvent(input$dec_num_button, {
		if ("all" %in% ls(e_data)) {
			l_opt_name <- list("dec_num" = input$dec_num)
			s_e_message <- f_check_tp1_inputs(l_opt_name)
			
			if (length(s_e_message) > 0) {
				showNotification(s_e_message, duration = 15, type = "error")
			}
			else {
				o_parameter$dec_num <- l_opt_name$dec_num
				eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			}
		}
	})
	
	# 3.10. Add events on inputs associated to label/(color, opacity)/(point type, size) edition
	# ===========================================================================================
	
	output$option_inventory <- DT::renderDataTable(o_option$data, server = F)
	o_option_inventory_proxy <- DT::dataTableProxy("option_inventory")
	output$point_type_info <- renderPlotly(o_option$plotly)
	
	observeEvent(input$edit_option_button, {
		if ("all" %in% ls(e_data)) {
			if (input$edit_option == "label") {
				v_label <- c("title", ifelse(input$data_type == "normal" & input$plot_type %in% c("plot", "histplot") & (input$model == "none" | (input$model == "calib" & !input$select_graph %in% c("QQplot", "Standardized residuals vs fitted values", "Residuals vs fitted values"))), "x", NA), ifelse(input$plot_type %in% c("plot", "boxplot") & input$model == "none", "y", NA), ifelse(input$dim_num == "3d", "z", NA))
				
				if (length(o_label_text$label) == 0) {
					o_label_text$label_temp <- v_label[!is.na(v_label)]
					o_label_text$text_temp <- rep("", length(o_label_text$label_temp))
				}
				else {
					eval(parse(text = paste0("v_text <- ", ifelse(length(o_label_text$text) > 0, "o_label_text$text", "rep(\"\", length(o_label_text$label))"))))
					names(v_text) <- o_label_text$label
					o_label_text$label_temp <- v_label[!is.na(v_label)]
					v_text_temp <- rep("", length(o_label_text$label_temp))
					names(v_text_temp) <- o_label_text$label_temp
					v_pos <- which(names(v_text) %in% o_label_text$label_temp)
					if (length(v_pos) > 0) {v_text_temp[names(v_text)[v_pos]] <- v_text[names(v_text)[v_pos]]}
					o_label_text$text_temp <- as.vector(v_text_temp)
				}
				
				o_option$data <- f_create_option_data(input$edit_option, NULL, o_label_text)
				
				showModal(modalDialog(title = "Edit label", 
					easyClose = F,
					size = "l",
					textAreaInput("custom_label", "Custom label:", character(0), width = "855px", height = validateCssUnit("auto"), resize = "none"),
					HTML("<br>"),
					DT::dataTableOutput("option_inventory"),
					footer = tagList(disabled(actionButton("option_deselect_all_button", "Deselect all")), actionButton("option_select_all_button", "Select all"), disabled(actionButton("option_clear_button", "Clear")), disabled(actionButton("option_add_button", "Add")), actionButton("option_ok_button", "Ok"), actionButton("option_close_button", "Close"))
				))
			}
			else { 
				if (o_click_button$display == 1) {
					eval(parse(text = paste0("v_var_name <- ", ifelse(input$data_type == "temporal", "o_parameter$y", ifelse(input$plot_type %in% c("boxplot", "barplot"), ifelse(!is.na(o_parameter$concat1_group), "o_parameter$concat1_group", "o_parameter$x"), ifelse(is.na(o_parameter$group), "NA", ifelse(!is.na(o_parameter$concat2_group), "o_parameter$concat2_group", "o_parameter$group")))))))
				}
				else {
					eval(parse(text = paste0("v_var_name <- ", ifelse(input$data_type == "temporal", "input$var_y", ifelse(input$plot_type %in% c("boxplot", "barplot"), "input$var_x", ifelse(input$group == "no", "NA", "input$var_group"))))))
				}
				
				eval(parse(text = paste0("df_all <- ", ifelse(input$data_type == "temporal", "NULL", ifelse(!"sub" %in% ls(e_data), "e_data$all", "e_data$sub")))))
				l_results <- f_update_graph_option_rv(o_name_option, input$edit_option, o_click_button$display, input$data_type, input$plot_type, v_var_name, df_all)
				if (length(l_results[[1]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", names(l_results[[1]]), " <- l_results[[1]][[", 1:length(l_results[[1]]), "]]"), collapse = "; ")))}
				if (length(l_results[[2]]) > 0) {eval(parse(text = paste(paste0("o_name_option$", l_results[[2]], " <- c()"), collapse = "; ")))}
				
				if (length(o_name_option$name) > 0) { # open color edition model dialog
					if (input$edit_option == "color/opacity") {
						o_option$data <- f_create_option_data(input$edit_option, "color", o_name_option, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
						
						showModal(modalDialog(title = "Edit color/opacity", 
							easyClose = F,
							size = "l",
							radioButtons("color_opacity", NULL, choices = c("color", "opacity"), selected = "color", inline = T), 
							HTML("<br>"),
							colourInput("custom_color", "Custom color:", "white"),
							hidden(numericInput("custom_opacity", "Custom opacity:", 1, min = 0, max = 1, step = 0.01, width = "150px")),  
							HTML("<br>"),
							DT::dataTableOutput("option_inventory"),
							footer = tagList(disabled(actionButton("option_deselect_all_button", "Deselect all")), actionButton("option_select_all_button", "Select all"), disabled(actionButton("option_clear_button", "Clear")), disabled(actionButton("option_add_button", "Add")), actionButton("option_ok_button", "Ok"), actionButton("option_close_button", "Close"))
						))
					}
					else { # point type/size
						o_option$plotly <- f_create_point_type_plotly(input$dim_num)
						o_option$data <- f_create_option_data(input$edit_option, "type", o_name_option, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
						eval(parse(text = paste0("v_choices <- ", ifelse(input$dim_num == "2d", "1:24", "1:8"))))
						
						showModal(modalDialog(title = "Edit point type/size", 
							easyClose = F,
							size = "l",
							radioButtons("point_type_size", NULL, choices = c("type", "size"), selected = "type", inline = T),
							HTML("<br>"),
							selectizeInput("custom_point_type", "Custom type:", choices = v_choices, selected = 1, width = "150px"),
							plotlyOutput("point_type_info", height = ifelse(input$dim_num == "2d", "150px", "50px")),
							hidden(numericInput("custom_point_size", "Custom size:", 6, min = 0, step = 1, width = "150px")),
							HTML("<br>"),
							DT::dataTableOutput("option_inventory"),
							footer = tagList(disabled(actionButton("option_deselect_all_button", "Deselect all")), actionButton("option_select_all_button", "Select all"), disabled(actionButton("option_clear_button", "Clear")), disabled(actionButton("option_add_button", "Add")), actionButton("option_ok_button", "Ok"), actionButton("option_close_button", "Close"))
						))
					}
				}
				else {
					showNotification(paste0(ifelse(input$data_type == "temporal", "Y", ifelse(input$plot_type %in% c("boxplot", "barplot"), "X", "Group")), " variable field is empty"), duration = 15, type = "error")
				}
			}
		}
	})
	
	observeEvent(input$option_select_all_button, {DT::selectRows(o_option_inventory_proxy, input$option_inventory_rows_all)})
	observeEvent(input$option_deselect_all_button, {DT::selectRows(o_option_inventory_proxy, NULL)})
	
	observeEvent(o_name_option$warning, {
		if (o_name_option$warning == 1) {showNotification("The first column is fixed until the graph is removed (color/opacity and point type/size)", duration = 15, type = "warning")}
	})
	
	observeEvent(input$color_opacity, {
		if (input$color_opacity == "color") {
			shinyjs::hide("custom_opacity")
			shinyjs::show("custom_color")
		}
		else {
			shinyjs::hide("custom_color")
			shinyjs::show("custom_opacity")
		}
		
		o_option$data <- f_create_option_data(input$edit_option, input$color_opacity, o_name_option, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
	})
	
	observeEvent(input$point_type_size, {
		if (input$point_type_size == "type") {
			shinyjs::hide("custom_point_size")
			shinyjs::show("custom_point_type")
			shinyjs::show("point_type_info")
		}
		else {
			shinyjs::hide("custom_point_type")
			shinyjs::hide("point_type_info")
			shinyjs::show("custom_point_size")
		}
		
		o_option$data <- f_create_option_data(input$edit_option, input$point_type_size, o_name_option, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
	})
	
	observeEvent(input$option_clear_button, {
		if ("all" %in% ls(e_data)) {
			if (input$edit_option == "label") {
				l_results <- f_update_option_data(input$edit_option, NULL, "clear", o_label_text, input$option_inventory_rows_selected)
			}
			else if (input$edit_option == "color/opacity") {
				l_results <- f_update_option_data(input$edit_option, input$color_opacity, "clear", o_name_option, input$option_inventory_rows_selected, NULL, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
			}
			else { # point type/size
				l_results <- f_update_option_data(input$edit_option, input$point_type_size, "clear", o_name_option, input$option_inventory_rows_selected, NULL, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
			}
			
			if (length(l_results[[1]]) > 0) {showNotification(l_results[[1]], duration = 15, type = "warning")} # warning message
			
			if (!is.null(l_results[[3]])) {
				eval(parse(text = paste0("o_", ifelse(input$edit_option == "label", "label_text$text", ifelse(input$edit_option == "color/opacity", ifelse(input$color_opacity == "color", "name_option$color", "name_option$opacity"), ifelse(input$point_type_size == "type", "name_option$point_type", "name_option$point_size"))), "_temp <- l_results[[2]]")))
				o_option$data <- l_results[[3]]
			}
		}
	})
	
	observeEvent(input$option_add_button, {
		if ("all" %in% ls(e_data)) {
			b_cond <- F
			
			if (input$edit_option == "label") {
				l_results <- f_update_option_data(input$edit_option, NULL, "add", o_label_text, input$option_inventory_rows_selected, input$custom_label)
			}
			else if (input$edit_option == "color/opacity") {
				if (input$color_opacity == "color") {
					l_results <- f_update_option_data(input$edit_option, input$color_opacity, "add", o_name_option, input$option_inventory_rows_selected, input$custom_color, input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
				}
				else { # opacity
					b_cond <- ifelse(is.na(input$custom_opacity), T, ifelse(input$custom_opacity < 0 | input$custom_opacity > 1, T, F))
					
					if (b_cond) {
						showNotification("Opacity must be a numeric value between 0 and 1", duration = 15, type = "error")
					}
					else {
						l_results <- f_update_option_data(input$edit_option, input$color_opacity, "add", o_name_option, input$option_inventory_rows_selected, as.character(input$custom_opacity), input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display) 
					}
				}
			}
			else { # point type/size
				if (input$point_type_size == "type") {
					l_results <- f_update_option_data(input$edit_option, input$point_type_size, "add", o_name_option, input$option_inventory_rows_selected, as.character(input$custom_point_type), input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display)
				}
				else { # size
					b_cond <- ifelse(is.na(input$custom_point_size), T, ifelse(input$custom_point_size <= 0, T, F))
					
					if (b_cond) {
						showNotification("Point size must be a numeric value strictly superior to 0", duration = 15, type = "error")
					}
					else {
						l_results <- f_update_option_data(input$edit_option, input$point_type_size, "add", o_name_option, input$option_inventory_rows_selected, as.character(input$custom_point_size), input$data_type, input$plot_type, ifelse(input$group == "yes", T, F), o_click_button$display) 
					}
				}
			}
			
			if (!b_cond) {
				if (length(l_results[[1]]) > 0) {showNotification(l_results[[1]], duration = 15, type = "warning")} # warning message
				
				if (!is.null(l_results[[3]])) {
					eval(parse(text = paste0("o_", ifelse(input$edit_option == "label", "label_text$text", ifelse(input$edit_option == "color/opacity", ifelse(input$color_opacity == "color", "name_option$color", "name_option$opacity"), ifelse(input$point_type_size == "type", "name_option$point_type", "name_option$point_size"))), "_temp <- l_results[[2]]")))
					o_option$data <- l_results[[3]]
				}
			}
		}
	})
	
	observeEvent(input$option_ok_button, {
		if ("all" %in% ls(e_data)) {
			if (input$edit_option == "label") {
				if ((length(which(o_label_text$text_temp == "")) == length(o_label_text$text_temp) & length(o_label_text$text) == 0) | paste(o_label_text$text_temp, collapse = "_") == paste(o_label_text$text, collapse = "_")) { # error message
					showNotification("No label is modified", duration = 15, type = "error")
				}
				else {
					if (length(which(o_label_text$text_temp == "")) == length(o_label_text$text_temp)) {
						eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- c()"), collapse = "; ")))
					}
					else {
						eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), " <- o_label_text$", c("label", "text"), "_temp"), collapse = "; ")))
					}
					
					if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
				}
				
				eval(parse(text = paste(paste0("o_label_text$", c("label", "text"), "_temp <- c()"), collapse = "; ")))
			}
			else if (input$edit_option == "color/opacity") {
				b_cond_1 <- (length(which(o_name_option$color_temp == "")) == length(o_name_option$color_temp) & length(o_name_option$color) == 0) | paste(o_name_option$color_temp, collapse = "_") == paste(o_name_option$color, collapse = "_")
				b_cond_2 <- (length(which(o_name_option$opacity_temp == "")) == length(o_name_option$opacity_temp) & length(o_name_option$opacity) == 0) | paste(o_name_option$opacity_temp, collapse = "_") == paste(o_name_option$opacity, collapse = "_")
				
				if (b_cond_1 & b_cond_2) { # error message
					showNotification("No color/opacity is modified", duration = 15, type = "error")
				}
				else {
					if (input$data_type == "ir") {o_cond$ok_color_opacity <- 1}
					if (!b_cond_1) {eval(parse(text = paste0("o_name_option$color <- ", ifelse(length(which(o_name_option$color_temp == "")) == length(o_name_option$color_temp), "c()", "o_name_option$color_temp"))))}
					if (!b_cond_2) {eval(parse(text = paste0("o_name_option$opacity <- ", ifelse(length(which(o_name_option$opacity_temp == "")) == length(o_name_option$opacity_temp), "c()", "o_name_option$opacity_temp"))))}
					if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
				}
				
				eval(parse(text = paste(paste0("o_name_option$", c("color", "opacity"), "_temp <- c()"), collapse = "; ")))
			}
			else { # point type/size
				b_cond_1 <- (length(which(o_name_option$point_type_temp == "")) == length(o_name_option$point_type_temp) & length(o_name_option$point_type) == 0) | paste(o_name_option$point_type_temp, collapse = "_") == paste(o_name_option$point_type, collapse = "_")
				b_cond_2 <- (length(which(o_name_option$point_size_temp == "")) == length(o_name_option$point_size_temp) & length(o_name_option$point_size) == 0) | paste(o_name_option$point_size_temp, collapse = "_") == paste(o_name_option$point_size, collapse = "_")
				
				if (b_cond_1 & b_cond_2) { # error message
					showNotification("No point type/size is modified", duration = 15, type = "error")
				}
				else {
					if (input$data_type == "ir") {o_cond$ok_point_type_size <- 1}
					if (!b_cond_1) {eval(parse(text = paste0("o_name_option$point_type <- ", ifelse(length(which(o_name_option$point_type_temp == "")) == length(o_name_option$point_type_temp), "c()", "o_name_option$point_type_temp"))))}
					if (!b_cond_2) {eval(parse(text = paste0("o_name_option$point_size <- ", ifelse(length(which(o_name_option$point_size_temp == "")) == length(o_name_option$point_size_temp), "c()", "o_name_option$point_size_temp"))))}
					if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
				}
				
				eval(parse(text = paste(paste0("o_name_option$", c("point_type", "point_size"), "_temp <- c()"), collapse = "; ")))
			}
			
			removeModal()
			o_option$data <- NULL
			if (input$edit_option == "point type/size") {o_option$plotly <- NULL}
		}
	})
	
	observeEvent(input$option_close_button, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			o_option$data <- NULL
			if (input$edit_option == "point type/size") {o_option$plotly <- NULL}
		}
	})
	
	# ----
	# Flag
	# ----
	
	# 3.11. Add events on "action" radio button 
	# =========================================
	
	observeEvent(input$action, { # temporal/ir data type
		if (isolate(o_click_button$display) == 1) {
			v_t3_id <- f_create_t3_input_id_vector(input$data_type)
			i_cond <- ifelse(length(v_t3_id) > 0, eval(parse(text = paste0("length(which(c(", paste(paste0("input$", v_t3_id), collapse = ", "), ")))"))), 0)
			
			if (i_cond == 0 & is.na(o_parameter$f) & is.na(o_parameter$g) & is.na(o_parameter$h)) {
				v_id <- c("qc", paste0(c("clear1", "clear2", "save"), "_button"), v_t3_id)
				v_status <- ifelse(input$action == "add_flag", 1, 0)
				
				if (length(e_current_flag) > 0) {
					if (input$data_type == "temporal") {
						if ("coord" %in% ls(e_current_flag)) {
							df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
							df_num <- df_num[-dim(df_num)[1],]
							names(df_num) <- c("name", "freq")
							v_num <- as.vector(df_num[df_num$freq == 1, "name"])
							v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
							i_object_num <- length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
						}
						else {
							i_object_num <- 0
						}
						
						if ("click_iter" %in% ls(e_current_flag)) {
							i_object_num <- i_object_num + 1
							eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
						}
						
						v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num  - 1)
					}
					else { # ir
						df_elt <- isolate(o_plot$elt)
						i_tot <- sum(rowSums(df_elt[, -1]))
						v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
					}
					
					s_num_del <- paste(v_num_del, collapse = ", ")
					eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
					rm(list = ls(e_current_flag), envir = e_current_flag)
					v_status <- c(v_status, rep(0, 3), rep(1, length(v_t3_id)))
				}
				
				names(v_status) <- v_id[1:length(v_status)]
				o_on_off$tp$id <- as.vector(o_input_status$val[o_input_status$val$id %in% v_id[1:length(v_status)], "id"])
				o_on_off$tp$status <- as.vector(v_status[o_on_off$tp$id])
				o_cond$update <- 1
				js$resetClick()
			}
		}
	})
	
	# 3.12. Add events on X/Y/Z variable check boxes 
	# ==============================================
	
	observeEvent(c(input$var_flag_1, input$var_flag_2), { # plot 2D/3D and no model (normal data type)
		if (isolate(o_click_button$display) == 1) {
			v_t3_id <- f_create_t3_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_"))
			eval(parse(text = paste0("i_cond <- length(which(c(", paste(paste0("input$", v_t3_id), collapse = ", "), ")))")))
			
			if (i_cond == 0 & is.na(o_parameter$f) & is.na(o_parameter$g) & is.na(o_parameter$h)) {
				if (length(e_current_flag) > 0) {
					v_num_del <- isolate(o_plot$elt):(isolate(o_plot$elt) + e_current_flag$num - 1)
					s_num_del <- paste(v_num_del, collapse = ", ")
					eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
					rm(list = ls(e_current_flag), envir = e_current_flag)
					o_on_off$tp$id <- as.vector(o_input_status$val[o_input_status$val$id %in% c(paste0(c("clear1", "clear2", "save"), "_button"), v_t3_id), "id"])
					o_on_off$tp$status <- ifelse(o_on_off$tp$id %in% paste0(c("clear1", "clear2", "save"), "_button"), 0, 1)
					js$resetClick()
				}
				
				o_cond$update <- 1
				
				if (input$dim_num == "2d") {
					if (is.null(input$var_flag_1)) {updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))}
				}
				else { # plot 3D
					if (is.null(input$var_flag_1) & input$var_flag_2 == F) {
						updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))
						updateCheckboxInput(session, "var_flag_2", value = T)
					}
				}
			}
		}
	})
	
	# 3.13. Add events on the "draw" radio button
	# ===========================================
	
	observeEvent(input$draw, { # temporal data type
		if (isolate(o_click_button$display) == 1 & is.na(o_parameter$f) & is.na(o_parameter$g) & is.na(o_parameter$h)) {
			if (input$draw == "pt" & "click_iter" %in% ls(e_current_flag)) {
				eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
				rm("click_iter", envir = e_current_flag)
				
				if ("coord" %in% ls(e_current_flag)) {
					e_current_flag$num <- e_current_flag$num - 1
					e_current_flag$all_coord <- e_current_flag$all_coord[-dim(e_current_flag$all_coord)[1],]
					df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
					df_num <- df_num[-dim(df_num)[1],]
					names(df_num) <- c("name", "freq")
					v_num <- as.vector(df_num[df_num$freq == 1, "name"])
					v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
					i_object_num <- length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
					i_num_del <- length(isolate(o_plot$elt)) + i_object_num
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "deleteTraces", list(i_num_del))
					eval(parse(text = paste(paste0("v_", c("id", "status"), " <- ", c("\"save_button\"", "1")), collapse = "; ")))
				}
				else {
					rm("num", envir = e_current_flag)
					rm("all_coord", envir = e_current_flag)
					i_num_del <- length(isolate(o_plot$elt))
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "deleteTraces", list(i_num_del))
					v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% paste0("clear", 1:2, "_button"), "id"])
					v_status <- rep(0, length(v_id))
				}
				
				eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))
				js$resetClick()
			}
			
			o_cond$update <- 1
		}
	})
	
	# 3.14. Add events of clear button
	# ================================
	
	observeEvent(input$clear1_button, {
		if (isolate(o_click_button$display) == 1) {
			v_id <- c()
			
			if (input$data_type == "normal") {
				v_num_del <- isolate(o_plot$elt) + e_current_flag$num - 1
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))
				
				if (e_current_flag$num > 1) {
					e_current_flag$coord <- e_current_flag$coord[-which(e_current_flag$coord$num == e_current_flag$num),]
					e_current_flag$num <- e_current_flag$num - 1
				}
				else {
					v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c(f_create_t3_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_")), paste0(c("clear1", "clear2", "save"), "_button")), "id"])
					v_status <- ifelse(v_id %in% paste0(c("clear1", "clear2", "save"), "_button"), 0, 1)
					rm(list = ls(e_current_flag), envir = e_current_flag)
				}
				
				js$resetClick()
			}
			else if (input$data_type == "temporal") {
				if ("click_iter" %in% ls(e_current_flag)) {
					eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
					
					if ("coord" %in% ls(e_current_flag)) {
						df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
						df_num <- df_num[-dim(df_num)[1],]
						names(df_num) <- c("name", "freq")
						v_num <- as.vector(df_num[df_num$freq == 1, "name"])
						v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
						v_num_del <- length(isolate(o_plot$elt)) + length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
						eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))	
						e_current_flag$all_coord <- e_current_flag$all_coord[-which(e_current_flag$all_coord$num == e_current_flag$num),]
						e_current_flag$num <- e_current_flag$num - 1
						rm("click_iter", envir = e_current_flag)
						eval(parse(text = paste(paste0("v_", c("id", "status"), " <- ", c("\"save_button\"", "1")), collapse = "; ")))
					}
					else {
						v_num_del <- length(isolate(o_plot$elt))
						eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))	
						rm(list = ls(e_current_flag), envir = e_current_flag)
						v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% paste0(c("clear1", "clear2", "save"), "_button"), "id"])
						v_status <- rep(0, length(v_id))
					}
					
					js$resetClick()
				}
				else {
					if ("coord" %in% ls(e_current_flag)) {
						df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
						df_num <- df_num[-dim(df_num)[1],]
						names(df_num) <- c("name", "freq")
						v_num <- as.vector(df_num[df_num$freq == 1, "name"])
						v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
						i_object_num <- length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
						s_num <- max(e_current_flag$coord$num)
						
						if (s_num %in% v_num) {
							if (e_current_flag$coord[e_current_flag$coord$num == s_num, "geom"] == "pt") {
								v_num_del <- length(isolate(o_plot$elt)) + i_object_num - 1
							}
							else {
								v_num_del <- (length(isolate(o_plot$elt)) + i_object_num - 3):(length(isolate(o_plot$elt)) + i_object_num - 1)
							}
						}
						else {
							v_num_del <- (length(isolate(o_plot$elt)) + i_object_num - 3):(length(isolate(o_plot$elt)) + i_object_num - 1)
						}
						
						s_num_del <- paste(v_num_del, collapse = ", ")
						eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
						
						if (e_current_flag$num > 1) {
							e_current_flag$coord <- e_current_flag$coord[-which(e_current_flag$coord$num == e_current_flag$num),]
							e_current_flag$all_coord <- e_current_flag$all_coord[-which(e_current_flag$all_coord$num == e_current_flag$num),]
							e_current_flag$num <- e_current_flag$num - 1
						}
						else {
							rm(list = ls(e_current_flag), envir = e_current_flag)
							v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% paste0(c("clear1", "clear2", "save"), "_button"), "id"])
							v_status <- rep(0, length(v_id))
						}
						
						js$resetClick()
					}
				}
			}
			else { # ir
				df_elt <- isolate(o_plot$elt)
				i_tot <- sum(rowSums(df_elt[, -1]))
				v_num_del <- i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1
				if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {v_num_del <- c(v_num_del - 1, v_num_del)}
				s_num_del <- paste(v_num_del, collapse = ", ")
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))
				
				if (dim(e_current_flag$coord)[1] > 1) {
					e_current_flag$coord <- e_current_flag$coord[-dim(e_current_flag$coord)[1],]
				}
				else {
					rm(list = ls(e_current_flag), envir = e_current_flag)
					v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% c(f_create_t3_input_id_vector(s_data = input$data_type), paste0(c("clear1", "clear2", "save"), "_button")), "id"])
					v_status <- ifelse(v_id %in% paste0(c("clear1", "clear2", "save"), "_button"), 0, 1)
				}
				
				js$resetClick()
			}
			
			if (length(v_id) > 0) {eval(parse(text = paste(paste0("o_on_off$tp$", c("id", "status"), " <- v_", c("id", "status")), collapse = "; ")))}
			o_cond$update <- 1
		}
	})
	
	# 3.15. Add events of clear all button
	# ====================================
	
	observeEvent(input$clear2_button, {
		if (length(e_current_flag) > 0) {
			if (input$data_type == "normal") {
				v_t3_id <- f_create_t3_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_"))
				v_num_del <- isolate(o_plot$elt):(isolate(o_plot$elt) + e_current_flag$num - 1)
			}
			else if (input$data_type == "temporal") {
				if ("coord" %in% ls(e_current_flag)) {
					df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
					df_num <- df_num[-dim(df_num)[1],]
					names(df_num) <- c("name", "freq")
					v_num <- as.vector(df_num[df_num$freq == 1, "name"])
					v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
					i_object_num <- length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
				}
				else {
					i_object_num <- 0
				}
				
				if ("click_iter" %in% ls(e_current_flag)) {
					i_object_num <- i_object_num + 1
					eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
				}
				
				v_t3_id <- c()
				v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num - 1)
			}
			else {
				df_elt <- isolate(o_plot$elt)
				i_tot <- sum(rowSums(df_elt[, -1]))
				v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
				v_t3_id <- f_create_t3_input_id_vector(s_data = input$data_type)
			}
			
			s_num_del <- paste(v_num_del, collapse = ", ")
			eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
			rm(list = ls(e_current_flag), envir = e_current_flag)
			o_on_off$tp$id <- as.vector(o_input_status$val[o_input_status$val$id %in% c(v_t3_id, paste0(c("clear1", "clear2", "save"), "_button")), "id"])
			o_on_off$tp$status <- ifelse(o_on_off$tp$id %in% paste0(c("clear1", "clear2", "save"), "_button"), 0, 1)
			o_cond$update <- 1
			js$resetClick()
		}
	})
	
	# 3.16. Add events of save button
	# ===============================
	
	observeEvent(input$save_button, {
		if (length(e_current_flag) > 0) {
			v_split_path <- unlist(strsplit(input$data_path1, split = "/"))
			if (is.na(isolate(o_flag$name))) {o_flag$name <- f_create_flag_data_name(input$data_type, v_split_path, isolate(o_parameter$id))}
			s_flag_path <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/", isolate(o_flag$name))
			
			if ("flag" %in% ls(e_data)) {
				b_cond_1 <- f_file_write(e_data$flag, s_flag_path)
			}
			else {
				b_cond_2 <- isolate(o_flag$name) %in% list.files(paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/"))
				
				if (b_cond_2 == T) {
					df_flag <- fread(file = s_flag_path)
					b_cond_1 <- f_file_write(df_flag, s_flag_path)
				}
				else {
					b_cond_1 <- NULL
				}
			}
			
			if (is.null(b_cond_1)) {
				if (input$action == "add_flag") {
					if (!"flag" %in% ls(e_data)) {o_cond$save1 <- 1}
				}
				
				if (input$data_type == "normal") {
					v_t3_id <- f_create_t3_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_"))
					eval(parse(text = paste0("e_data$flag <- f_save_current_flag_data(s_data_type = \"normal\", df_previous_flag = ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", df_current_flag = e_current_flag$coord, s_id_var = isolate(o_parameter$id))")))
					fwrite(e_data$flag, file = s_flag_path, sep = ",")
					v_num_del <- isolate(o_plot$elt):(isolate(o_plot$elt) + e_current_flag$num - 1)
				}
				else if (input$data_type == "temporal") {
					v_t3_id <- c()
					if ("data" %in% ls(e_previous_flag)) {rm("data", envir = e_previous_flag)}
					eval(parse(text = paste0("e_data$flag <- f_save_current_flag_data(\"temporal\", input$action, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", e_current_flag$coord, o_plot$data, NA, isolate(o_parameter$x), as.numeric(input$qc), input$comment)")))
					fwrite(e_data$flag, file = s_flag_path, sep = ",")
					
					if ("coord" %in% ls(e_current_flag)) {
						df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
						df_num <- df_num[-dim(df_num)[1],]
						names(df_num) <- c("name", "freq")
						v_num <- as.vector(df_num[df_num$freq == 1, "name"])
						v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
						i_object_num <- length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
					}
					else {
						i_object_num <- 0
					}
					
					if ("click_iter" %in% ls(e_current_flag)) {
						i_object_num <- i_object_num + 1
						eval(parse(text = f_update_rv(list("rv" = rep("o_click_graph", 2), "id" = paste0("prev_", c("date", "var")), "value" = rep("NULL", 2)))))
					}
					
					v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num - 1)
					eval(parse(text = f_update_rv(list("rv" = rep("o_plot", 3), "id" = c("elt_pt_pos", "add_pt", "var_pt"), "value" = rep("NA", 3)))))
				}
				else {
					v_t3_id <- f_create_t3_input_id_vector(s_data = input$data_type)
					eval(parse(text = paste0("e_data$flag <- f_save_current_flag_data(\"ir\", input$action, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", e_current_flag$coord, NULL, isolate(o_parameter$id), NA, as.numeric(input$qc), input$comment)")))
					fwrite(e_data$flag, file = s_flag_path, sep = ",")
					df_elt <- isolate(o_plot$elt)
					i_tot <- sum(rowSums(df_elt[, -1]))
					v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
				}
				
				s_num_del <- paste(v_num_del, collapse = ", ")
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))
				rm(list = ls(e_current_flag), envir = e_current_flag)
				o_on_off$tp$id <- c(v_t3_id, paste0(c(paste0("clear", 1:2), "save"), "_button"))
				o_on_off$tp$status <- ifelse(o_on_off$tp$id %in% paste0(c(paste0("clear", 1:2), "save"), "_button"), 0, 1) 
				js$resetClick()
				eval(parse(text = f_update_rv(list("rv" = c("o_plot", rep("o_cond", 3)), "id" = c("elt", "update", "save2", "display"), "value" = c("NA", rep("1", 3))))))
			}
			else {
				o_cond$update <- 1
				showNotification(paste0("Please close the flag file (", s_flag_path, ")"), duration = 15, type = "error")
			}
		}
	})
	
	# ----------
	# Statistics
	# ----------
	
	# 3.17. Add events on linear regression check box
	# ===============================================
	
	observeEvent(input$lreg, {
		if ("all" %in% ls(e_data)) {
			o_parameter$lreg <- input$lreg
			
			if (is.na(o_cond$stat)) {
				if (isolate(o_click_button$display) == 1) {
					if (input$lreg) {
						updateSelectizeInput(session, "lreg_info_elt", choices = c("lreg parameters", "R2", ifelse(input$model == "valid", "t-test on lreg parameters", "RMSE")))
						shinyjs::show(id = "sh_popup2")
					}
					else {
						if (input$lreg_info_xpos != "left") {
							updateRadioButtons(session, "lreg_info_xpos", selected = "left")
							o_lreg_info$xpos <- 0
						}
						
						if (input$lreg_info_ypos != "top") {
							updateRadioButtons(session, "lreg_info_ypos", selected = "top")
							o_lreg_info$ypos <- 1
						}
						
						updateSelectizeInput(session, "lreg_info_elt", choices = " ")
						o_lreg_info$elt <- NA
						shinyjs::hide(id = "sh_popup2")
						shinyjs::disable("reset3_button")
					}
					
					l_results <- f_update_traces_input("lreg", input$traces, o_cond$legend, o_input_status$val, o_parameter)
					if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
					o_cond$legend <- l_results[[2]]
					js$resetClick()
					eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("update", "stat", "display"), "value" = c("1", "\"lreg\"", "1")))))
				}
			}
			else {
				if (o_cond$stat == -1) {o_cond$stat <- NULL}
			}
		}
	})
	
	# 3.18. Add events on confidence ellipsoid check box
	# ==================================================
	
	observeEvent(input$conf_ellipsoid, {
		if ("all" %in% ls(e_data)) {
			o_parameter$conf_ellipsoid <- input$conf_ellipsoid
			
			if (is.na(o_cond$stat)) {
				if (isolate(o_click_button$display) == 1) {
					l_results <- f_update_traces_input("conf_ellipsoid", input$traces, o_cond$legend, o_input_status$val, o_parameter)
					if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
					o_cond$legend <- l_results[[2]]
					js$resetClick()
					eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("update", "stat", "display"), "value" = c("1", "\"conf_ellipsoid\"", "1")))))
				}
			}
			else {
				if (o_cond$stat == -1) {o_cond$stat <- NULL}
			}
		}
	})
	
	# 3.19. Add events on centroid check box
	# ======================================
	
	observeEvent(input$centroid, {
		if ("all" %in% ls(e_data)) {
			o_parameter$centroid <- input$centroid
			
			if (is.na(o_cond$stat)) {
				if (isolate(o_click_button$display) == 1) {
					l_results <- f_update_traces_input("centroid", input$traces, o_cond$legend, o_input_status$val, o_parameter)
					if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
					o_cond$legend <- l_results[[2]]
					js$resetClick()
					eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("update", "stat", "display"), "value" = c(1, 0, 1)))))
				}
			}
		}
	})
	
	# 3.20. Add events on mean/sd check box
	# =====================================
	
	observeEvent(input$box_mean_sd, {
		if ("all" %in% ls(e_data)) {
			o_parameter$boxmean <- ifelse(input$box_mean_sd, "\"sd\"", "NULL")
			if (isolate(o_click_button$display) == 1) {eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))}
		}
	})
	
	# 3.21. Add events on density curve check box
	# ===========================================
	
	observeEvent(input$dens_curve, {
		if ("all" %in% ls(e_data)) {
			o_parameter$dens_curve <- input$dens_curve
			
			if (is.na(o_cond$stat)) {
				if (isolate(o_click_button$display) == 1) {
					l_results <- f_update_traces_input("dens_curve", input$traces, o_cond$legend, o_input_status$val, o_parameter)
					if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
					o_cond$legend <- l_results[[2]]
					eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("update", "stat", "display"), "value" = c("1", "\"dens_curve\"", "1")))))
				}
			}
			else {
				if (o_cond$stat == -1) {o_cond$stat <- NULL}
			}
		}
	})
	
	# 3.22. Add events on normal density curve check box 
	# ==================================================
	
	observeEvent(input$norm_dens_curve, {
		if ("all" %in% ls(e_data)) {
			o_parameter$norm_dens_curve <- input$norm_dens_curve
			
			if (is.na(o_cond$stat)) {
				if (isolate(o_click_button$display) == 1) {
					l_results <- f_update_traces_input("norm_dens_curve", input$traces, o_cond$legend, o_input_status$val, o_parameter)
					if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
					o_cond$legend <- l_results[[2]]
					eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 3), "id" = c("update", "stat", "display"), "value" = c("1", "\"norm_dens_curve\"", "1")))))
				}
			}
			else {
				if (o_cond$stat == -1) {o_cond$stat <- NULL}
			}
		}
	})
	
	# 3.23. Add events on mean spectrum check box
	# ===========================================
	
	observeEvent(input$mean_spect, {
		if (length(ls(e_data)) > 0) {
			o_parameter$mean_spect <- input$mean_spect
			
			if (isolate(o_click_button$display) == 1) {
				l_results <- f_update_traces_input("mean_spect", input$traces, o_cond$legend, o_input_status$val, o_parameter)
				if (length(l_results[[1]]) > 0) {eval(parse(text = l_results[[1]]))}
				o_cond$legend <- l_results[[2]]
				eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			}
		}
	})
	
	# ==========
	# Main panel
	# ==========
	
	# 4.1. Add events on picture details "ok" button
	# ==============================================
	
	observeEvent(input$ok1_button, {
		if (isolate(o_click_button$display) == 1) {
			o_picture_info$filename <- input$picture_name
			o_picture_info$format <- input$picture_format 
			o_picture_info$height <- ifelse(is.na(input$picture_height), 800, input$picture_height)
			o_picture_info$width <- ifelse(is.na(input$picture_width), 1000, input$picture_width)
			eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			toggleModal(session, "picture_info", toggle = "close")
		}
	})
	
	# 4.2. Add conditions on picture height/width inputs 
	# ==================================================
	
	observe({
		if (!is.numeric(input$picture_height) | input$picture_height < 100 | is.na(input$picture_height)) {updateNumericInput(session, "picture_height", value = ifelse(!is.numeric(input$picture_height) | is.na(input$picture_height), 800, 100))}
		if (!is.numeric(input$picture_width) | input$picture_width < 300 | is.na(input$picture_width)) {updateNumericInput(session, "picture_width", value = ifelse(!is.numeric(input$picture_width) | is.na(input$picture_width), 1000, 300))}
	})
	
	# 4.3. Add events on "reset axes" button
	# ======================================
	
	observeEvent(input$reset2_button, {
		if (isolate(o_click_button$display) == 1) {
			if (input$data_type %in% c("normal", "ir")) {
				if (input$data_type == "normal") {
					o_zoom$coord <- NULL
					o_cond$reset2 <- 1
					if (isolate(o_parameter$plot_type) == "plot") {
						s_xlab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "x")] != "", o_label_text$text[which(o_label_text$label == "x")], o_parameter$xlab), o_parameter$xlab)
						s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = s_xlab, range = NULL), yaxis = list(title = s_ylab, range = NULL)))
					}
					
					if (isolate(o_parameter$plot_type) == "histplot") {
						s_xlab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "x")] != "", o_label_text$text[which(o_label_text$label == "x")], o_parameter$xlab), o_parameter$xlab)
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = s_xlab, range = NULL), yaxis = list(title = "Density", range = NULL)))
					} 
				}
				else {
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = "Frequency (cm-1)", rangeslider = list(thickness = 0.1, borderwidth = 1), range = rev(range(as.vector(o_plot$data$Frequency))))))
				}
			}
			
			o_cond$update <- 1
		}
	})
	
	# 4.4. Add events on linear regression informations "ok" button
	# =============================================================
	
	observeEvent(input$ok2_button, {
		if (isolate(o_click_button$display) == 1) {
			if (is.null(input$lreg_info_elt)) {
				showNotification("Information field is empty", duration = 15, type = "error")
			}
			else {
				o_lreg_info$xpos <- ifelse(input$lreg_info_xpos == "left", 0, 1)
				o_lreg_info$ypos <- ifelse(input$lreg_info_ypos == "top", 1, 0)
				o_lreg_info$elt <- input$lreg_info_elt
				shinyjs::enable("reset3_button")
				eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
				toggleModal(session, "lreg_info", toggle = "close")
			}
		}
	})
	
	# 4.5. Add events on linear regression informations "reset" button
	# ================================================================
	
	observeEvent(input$reset3_button, {
		if (isolate(o_click_button$display) == 1) {
			eval(parse(text = f_update_rv(list("rv" = rep("o_lreg_info", 3), "id" = c("xpos", "ypos", "elt"), "value" = c("0", "1", "NA")))))
			shinyjs::disable("reset3_button")
			eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			toggleModal(session, "lreg_info", toggle = "close")
		}
	})
	
	# 4.6. Add events on graph selection input
	# ========================================
	
	observeEvent(input$select_graph, {
		if (isolate(o_click_button$display) == 1 & !is.na(o_parameter$select_graph)) {
			if (isolate(o_cond$select_graph1) == 0) {
				o_cond$select_graph1 <- 1
			}
			else {
				if (!is.na(isolate(o_parameter$group)) & paste(isolate(o_parameter$model), input$select_graph, sep = "_") == "calib_QQplot") {o_cond$select_graph2 <- 1}
				o_parameter$select_graph <- input$select_graph
				if (length(o_label_text$label) > 0) {eval(parse(text = f_update_rv(list("rv" = rep("o_label_text", 2), "id" = c("label", "text"), "value" = rep("c()", 2)))))} # delete custom labels
				eval(parse(text = f_update_rv(list("rv" = rep("o_cond", 2), "id" = c("update", "display"), "value" = rep(1, 2)))))
			}
		}
	})
	
	# 4.7. Add events on mouse click  
	# ==============================
	
	output$click_corplot <- renderPlotly(o_click_corplot$plotly)
	
	o_graphic_click <- observeEvent(event_data("plotly_click", source = "graphic"), suspended = T, {	
		if (isolate(o_click_button$display) == 1) {
			o_click_ev <- event_data("plotly_click", source = "graphic")
			
			shinyjs::delay(100, {
				v_id <- c()
				
				if (input$data_type == "normal") {
					if (input$plot_type == "plot" & input$model == "none") {
						if ((o_click_ev[["curveNumber"]] + 1) <= isolate(o_plot$elt) & isolate(o_parameter$lreg) == F & isolate(o_parameter$conf_ellipsoid) == F & isolate(o_parameter$centroid) == F & input$f_radio == "no" & input$g_radio == "no" & input$h_radio == "no") {
							eval(parse(text = paste0("v_sub_row <- ", ifelse("sub" %in% ls(e_data), "isolate(o_sdata_cond$row_num)", "NULL"))))
							l_click_info <- f_create_click_info(s_data_type = "normal", s_dim_num = input$dim_num, v_xy_flag = input$var_flag_1, b_z_flag = input$var_flag_2, s_flag_name = isolate(o_flag$name), e_data = e_data, v_sub_row = v_sub_row, o_click_ev = o_click_ev, o_parameter = o_parameter, o_plot = NULL, o_cond = NULL)
							i_click <- 0
							
							if ("coord" %in% ls(e_current_flag)) {
								if (length(which(l_click_info[[3]] %in% e_current_flag$coord$id)) > 0) {i_click <- 1}
							}
							
							if (i_click == 0) {
								eval(parse(text = paste0("l_results <- f_create_current_flag_data(s_data_type = \"normal\", l_click_info = l_click_info, df_previous_flag = ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", e_current_flag = e_current_flag)"))) # create current flag data
								
								if (!"message" %in% names(l_results)) {
									eval(parse(text = paste(paste0("e_current_flag$", names(l_results), " <- l_results[[", 1:length(l_results), "]]"), collapse = "; ")))
									
									ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "addTraces", f_add_current_flag("normal", input$dim_num, NULL, o_parameter, NULL, o_click_ev, e_current_flag$num)) # add current flag on the graph
									ply_1
									v_id <- c(v_id, paste0(c(paste0("clear", 1:2), "save"), "_button"))
									
									if (e_current_flag$num == 1) {
										showNotification("Statistic tab inputs are disabled when new flags are added on the graph", duration = 15, type = "warning")
										v_id <- c(v_id, f_create_t3_input_id_vector(s_graph = paste(input$plot_type, input$dim_num, sep = "_")))
									}
								}
								else {
									showNotification(l_results$message, duration = 15, type = "warning")
									js$resetClick()
								}
								
								rm(list = "l_results")
							}
						}
					}
					else {
						if (input$plot_type == "corplot") {
							df_all <- f_create_xy_cell_data(isolate(o_plot$data), o_parameter, o_click_ev[["x"]], o_click_ev[["y"]])
							
							if (dim(df_all)[1] > 0 & which(isolate(o_parameter$y) %in% o_click_ev[["x"]]) < which(isolate(o_parameter$y) %in% o_click_ev[["y"]])) {
								o_click_corplot$num <- 0
								o_click_corplot$data <- df_all
								o_click_corplot$plotly <- suppressMessages(plotly_build(f_create_xy_cell_plotly(o_click_corplot$data,  o_click_corplot$num)))
								
								showModal(modalDialog(
									title = paste0("cor = ", round(cor(df_all[, 1], df_all[, 2]), digits = 2), ", size = ", dim(df_all)[1]),
									easyClose = F,
									size = "m",
									plotlyOutput("click_corplot"),
									footer = tagList(actionButton("prev_button", icon("angle-left")), actionButton("next_button", icon("angle-right")), actionButton("click_corplot_close_button", "Close"))
								))
							}
							
							js$resetClick()
						}
					}
				}
				else if (input$data_type == "temporal") {	
					if (input$g_radio == "no") {
						v_date_ms <- as.vector(o_plot$data[, paste0(isolate(o_parameter$x), "_trf")]) 
						s_x_click <- unique(o_plot$data[which(v_date_ms == as.numeric(as.POSIXct(o_click_ev[["x"]], tz = "GMT"))), isolate(o_parameter$x)]) 
						
						if ((o_click_ev[["curveNumber"]] + 1) <= length(isolate(o_plot$elt))) {
							v_pos <- c()
							i_qc <- 0
							
							if ("data" %in% ls(e_previous_flag)) {
								v_x_pos <- which(e_previous_flag$data$x == s_x_click)
								
								if (length(v_x_pos ) > 0) {
									v_y_pos <- which(e_previous_flag$data$y[v_x_pos] == o_click_ev[["y"]])
									
									if (length(v_y_pos) > 0) {
										v_pos <- which(as.vector(e_previous_flag$data$var_name[v_x_pos[v_y_pos]]) == isolate(o_plot$elt[o_click_ev[["curveNumber"]] + 1]))
										if (length(v_pos) > 0) {i_qc <- as.vector(e_previous_flag$data$qc[v_x_pos[v_y_pos[v_pos]]])}
									}
								}
							}
							
							if (input$action == "add_flag" & i_qc == 1) {
								showNotification("Flag already exists", duration = 15, type = "warning")
								js$resetClick()
							}
							
							if ((input$action == "add_flag" & length(v_pos) == 0) | (input$action == "replace_qc" & length(v_pos) == 1 & i_qc == 1)) {
								i_click <- 0
								
								if ("all_coord" %in% ls(e_current_flag)) {
									if (length(which(e_current_flag$all_coord$x == s_x_click & e_current_flag$all_coord$var_name == isolate(o_plot$elt[o_click_ev[["curveNumber"]] + 1]))) > 0) {i_click <- 1}
								}
								
								if (i_click == 0) {
									l_click_info <- list(s_x_click, o_click_ev[["y"]], isolate(o_plot$elt[o_click_ev[["curveNumber"]] + 1]))
									eval(parse(text = paste0("l_results <- f_create_current_flag_data(\"temporal\", input$action, input$draw, isolate(o_parameter$x), l_click_info, ", ifelse("data" %in% ls(e_previous_flag), "e_previous_flag$data", "NULL"), ", e_current_flag, o_click_graph, o_plot)"))) # create current flag data
									
									if (length(l_results$message) == 0) {
										l_pos <- list(which(names(l_results) %in% c("num", "coord", "all_coord", "click_iter")), which(names(l_results) %in% c("prev_date", "prev_var")), which(names(l_results) %in% c("marker", "line")))
										if (sum(lengths(l_pos[1:2])) > 0) {eval(parse(text = paste(paste0(c(rep("e_current_flag", length(l_pos[[1]])), rep("o_click_graph", length(l_pos[[2]]))), "$", names(l_results)[c(l_pos[[1]], l_pos[[2]])], " <- l_results$", names(l_results)[c(l_pos[[1]], l_pos[[2]])]), collapse = "; ")))}
										l_temp_coord <- NULL
										
										if (length(l_pos[[3]]) > 0) {
											rm("click_iter", envir = e_current_flag)
											l_temp_coord <- list(l_results$marker, l_results$line)
										}
										
										ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "addTraces", f_add_current_flag("temporal", NULL, l_temp_coord, o_parameter, o_plot, o_click_ev, e_current_flag$num)) # add current flag on the graph
										ply_1
										v_id <- c(v_id, l_results$button_name)
									}
									else {
										showNotification(l_results$message, duration = 15, type = "warning")
									}
									
									rm(list = "l_results")
								}
							}
						}
					}
				}
				else {
					if (isolate(o_parameter$mean_spect) == F) {
						l_results <- f_create_click_info(s_data_type = "ir", s_action = input$action, o_click_ev = o_click_ev, o_plot = o_plot, o_cond = o_cond)
						
						if (l_results[[3]]) {
							i_click <- 0
							
							if ("coord" %in% ls(e_current_flag)) {
								if (length(which(l_results[[1]] %in% e_current_flag$coord$id)) > 0) {i_click <- 1}
							}
							else {
								showNotification("Statistic tab inputs are disabled when new flags are added on the graph", duration = 15, type = "warning")
								v_id <- c(v_id, paste0(c(paste0("clear", 1:2), "save"), "_button"), f_create_t3_input_id_vector(s_data = input$data_type))
							}
							
							if (i_click == 0) {
								l_click_info <- l_results[1:2]
								e_current_flag$coord <- f_create_current_flag_data(s_data_type = "ir", l_click_info = l_click_info, e_current_flag = e_current_flag, o_plot = o_plot) # create current flag
								ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "addTraces", f_add_current_flag(s_data_type = "ir", o_parameter = o_parameter, o_plot = o_plot, o_click_ev = NULL, i_flag_num = dim(e_current_flag$coord)[1], l_click_info = l_click_info)) # add current flag on the graph
								ply_1
							}
						}
						else {
							if (length(l_results[[4]]) > 0) {
								showNotification(l_results[[4]], duration = 15, type = "warning")
								js$resetClick()
							}
						}
						
						rm(list = "l_results")
					}
				}
				
				if (length(v_id) > 0) {
					v_id <- as.vector(o_input_status$val[o_input_status$val$id %in% v_id, "id"])
					o_on_off$val <- f_update_input_status_list(f_create_input_status_list(v_id, ifelse(v_id %in% paste0(c(paste0("clear", 1:2), "save"), "_button"), 1, 0)), o_input_status$val)
				}
			})
		}
	})
	
	observeEvent(input$click_corplot_close_button, {
		if ("all" %in% ls(e_data)) {
			removeModal()
			eval(parse(text = f_update_rv(list("rv" = rep("o_click_corplot", 3), "id" = c("num", "data", "plotly"), "value" = rep("NULL", 3)))))
		}
	})
	
	# 4.8. Add events on zoom 
	# =======================
	
	o_graphic_relayout <- observeEvent(event_data("plotly_relayout", source = "graphic"), suspended = T, {
		if (isolate(o_click_button$display) == 1) {
			o_zoom_ev <- event_data("plotly_relayout", source = "graphic")
			
			if (length(names(o_zoom_ev)) > 0 & length(which("width" %in% names(o_zoom_ev))) == 0) {
				if (!is.na(isolate(o_parameter$plot_type))) {
					if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
						if (!"dragmode" %in% names(o_zoom_ev)) {
							if (length(which(c("scene.camera", "scene.camera.up") %in% names(o_zoom_ev))) > 0) {
								o_zoom$coord <- list()
								
								if (names(o_zoom_ev)[1] == "scene.camera") {
									o_zoom$coord[[1]] <- list(o_zoom_ev$`scene.camera`$up$x, o_zoom_ev$`scene.camera`$up$y, o_zoom_ev$`scene.camera`$up$z)
									o_zoom$coord[[2]] <- list(o_zoom_ev$`scene.camera`$center$x, o_zoom_ev$`scene.camera`$center$y, o_zoom_ev$`scene.camera`$center$z)
									o_zoom$coord[[3]] <- list(o_zoom_ev$`scene.camera`$eye$x, o_zoom_ev$`scene.camera`$eye$y, o_zoom_ev$`scene.camera`$eye$z)
									names(o_zoom$coord[[1]]) <- c("x", "y", "z")
									names(o_zoom$coord[[2]]) <- c("x", "y", "z")
									names(o_zoom$coord[[3]]) <- c("x", "y", "z")
								}
								else {
									if (is.null(o_zoom_ev[[1]])) {
										o_zoom$coord <- NULL
									}
									else {
										o_zoom$coord[[1]] <- o_zoom_ev$`scene.camera.up`
										o_zoom$coord[[2]] <- o_zoom_ev$`scene.camera.center`
										o_zoom$coord[[3]] <- o_zoom_ev$`scene.camera.eye`
									}
								}
							}
							else {
								if (!"xaxis.range[0]" %in% names(o_zoom_ev) | !"yaxis.range[0]" %in% names(o_zoom_ev)) {
									if (is.null(isolate(o_zoom$coord))) {
										if (isolate(o_cond$reset2) != 1) {
											o_zoom$coord <- list()
											
											if (!"xaxis.range[0]" %in% names(o_zoom_ev)) {
												o_zoom$coord[[1]] <- NA
												o_zoom$coord[[2]] <- c(o_zoom_ev$`yaxis.range[0]`, o_zoom_ev$`yaxis.range[1]`)
											}
											else {
												o_zoom$coord[[1]] <- c(o_zoom_ev$`xaxis.range[0]`, o_zoom_ev$`xaxis.range[1]`)
												o_zoom$coord[[2]] <- NA
											}
										}
										else {
											o_cond$reset2 <- 0
										}
									}
									else {
										if (!"xaxis.range[0]" %in% names(o_zoom_ev)) {
											o_zoom$coord[[2]] <- c(o_zoom_ev$`yaxis.range[0]`, o_zoom_ev$`yaxis.range[1]`)
										}
										else {
											o_zoom$coord[[1]] <- c(o_zoom_ev$`xaxis.range[0]`, o_zoom_ev$`xaxis.range[1]`)
										}
									}
								}
								else {
									o_zoom$coord <- list()
									o_zoom$coord[[1]] <- c(o_zoom_ev$`xaxis.range[0]`, o_zoom_ev$`xaxis.range[1]`)
									o_zoom$coord[[2]] <- c(o_zoom_ev$`yaxis.range[0]`, o_zoom_ev$`yaxis.range[1]`)
								}
							}
						}
					}
				}
				else {
					if (length(which("yaxis" %in% names(o_zoom_ev))) == 0) {
						if (length(which(c("xaxis.range", "xaxis.autorange") %in% names(o_zoom_ev))) == 0) {
							o_zoom$coord <- c(o_zoom_ev$`xaxis.range[0]`, o_zoom_ev$`xaxis.range[1]`)
							
							if (length(unique(isolate(o_zoom$coord))) == 1) {
								if (input$data_type == "temporal") {
									o_zoom$coord <- NULL
								}
								else {
									o_zoom$coord <- rev(range(as.vector(o_plot$data$Frequency)))
								}
							}
						}
						else {
							if (length(which("xaxis.range" %in% names(o_zoom_ev))) > 0) {
								o_zoom$coord <- o_zoom_ev$`xaxis.range`
								
								if (length(unique(isolate(o_zoom$coord))) == 1) {
									if (input$data_type == "temporal") {
										o_zoom$coord <- NULL
									}
									else {
										o_zoom$coord <- rev(range(as.vector(o_plot$data$Frequency)))
									}
								}
							}
							else {
								if (input$data_type == "temporal") {
									o_zoom$coord <- NULL
								}
								else {
									o_zoom$coord <- rev(range(as.vector(o_plot$data$Frequency)))
								}
							}
						}
						
						if (input$y_scale == "auto") {
							df_click_legend <- isolate(o_click_legend$item)
							
							if (is.list(input$traces)) {
								v_name <- names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")]
								df_click_legend$statut <- ifelse(df_click_legend$name %in% v_name, "\"legendonly\"", "T")
							}
							
							o_plot$y_coord <- NULL
							eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data[[isolate(o_parameter$data_name)]], o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", df_click_legend, input$fraction, NULL)")))
							o_plot$y_coord <- l_results[[2]]
							s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
							ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = s_ylab, range = l_results[[1]])))
							if (length(l_results[[3]]) > 0) {showNotification(l_results[[3]], duration = 15, type = "warning")}
							rm(list = "l_results")
						}
					}
				}
			}
		}
	})
	
	# 4.9. Add events on check/uncheck legend items
	# =============================================
	
	observeEvent(input$traces, {
		if (isolate(o_click_button$display) == 1) {
			if (input$data_type == "normal") {
				if (isolate(o_cond$legend) == 1) {o_cond$legend <- 0}
			}
			else {
				if (input$y_scale == "auto") {
					df_click_legend <- isolate(o_click_legend$item)
					
					if (is.list(input$traces)) {
						v_name <- names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")]
						df_click_legend$statut <- ifelse(df_click_legend$name %in% v_name, "\"legendonly\"", "T")
					}
					
					o_plot$y_coord <- NULL
					eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data[[isolate(o_parameter$data_name)]], o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", df_click_legend, input$fraction, NULL)")))
					o_plot$y_coord <- l_results[[2]]
					s_ylab <- ifelse(length(o_label_text$text) > 0, ifelse(o_label_text$text[which(o_label_text$label == "y")] != "", o_label_text$text[which(o_label_text$label == "y")], o_parameter$ylab), o_parameter$ylab)
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = s_ylab, range = l_results[[1]])))
					if (length(l_results[[3]]) > 0) {showNotification(l_results[[3]], duration = 15, type = "warning")}
					rm(list = "l_results")
				}
			}
		}
	})
	
	# 4.10. Add events on previous/next buttons (modal dialog: plotly_click)
	# =========================================
	
	observeEvent(input$prev_button, {
		o_click_corplot$num <- isolate(o_click_corplot$num) - 1 
		if (isolate(o_click_corplot$num) == (-1)) {o_click_corplot$num <- 3}
		o_click_corplot$plotly <- suppressMessages(plotly_build(f_create_xy_cell_plotly(o_click_corplot$data,  o_click_corplot$num)))
	})
	
	observeEvent(input$next_button, {
		o_click_corplot$num <- isolate(o_click_corplot$num) + 1 
		if (isolate(o_click_corplot$num) == 4) {o_click_corplot$num <- 0}
		o_click_corplot$plotly <- suppressMessages(plotly_build(f_create_xy_cell_plotly(o_click_corplot$data,  o_click_corplot$num)))
	})
	
	# ===
	# End
	# ===
	
	# Close session
	# =============
	
	session$onSessionEnded(function() {stopApp()})
}
