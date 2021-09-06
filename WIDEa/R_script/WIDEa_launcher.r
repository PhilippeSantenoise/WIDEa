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
# Description: Creating/launching of the web-interface
#
# Creation date: August 2019
####################################################################################


#===================================================================================
# Syntax
#===================================================================================

# The name of variables start with a code designating the following types:
# i = integer
# n = numeric
# s = string (single/double quotes)
# df = data.frame
# m = matrix
# v = vector
# l = list
# b = boolean
# f = function
# e = environment
# o = user's interface (fluidPage), reactiveValues, eventdata and shinyApp (available with R package shiny)  
# ply = plotly


#===================================================================================
# Loading R packages
#===================================================================================

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyFiles)
library(shinythemes)
library(shinybusy)
library(V8)

library(plotly)
library(htmltools)
library(htmlwidgets)

library(bindrcpp)

library(scales) # color brewer

library(data.table)
library(arrangements)

library(car) # levene test


#===================================================================================
# Loading R scripts (functions used by the server)
#===================================================================================

# Utils
# =====

source(paste0(s_WIDEa_path, "R_script/utils.r"))

# Left panel
# ==========

# Display button
# --------------

# Checking process:
v_rscript_name <- c("variables", "trsf_variables", "model")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/data/checking_process/check_", v_rscript_name, ".r\")"), collapse = "; ")))

# Data:
v_rscript_name <- c("create_date_variable", "prepare_data", "create_model_data")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/data/", v_rscript_name, ".r\")"), collapse = "; ")))

# Graph:
v_rscript_name <- c("check_graphic_options", "create_color_palette", "create_element_data", "build_legend_items", "edit_axis_layout", "calculate_y_axis_range", "build_graph", "add_flag")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/graph/", v_rscript_name, ".r\")"), collapse = "; ")))

# Statistics:
v_rscript_name <- c("lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve", "mean_spect")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/graph/statistics/add_", v_rscript_name, ".r\")"), collapse = "; ")))

# Top panel
# =========

# Save button
# -----------

v_rscript_name <- c("create_flag_data_name", "save_current_flag_data")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/save_button/", v_rscript_name, ".r\")"), collapse = "; ")))

# Main panel
# ==========

# Plotly click
# ------------

v_rscript_name <- c("create_click_info", "create_current_flag_data", "add_current_flag", "create_modal_data", "build_modal_graph")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/plotly_click/", v_rscript_name, ".r\")"), collapse = "; ")))


#===================================================================================
# Creating Operating System parameter data
#===================================================================================
# Parameters are used in the User's Interface 

s_OS_name <- Sys.info()[[1]]
df_UI_param <- data.frame("Parameters" = c("lp_text_input", "lp_multi_rb_top_margin", "lp_button_gap", "multi_rb_margin_after", "tp_multi_rb_top_margin_1", "tp_multi_rb_top_margin_2", "tp_multi_rb_top_margin_3", "tp_multi_rb_right_margin_1", "tp_multi_rb_right_margin_2", "tp_multi_rb_right_margin_3", "tp_gap_1", "tp_gap_2"), "Windows" = c(244, 0, 0, 0, 0, 10, 0, -22, -3, -42, 0, 0.7), "Darwin" = c(260, 6, 1.05, 3, 3, 12, 10, 0, 0, 0, 1.5, 0.2), "Linux" = c(260, 6, 1.05, 3, 3, 12, 10, 0, 0, 0, 1.5, 0.2))


#===================================================================================
# Loading web-interface
#===================================================================================

# I) Creating the User's Interface
# ================================

# Change the background color of an element
s_js_1 <- 'shinyjs.backgroundCol = function(params) {
	var defaultParams = {id : null, col : "white"};
	params = shinyjs.getParams(params, defaultParams);
	var el = $("#" + params.id);
	el.css("background-color", params.col);
}'

# Save name with status when a legend item is checked/unchecked (visible/not visible on graph) 
s_js_2 <- c(
"function(el, x) {",
"	var d3 = Plotly.d3;",
"	el.on('plotly_restyle', function(evtData) {",
"		var out = {};",
"		d3.select('g.legend').selectAll('.traces').each(function() {",
"			var trace = d3.select(this)[0][0].__data__[0].trace;",
"			out[trace.name] = trace.visible;",
"		});",
"		Shiny.setInputValue('traces', out);",
"	});",
"}")

# Resize graph window
s_js_3 <- '
	var dimension = [0, 0];
	$(document).on("shiny:connected", function(e) {
		dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
		dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
    });
'

# Selectize input parameters (UI)
s_css_1 <- "select ~ 
.selectize-control .selectize-input {
	max-height: 36px;
	overflow-y: auto;
}"

# Multi-column checkbox/radiobutton class (UI) 
s_css_2 <- '.multicol1 .shiny-options-group{-webkit-column-count: 1; -moz-column-count: 1; column-count: 1; -moz-column-fill: balanced; -column-fill: balanced;}'	
s_css_3 <- '.multicol2 .shiny-options-group{-webkit-column-count: 2; -moz-column-count: 2; column-count: 2; -moz-column-fill: balanced; -column-fill: balanced;}'
s_css_4 <- '.multicol3 .shiny-options-group{-webkit-column-count: 3; -moz-column-count: 3; column-count: 3; -moz-column-fill: balanced; -column-fill: balanced;}'

# Remove rangeslider grabber for X axis (temporal/IR) 
s_css_5 <- '.rangeslider-grabber-min {display: none; !important} .rangeslider-grabber-max {display: none; !important}'

# Notification panel width
s_css_6 <- '#shiny-notification-panel{width: 500px;}'

o_ui <- fluidPage(theme = shinytheme("flatly"),
	useShinyjs(),
	extendShinyjs(text = s_js_1, functions = c("backgroundCol")),
	extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-graphic', 'null'); }", functions = c("resetClick")), # reset click on graph
	extendShinyjs(text = "shinyjs.resetClick_leg = function() { Shiny.onInputChange('traces', 'null'); }", functions = c("resetClick_leg")),           # reset status of legend items
	
	# Reload name/status of legend items
	extendShinyjs(text = "shinyjs.reloadClick_leg = function() {
		var d3 = Plotly.d3; 
		var out = {};
		d3.select('g.legend').selectAll('.traces').each(function() {
			var trace = d3.select(this)[0][0].__data__[0].trace;
			out[trace.name] = trace.visible;
		});	
		Shiny.onInputChange('traces', out); 
	}", functions = c("reloadClick_leg")),
	
	tags$style(s_css_1),
	tags$head(tags$style(HTML(s_css_2))),
	tags$head(tags$style(HTML(s_css_3))),
	tags$head(tags$style(HTML(s_css_4))),
	tags$head(tags$style(HTML(s_css_5))),
	tags$head(tags$style(HTML(s_css_6))),
	tags$head(tags$script(s_js_3)),
	
	# Add a loading spinner
	add_busy_spinner(spin = "double-bounce", color = "#FFFFFF", position = "top-left", timeout = 100, margins = c(0, 70), height = "40px", width = "55px"),
	
	# Style class of top-sidebar and main panel 
	shinyjs::inlineCSS(list(.tpanel_class1 = "overflow: auto; position: fixed; margin-top: 40px; margin-left: 300px; height: 220px; top: 0; left: 0; bottom: 0; right: 0;")),
	shinyjs::inlineCSS(list(.tpanel_class2 = "overflow: auto; position: fixed; margin-top: 40px; height: 220px; top: 0; left: 0; bottom: 0; right: 0;")),
	shinyjs::inlineCSS(list(.mainpanel_class1 = "position: fixed; margin-top: 260px; margin-left: 305px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")),
	shinyjs::inlineCSS(list(.mainpanel_class2 = "position: fixed; margin-top: 260px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")),
	shinyjs::inlineCSS(list(.mainpanel_class3 = "position: fixed; margin-top: 40px; margin-left: 305px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")),
	shinyjs::inlineCSS(list(.mainpanel_class4 = "position: fixed; margin-top: 40px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")),
	
	# Popup window
	# ------------
	
	bsModal("picture_info", "Picture details", "popup1_button", size = "small",
		fluidRow(
			column(12, textInput('picture_name', 'Enter name:', 'Picture_name'))
		),
		fluidRow(
			column(6, numericInput('picture_height', 'Height:', 800, min = 100)),
			column(6, numericInput('picture_width', 'Width:', 1000, min = 300))
		),
		fluidRow(
			column(12, selectInput('picture_format', 'Format:', c('png', 'jpeg', 'svg'), 'png'))
		),
		br(),
		br(),
		fluidRow(
			column(3, actionButton('ok1_button', 'Ok', style = "position: absolute; top: -2.6em; right: -13.4em;"))
		),
		tags$head(tags$style("#picture_info .modal-footer{display:none}"))
	),
	
	bsModal("lreg_info", "Linear regression information", "popup2_button", size = "small",
		fluidRow(
			column(6, radioButtons("lreg_info_xpos", "X position:", choices = c("Left" = "left", "Right" = "right"), selected = "left", inline = F)),
			column(6, radioButtons("lreg_info_ypos", "Y position:", choices = c("Top" = "top", "Bottom" = "bottom"), selected = "top", inline = F))
		),
		fluidRow(
			column(12, selectizeInput("lreg_info_elt", "Information:", choices = " ", options = list(maxOptions = 9999, maxItems = 9999)))
		),
		br(),
		br(),
		fluidRow(
			column(3, actionButton('ok2_button', 'Ok', style = "position: absolute; top: -2.6em; right: -13.4em;")),
			column(3, disabled(actionButton('reset3_button', 'Reset', style = "position: absolute; top: -2.6em; right: -4em;")))
		),
		tags$head(tags$style("#lreg_info .modal-footer{display:none}"))
	),
	
	# Sidebars
	# --------
	
	div(
		style = "display:flex; align-items:flex-start",
		
		# Hide/show panels
		# ----------------
		
		wellPanel( 
			style = "background: #367BB4FF; overflow: hidden; position: fixed; height: 5px; top: 0; left: 0; bottom: 0; right: 0;",
			div(style = "position: absolute; top: 0.36em; left: 0.3em;", img(src = 'WIDEa_header_img.png', width = '25%')),
			div(style = "color: white; position: absolute; top: 0em; right: 8.9em;", h5("Hide/show panel:")),
			div(style = "position: absolute; top: 0.4em; right: 6.4em;", actionButton("hs_lpanel_button", "Left", style='padding: 0px; font-size: 100%')),
			div(style = "position: absolute; top: 0.4em; right: 3.95em;", actionButton("hs_tpanel_button", "Top", style='padding: 0px; font-size: 100%')),
			div(style = "position: absolute; top: 0.4em; right: 1em;", actionButton("hs_bpanel_button", "Both", style='padding: 0px; font-size: 100%'))
		),
		
		# Left panel
		# ----------
		
		tags$head(tags$style(HTML('#data_path1{background-color: white;}'))),
		tags$head(tags$style(HTML('#display_button{margin-bottom: 10px;}'))),
		tags$head(tags$style(HTML(paste0("#plot_type .radio{margin-left: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_multi_rb_top_margin"), s_OS_name]), "px !important; -webkit-margin-after: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "multi_rb_margin_after"), s_OS_name]), "px !important;} #plot_type>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML(paste0("#model .radio{margin-left: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_multi_rb_top_margin"), s_OS_name]), "px !important; -webkit-margin-after: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "multi_rb_margin_after"), s_OS_name]), "px !important;} #model>*{margin-bottom: 15px;}")))),
		
		tags$head(tags$style(HTML(".del {background-color: #F8412C; border: none; color: #FFFFFF; width: 40px; padding: 2px; font-size: 85%;}
			.del.disabled:hover, .del.disabled:focus {background-color: #F8412C;}
			.del:hover, .del:focus, .del.focus, .del:active:focus, .del.active:focus, .del:active.focus, .del.active.focus {background-color: #FF0000;}"
		))),
		
		tags$head(tags$style(HTML(".expand {border: none; width: 40px; padding: 2px; font-size: 85%;}"))),
		
		wellPanel(id = "lpanel",
			style = "overflow-y: auto; overflow-x: hidden; position: fixed; width: 300px; margin-top: 40px; margin-bottom: 0px; top: 0; left: 0; bottom: 0;",
			
			div(style = "position: absolute; top: 0em; left: 0em;", hidden(actionButton('update_button', 'Update'))),
			div(style = "position: absolute; top: 0em; left: 0em;", hidden(actionButton('reset1_button', 'Reset1'))),
			
			# Data selection
			div(style = "position: absolute; top: 0em; left: 1.25em; color: #367BB4FF;", h4("Data type selection")),
			
			div(style = "position: absolute; top: 3em; left: 1.25em;", radioButtons("data_type", NULL, choices = c("Normal" = "normal", "Temporal" = "temporal", "IR" = "ir"), selected = "normal", inline = T)),
			
			# Data loading
			div(style = "position: absolute; top: 5.7em; left: 1.25em; color: #367BB4FF;", h4("Data loading")),
			
			div(style = "position: absolute; top: 8em; left: 1.25em;", checkboxInput('flag', 'With flags', F)),
			div(style = "position: absolute; top: 11em; left: 1.25em;", textInput('data_path1', 'Select data:', '', width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"))),
			div(style = paste0("position: absolute; top: 15.74em; left: ", 6.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), shinyFilesButton("browse1_button", "Browse", "Please select a file", F)),
			div(style = paste0("position: absolute; top: 15.74em; left: ", 13.1 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('load1_button', 'Load'))),
			div(style = "position: absolute; top: 19.5em; left: 1.25em;", disabled(textInput('data_path2', 'Select code/frequency:', '', width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			div(style = paste0("position: absolute; top: 24.24em; left: ", 6.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]),"em;"), disabled(shinyFilesButton("browse2_button", "Browse", "Please select a file", F))),
			div(style = paste0("position: absolute; top: 24.24em; left: ", 13.1 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('load2_button', 'Load'))),
			
			# Normal plot selection
			div(style = "position: absolute; top: 28.5em; left: 1.25em; color: #367BB4FF;", h4("Normal plot selection")),
			
			div(style = "position: absolute; top: 31.5em; left: 1.25em;", strong("Type:")),
			div(style = paste0("position: absolute; top: ", 33.5 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_2"), s_OS_name]), "em; left: 1.25em;"), align = 'left', class = 'multicol3', disabled(radioButtons("plot_type", NULL, choices = c("Plot" = "plot", "Boxplot" = "boxplot", "Histplot" = "histplot", "Barplot" = "barplot", "Corplot" = "corplot"), selected = "plot", inline = F))),
			
			div(style = "position: absolute; top: 38.5em; left: 1.25em;", disabled(radioButtons("dim_num", "Dimension number:", choices = c("2" = "2d", "3" = "3d"), selected = "2d", inline = T))),
			
			div(style = "position: absolute; top: 43em; left: 1.25em;", strong("Model:")),
			div(style = paste0("position: absolute; top: ", 45 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_2"), s_OS_name]), "em; left: 1.25em;"), align = 'left', class = 'multicol2', disabled(radioButtons("model", NULL, choices = c(None = "none", Calibration = "calib", Validation = "valid"), selected = "none", inline = F))),
			
			# Model parameter loading
			div(style = "position: absolute; top: 50em; left: 1.25em; color: #367BB4FF;", h4("Model parameter loading")),
			
			div(style = "position: absolute; top: 53em; left: 1.25em;", strong("Select model parameter:")),
			div(style = "position: absolute; top: 55.5em; left: 1.25em;", disabled(textInput('data_path3', NULL, '', width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			div(style = paste0("position: absolute; top: 58.54em; left: ", 6.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(shinyFilesButton("browse3_button", "Browse", "Please select a file", F))),
			div(style = paste0("position: absolute; top: 58.54em; left: ", 13.1 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('load3_button', 'Load'))),
			
			# Variable selection
			div(style = "position: absolute; top: 63em; left: 1.25em; color: #367BB4FF;", h4("Variable selection")),
			
			div(style = "position: absolute; top: 66em; left: 1.25em;", disabled(radioButtons("id", "ID:", choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = "position: absolute; top: 70em; left: 1.25em;", disabled(selectizeInput("var_id", label = NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
			
			div(style = "position: absolute; top: 73.4em; left: 1.25em;", strong("Random:")),
			div(style = "position: absolute; top: 73.31em; left: 5.7em;", disabled(radioButtons("ref_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = paste0("position: absolute; top: 73.8em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del1_button', NULL, icon = icon("trash-alt"), class = "del"))),
			div(style = "position: absolute; top: 75.41em; left: 1.25em;", disabled(selectizeInput("ref", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
			
			div(style = "position: absolute; top: 78.9em; left: 1.25em;", strong("X:")),
			div(style = "position: absolute; top: 79.9em; left: 1.25em;", disabled(checkboxInput('concat1', 'Concatenation', F))),
			div(style = paste0("position: absolute; top: 80.91em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del2_button', NULL, icon = icon("trash-alt"), class = "del"))),
			div(style = "position: absolute; top: 82.5em; left: 1.25em;", disabled(selectizeInput("var_x", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 1)))),
			div(style = "position: absolute; top: 85.29em; left: 1.25em;", strong("f(x):")),
			div(style = "position: absolute; top: 85.2em; left: 3.5em;", disabled(radioButtons("f_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = paste0("position: absolute; top: 85.6em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand1_button', NULL, icon = icon("expand-alt"), class = "expand"))),
			div(style = "position: absolute; top: 87.1em; left: 1.25em;", disabled(textInput("f_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			div(style = "position: absolute; top: 91.05em; left: 1.25em;", strong("format:")),
			div(style = "position: absolute; top: 90.7em; left: 5.45em;", disabled(selectInput("date_format", label = NULL, choices = " ", width = '180px'))),
			
			div(style = "position: absolute; top: 94.3em; left: 1.25em;", strong("Y:")),
			div(style = paste0("position: absolute; top: 94.56em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del3_button', NULL, icon = icon("trash-alt"), class = "del"))),
			div(style = "position: absolute; top: 96.15em; left: 1.25em;", disabled(selectizeInput("var_y", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 1)))),
			div(style = "position: absolute; top: 98.94em; left: 1.25em;", strong("g(y):")),
			div(style = "position: absolute; top: 98.85em; left: 3.5em;", disabled(radioButtons("g_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = paste0("position: absolute; top: 99.45em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand2_button', NULL, icon = icon("expand-alt"), class = "expand"))),
			div(style = "position: absolute; top: 100.95em; left: 1.25em;", disabled(textInput("g_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			
			div(style = "position: absolute; top: 105.2em; left: 1.25em;", disabled(selectizeInput("var_z", "Z:", choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
			div(style = "position: absolute; top: 109.69em; left: 1.25em;", strong("h(z):")),
			div(style = "position: absolute; top: 109.6em; left: 3.5em;", disabled(radioButtons("h_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = paste0("position: absolute; top: 110.2em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand3_button', NULL, icon = icon("expand-alt"), class = "expand"))),
			div(style = "position: absolute; top: 111.7em; left: 1.25em;", disabled(textInput("h_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			
			div(style = "position: absolute; top: 116.04em; left: 1.25em;", strong("Weighted residuals:")),
			div(style = "position: absolute; top: 115.95em; left: 10.6em;", disabled(radioButtons("wres_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = "position: absolute; top: 117.25em; left: 1.25em;", disabled(checkboxInput('wres_cbox', 'with groups:', F))),
			div(style = paste0("position: absolute; top: 118.24em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del4_button', NULL, icon = icon("trash-alt"), class = "del"))),
			div(style = "position: absolute; top: 119.85em; left: 1.25em;", disabled(selectizeInput("wres_group", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
			div(style = "position: absolute; top: 122.41em; left: 1.25em;", strong("variance function:")),
			div(style = paste0("position: absolute; top: 122.96em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand4_button', NULL, icon = icon("expand-alt"), class = "expand"))),
			div(style = "position: absolute; top: 124.46em; left: 1.25em;", disabled(textInput("wres_vfun", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
			
			div(style = "position: absolute; top: 128.6em; left: 1.25em;", disabled(radioButtons("group", "Group:", choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
			div(style = "position: absolute; top: 131.6em; left: 1.25em;", disabled(checkboxInput('concat2', 'Concatenation', F))),
			div(style = paste0("position: absolute; top: 132.61em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del5_button', NULL, icon = icon("trash-alt"), class = "del"))),
			div(style = "position: absolute; top: 134.2em; left: 1.25em;", disabled(selectizeInput("var_group", label = NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
			
			div(style = paste0("position: absolute; top:138.35em; left: ", 11.95 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('display_button', 'Display')))
		),
		
		# Top panel
		# ---------
		
		tags$head(tags$style(HTML(paste0("#mode .radio{margin-left: 0px; margin-right: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_top_margin_1"), s_OS_name]), "px !important; -webkit-margin-after: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "multi_rb_margin_after"), s_OS_name]), "px !important;} #mode>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML(paste0("#op_radio .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_1"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #op_radio>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML(paste0("#bin_radio .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_1"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #bin_radio>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML(paste0("#y_scale .radio{margin-left: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_top_margin_2"), s_OS_name]), "px; margin-bottom: 25px;} #y_scale>*{margin-bottom: 6px;}")))),
		tags$head(tags$style(HTML(paste0("#dec_num_radio .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_1"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #dec_num_radio>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML('#fraction {height: 42px;}'))),
		tags$head(tags$style(HTML('#ymin {height: 35px}'))),
		tags$head(tags$style(HTML('#ymax {height: 35px;}'))),
		tags$head(tags$style(HTML('#apply_button {margin-right: 10px;}'))),
		
		tags$head(tags$style(HTML(paste0("#action .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_3"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #action>*{margin-bottom: 15px;}")))),
		tags$head(tags$style(HTML('#var_flag_1 .checkbox{margin-left: 0px; margin-right: 0px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #var_flag_1>*{margin-bottom: 15px;}'))),
		tags$head(tags$style(HTML('#draw .radio{margin-left: 0px; margin-right: 0px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #draw>*{margin-bottom: 15px;}'))),
		tags$head(tags$style(HTML('#qc .radio{margin-left: 0px; margin-right: 2px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #qc>*{margin-bottom: 15px;}'))),
		tags$head(tags$style(HTML('#save_button {margin-right: 10px;}'))),
		
		wellPanel(id = "tpanel",
			class = "tpanel_class1",
			tags$style(HTML(".tabbable > .nav > li > a {margin-left: -20px; margin-right: 20px; margin-top: -20px; background-color: #C5D1D8;  color: #FFF;}")),
			tags$style(HTML(".tabbable > .nav > li > a[data-value='Graphic'] {position: absolute; top: 0em; left: 0em;}")),
			tags$style(HTML(".tabbable > .nav > li > a[data-value='Flag'] {position: absolute; top: 0em; left: 5.6em;}")),
			tags$style(HTML(".tabbable > .nav > li > a[data-value='Statistics'] {position: absolute; top: 0em; left: 9.6325em;}")),
			tags$style(HTML(".tabbable > .nav > li[class=active] > a {background-color: #889AA5; color: #FFF;}")),
			
			tabsetPanel(
				# Graphical parameters
				
				tabPanel(title = "Graphic",
					div(style = "position: absolute; top: 3.5em; left: 2em;", disabled(radioButtons("webgl", "WebGL:", choices = c(Yes = "yes", No = "no"), selected = "yes", inline = T))),
					div(style = "position: absolute; top: 7.4em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("mode", "Mode:", choices = c("Line+marker" = "line_marker", "Line" = "line", "Marker" = "marker"), selected = "marker", inline = F))),
					div(style = "position: absolute; top: 13.4em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("op_radio", "Opacity:", choices = c("Auto" = "auto", "Manual:" = "manual"), selected = "auto", inline = F))),
					div(style = paste0("position: absolute; top: 13em; left: ", 12 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(numericInput("op", "", numeric(0), min = 0, max = 1, step = 0.01, width = "100px"))),
					div(style = paste0("position: absolute; top: 14.65em; left: ", 19.4 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("op_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
					div(style = "position: absolute; top: 17.9em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("bin_radio", "Bin width:", choices = c("Auto" = "auto", "Manual:" = "manual"), selected = "auto", inline = F))),
					div(style = paste0("position: absolute; top: 17.5em; left: ", 12 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput("bw", "", character(0), width = "100px"))),
					div(style = paste0("position: absolute; top: 19.15em; left: ", 19.4 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("bw_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
					
					div(style = paste0("position: absolute; top: 3.5em; left: ", 25 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), align = 'left', class = 'multicol1', disabled(radioButtons("y_scale", "Y", choices = c("None" = "none", "Auto:" = "auto", "Manual:" = "manual"), selected = "none", inline = F))),
					div(style = paste0("position: absolute; top: 3.5em; left: ", 26 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), strong("scale:")),
					div(style = paste0("position: absolute; top: 7.91em; left: ", 29.5 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("fraction")),
					div(style = paste0("position: absolute; top: 7.8em; left: ", 33.5 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(numericInput('fraction', NULL, value = numeric(0), min = 0, max = 0.1, step = 0.01, width = "100px"))),
					div(style = paste0("position: absolute; top: 11.02em; left: ", 30.7 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("min")),
					div(style = paste0("position: absolute; top: 11.12em; left: ", 32.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput('ymin', NULL, character(0), width = "110px"))),
					div(style = paste0("position: absolute; top: 13.92em; left: ", 30.7 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("max")),
					div(style = paste0("position: absolute; top: 14.02em; left: ", 32.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput('ymax', NULL, character(0), width = "110px"))),
					div(style = paste0("position: absolute; top: 12.4em; left: ", 41 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("yscale_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
					div(style = paste0("position: absolute; top: 17.9em; left: ", 25 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), align = 'left', class = 'multicol2', disabled(radioButtons("dec_num_radio", "Decimal number:", choices = c("Auto" = "auto", "Manual:" = "manual"), selected = "auto", inline = F))),
					div(style = paste0("position: absolute; top: 17.5em; left: ", 35 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(numericInput("dec_num", "", numeric(0), min = 0, step = 1, width = "100px"))),
					div(style = paste0("position: absolute; top: 19.15em; left: ", 42.4 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("dec_num_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
					
					div(style = paste0("position: absolute; top: 3.5em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), strong("Label:")),
					div(style = paste0("position: absolute; top: 4.55em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("Title:")),
					div(style = paste0("position: absolute; top: 5em; left: ", 49.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(radioButtons("edit1_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
					div(style = paste0("position: absolute; top: 7.1em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput("edit1", NULL, character(0), width = "250px"))),
					div(style = paste0("position: absolute; top: 9.85em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("X:")),
					div(style = paste0("position: absolute; top: 10.3em; left: ", 48.1 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(radioButtons("edit2_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
					div(style = paste0("position: absolute; top: 12.4em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput("edit2", NULL, character(0), width = "250px"))),
					div(style = paste0("position: absolute; top: 4.55em; left: ", 64 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("Y:")),
					div(style = paste0("position: absolute; top: 5em; left: ", 65.6 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(radioButtons("edit3_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
					div(style = paste0("position: absolute; top: 7.1em; left: ", 64 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput("edit3", NULL, character(0), width = "250px"))),
					div(style = paste0("position: absolute; top: 9.85em; left: ", 64 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), h5("Z:")),
					div(style = paste0("position: absolute; top: 10.3em; left: ", 65.6 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(radioButtons("edit4_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
					div(style = paste0("position: absolute; top: 12.4em; left: ", 64 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(textInput("edit4", NULL, character(0), width = "250px"))),
					div(style = paste0("position: absolute; top: 7.1em; left: ", 82 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton('apply_button', 'Apply')))
				),
				
				# Flag parameters
				
				tabPanel(title = "Flag",
					div(style = "position: absolute; top: 3.5em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("action", "Action:", choices = c("Add new flags" = "add_flag", "Replace qc = 1 with 2" = "replace_qc"), selected = "add_flag", inline = F))),
					div(style = "position: absolute; top: 7.5em; left: 2em;", align = 'left', class = 'multicol2', disabled(checkboxGroupInput("var_flag_1", "Variable:", choices = c("X" = "flag_x", "Y" = "flag_y"), inline = F))),
					div(style = "position: absolute; top: 8.57em; left: 7.85em;", disabled(checkboxInput("var_flag_2", "Z", F))),
					div(style = "position: absolute; top: 7.5em; left: 12em;", align = 'left', class = 'multicol2', disabled(radioButtons("draw", "Draw:", choices = c(Interval = "mpt", Point = "pt"), selected = "pt", inline = F))),
					div(style = "position: absolute; top: 7.5em; left: 23.5em;", align = 'left', class = 'multicol2', disabled(radioButtons("qc", "Quality", choices = c("1" = "1", "2" = "2"), selected = "2", inline = F))),
					div(style = "position: absolute; top: 7.5em; left: 27.2em;", strong("code:")),
					div(style = "position: absolute;top: 7.5em; left: 32.5em;", disabled(textAreaInput('comment', 'Commentary:', '', height = '60px', width = '388px'))),
					div(style = "position: absolute; top: 9.25em; left: 59.2em;", disabled(actionButton('clear1_button', 'Clear'))),
					div(style = "position: absolute; top: 9.25em; left: 64.2em;", disabled(actionButton('clear2_button', 'Clear all'))),
					div(style = "position: absolute; top: 9.25em; left: 70.7em;", disabled(actionButton('save_button', 'Save')))
				),
				
				# Statistics
				
				tabPanel(title = "Statistics",
					div(style = "position: absolute; top: 2.6em; left: 2em; color: #367BB4FF;", h4("Normal:")),
					div(style = "position: absolute; top: 5.5em; left: 2em;", strong("Plot:")),
					div(style = "position: absolute; top: 6.6em; left: 2em;", align = 'left', class = 'multicol1', disabled(checkboxInput("lreg", "Add linear regression", F))),
					div(style = "position: absolute; top: 8.3em; left: 2em;", align = 'left', class = 'multicol1', disabled(checkboxInput("conf_ellipsoid", "Add confidence ellipsoid", F))),
					div(style = "position: absolute; top: 10em; left: 2em;", align = 'left', class = 'multicol1', disabled(checkboxInput("centroid", "Add centroid", F))),
					div(style = "position: absolute; top: 12.9em; left: 2em;", strong("Boxplot:")),
					div(style = "position: absolute; top: 14em; left: 2em;", align = 'left', class = 'multicol1', disabled(checkboxInput("box_mean_sd", "Add mean/sd", F))),
					div(style = "position: absolute; top: 5.5em; left: 15.3em;", strong("Histplot:")),
					div(style = "position: absolute; top: 6.6em; left: 15.3em;", disabled(checkboxInput("dens_curve", "Add density curve", F))),
					div(style = "position: absolute; top: 8.3em; left: 15.3em;", disabled(checkboxInput("norm_dens_curve", "Add normal density curve", F))),
					
					div(style = "position: absolute; top: 2.6em; left: 33em; color: #367BB4FF;", h4("IR:")),
					div(style = "position: absolute; top: 5.5em; left: 33em;", disabled(checkboxInput("mean_spect", "Add mean spectrum", F)))
				)
			)
		),
		
		# Main panel
		# ----------
		
		tags$head(tags$style(HTML('#select_graph+ div>.selectize-input{margin-left: 30px; margin-top: 5px; margin-bottom: 0px;}'))),
		
		div(id = "mainpanel",
			class = "mainpanel_class1",
			fluidRow(
				column(12, offset = 1, hidden(selectInput("select_graph", label = NULL, choices = " ", width = "300px"))),
				column(12, plotlyOutput("graphic"))
			),
			hidden(div(id = "sh_popup1", style = "position: absolute; top: 0em; left: 0.5em;", actionButton('popup1_button', NULL, icon = icon("plus"), style='padding:0px; font-size:60%'))),
			bsTooltip("popup1_button", "Change picture details", placement = "right"),
			hidden(div(id = "sh_reset", style = "position: absolute; top: 0em; left: 1.5em;", actionButton('reset2_button', NULL, icon = icon("home"), style='padding:0px; font-size:60%'))),
			bsTooltip("reset2_button", "Reset axes", placement = "right"),
			hidden(div(id = "sh_popup2", style = "position: absolute; top: 0em; left: 2.7em;", actionButton('popup2_button', NULL, icon = icon("edit"), style='padding:0px; font-size:60%'))),
			bsTooltip("popup2_button", "Add linear regression information on graph", placement = "right")
		)
	)
)


# II) Creating the server
# =======================

f_server <- function(input, output, session) {
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
	
	o_reset <- reactiveValues(code = 0, model1 = 0, model2 = 0) # parameters used to reset initial parameters 
	
	# parameters associated to load data fields
	o_path <- reactiveValues(name1_prev = NA, name2_prev = NA, name3_prev = NA, name1 = NA, name2 = NA, name3 = NA, color1 = "white", color2 = "transparent", color3 = "transparent") # path, color assigned to fields
	o_load_error <- reactiveValues(code = rep(0, 3)) # error message returned (1) or not (0)
	o_data_info <- reactiveValues(mtime = rep(NA, 3), code = NA) # data modification time and code range (ir data type)
	
	o_flag <- reactiveValues(name = NA) # flag data name
	o_click_button <- reactiveValues(left_panel = 0, top_panel = 0, both_panel = 0, both_panel_prev = 0, browse1 = 0, browse2 = 0, browse3 = 0, load1 = 0, load2 = 0, load3 = 0, display = 0) # parameters associated to several buttons: clicked (1) or not (0)
	o_click_graph <- reactiveValues(prev_date = NULL, prev_var = NULL) # parameters associated to current flag data for the temporal data type (used to draw an interval) 
	o_click_legend <- reactiveValues(item = NULL) # parameter corresponding to graph legend items (including name, statut)
	o_zoom <- reactiveValues(coord = NULL) # zoom coordinates
	
	o_cond <- reactiveValues(
		top_panel = 0, display = 0, update = 0, del = rep(0, 5), concat1 = 0, concat2 = 0, webgl = 0, mode = 0, select_graph1 = 0, select_graph2 = 0, label = rep(0, 4), 
		flag = 0, qc1 = 0, qc2 = 0, save1 = 0, save2 = 0, stat = 0, reset2 = 0, legend = 0, flag_msg = 0
	)
	
	o_norm_select <- reactiveValues(type = NA) # plot type (only normal data type)
	
	o_plot <- reactiveValues(
		data = data.frame(), model = NA, code_freq = NA, id_group = NA, y_coord = NULL, color = NA, 
		add_pt = T, pt_pos = NA, var_pt = NA, var_pt_color = NA, 
		data_qc1 = NA, data_qc2 = NA, var_qc1 = NA, var_qc2 = NA, leg_name_qc = NA, 
		elt = NA, elt_pt_pos = NA
	) # parameters associated to the display button
	
	o_picture_info <- reactiveValues(filename = "Picture_name", format = "png", height = 800, width = 1000) # parameters associated to the graph picture
	o_lreg_info <- reactiveValues(xpos = 0, ypos = 1, elt = NA) # parameters associated to the linear regression legend
	
	# parameters associated to a modal window and used to represent a supplementary graph by clicking on a corplot cell (normal data type)
	o_modal_plot <- reactiveValues(num = 0, x = NA, y = NA) 
	
	o_parameter <- reactiveValues( 
		plot_type = NA, dim_num = NA, model = NA, id = NA, ref = NA, concat1 = F, concat1_group = NA, x = NA, f = NA, date_format = NA, y = NA, g = NA, z = NA, h = NA, wres_group = NA, wres_vfun = NA, concat2 = F, concat2_group = NA, group = NA,
		webgl = "yes", mode = "marker", autoop = T, op = NA, autobin = T, bw = NA, y_scale = "none", autodec_num = T, dec_num = NA, autotitle = T, title = NA, autoxlab = T, xlab = "x", autoylab = T, ylab = "y", autozlab = T, zlab = "z",
		lreg = F, conf_ellipsoid = F, centroid = F, boxmean = "NULL", dens_curve = F, norm_dens_curve = F, mean_spect = F,
		select_graph = NA, corplot_group = NA
	) # parameters associated to the "variable selection" section (left panel) and top panel tabs
	
	o_legend_group <- reactiveValues(lreg = c(), conf_ellipsoid = c(), dens_curve = c(), norm_dens_curve = c()) # graph legend items associated to several statistical methods 
	o_w_message <- reactiveValues(lreg = 0, conf_ellipsoid = 0, dens_curve = 0, norm_dens_curve = 0) # parameters used to display (0) or not (1) a warning message returned by several statistical methods
	
	# 1.2. Add events of hide/show panel buttons
	# ==========================================
	
	# left panel
	
	observeEvent(input$hs_lpanel_button, {
		if (isolate(o_click_button$left_panel) == 1) {
			shinyjs::show(id = "lpanel")
			shinyjs::removeClass("tpanel", class = "tpanel_class2")
			shinyjs::addClass("tpanel", class = "tpanel_class1")
			o_cond$top_panel <- 0
			
			if (isolate(o_click_button$top_panel) == 1) {
				if (isolate(o_click_button$both_panel_prev) == 1) {
					o_click_button$both_panel_prev <- 0
				}
				
				shinyjs::disable("hs_bpanel_button")
				shinyjs::removeClass("mainpanel", class = "mainpanel_class4")
				shinyjs::addClass("mainpanel", class = "mainpanel_class3")
			}
			else {
				shinyjs::enable("hs_bpanel_button")
				o_click_button$both_panel <- 0
				shinyjs::removeClass("mainpanel", class = "mainpanel_class2")
				shinyjs::addClass("mainpanel", class = "mainpanel_class1")
			}
			
			o_click_button$left_panel <- 0
		}
		else {
			shinyjs::hide(id = "lpanel")
			shinyjs::removeClass("tpanel", class = "tpanel_class1")
			shinyjs::addClass("tpanel", class = "tpanel_class2")
			o_cond$top_panel <- 1
			
			if (isolate(o_click_button$top_panel) == 1) {
				shinyjs::enable("hs_bpanel_button")
				o_click_button$both_panel <- 1
				shinyjs::removeClass("mainpanel", class = "mainpanel_class3")
				shinyjs::addClass("mainpanel", class = "mainpanel_class4")
			}
			else {
				shinyjs::disable("hs_bpanel_button")
				shinyjs::removeClass("mainpanel", class = "mainpanel_class1")
				shinyjs::addClass("mainpanel", class = "mainpanel_class2")
			}
			
			o_click_button$left_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {
			click("update_button")
			o_cond$display <- 1
			click("display_button")
		}
	})
	
	# top panel
	
	observeEvent(input$hs_tpanel_button, {
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
				
				shinyjs::disable("hs_bpanel_button")
				shinyjs::removeClass("mainpanel", class = "mainpanel_class4")
				shinyjs::addClass("mainpanel", class = "mainpanel_class2")
			}
			else {
				shinyjs::show(id = "tpanel")
				shinyjs::enable("hs_bpanel_button")
				o_click_button$both_panel <- 0
				shinyjs::removeClass("mainpanel", class = "mainpanel_class3")
				shinyjs::addClass("mainpanel", class = "mainpanel_class1")
			}
			
			o_click_button$top_panel <- 0
		}
		else {
			shinyjs::hide(id = "tpanel")
			
			if (isolate(o_click_button$left_panel) == 1) {
				shinyjs::enable("hs_bpanel_button")
				o_click_button$both_panel <- 1
				shinyjs::removeClass("mainpanel", class = "mainpanel_class2")
				shinyjs::addClass("mainpanel", class = "mainpanel_class4")
			}
			else {
				shinyjs::disable("hs_bpanel_button")
				shinyjs::removeClass("mainpanel", class = "mainpanel_class1")
				shinyjs::addClass("mainpanel", class = "mainpanel_class3")
			}
			
			o_click_button$top_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {
			click("update_button")
			o_cond$display <- 1
			click("display_button")
		}
	})
	
	# both panels
	
	observeEvent(input$hs_bpanel_button, {
		if (isolate(o_click_button$both_panel) == 1) {
			if (isolate(o_cond$top_panel) == 1) {
				shinyjs::removeClass("tpanel", class = "tpanel_class2")
				shinyjs::addClass("tpanel", class = "tpanel_class1")
				o_cond$top_panel <- 0
			}
			
			shinyjs::show(id = "lpanel")
			shinyjs::show(id = "tpanel")
			shinyjs::removeClass("mainpanel", class = "mainpanel_class4")
			shinyjs::addClass("mainpanel", class = "mainpanel_class1")
			o_click_button$both_panel_prev <- 0
			o_click_button$both_panel <- 0
			o_click_button$left_panel <- 0
			o_click_button$top_panel <- 0
		}
		else {
			shinyjs::hide(id = "lpanel")
			shinyjs::hide(id = "tpanel")
			shinyjs::removeClass("mainpanel", class = "mainpanel_class1")
			shinyjs::addClass("mainpanel", class = "mainpanel_class4")
			o_click_button$both_panel_prev <- 1
			o_click_button$both_panel <- 1
			o_click_button$left_panel <- 1
			o_click_button$top_panel <- 1
		}
		
		if (isolate(o_click_button$display) == 1) {
			click("update_button")
			o_cond$display <- 1
			click("display_button")
		}
	})
	
	# 1.3. Add events of reset1 button  
	# ================================
	
	observeEvent(input$reset1_button, {
		if (isolate(o_reset$model1) == 1) {
			o_reset$model1 <- 0
			
			if (input$f_radio == "yes") {
				updateRadioButtons(session, "f_radio", selected = "no")
				shinyjs::delay(100, disable("f_radio"))
			}
			else {
				shinyjs::disable("f_radio")
			}
			
			updateSelectizeInput(session, "var_x", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
			shinyjs::delay(100, disable("var_x"))
			
			updateSelectizeInput(session, "var_y", choices = " ")
			shinyjs::delay(100, disable("var_y"))
			
			if (input$g_radio == "yes") {
				updateRadioButtons(session, "g_radio", selected = "no")
				shinyjs::delay(100, disable("g_radio"))
			}
			else {
				shinyjs::disable("g_radio")
			}
			
			if (input$group == "yes") {
				updateRadioButtons(session, "group", selected = "no")
				shinyjs::delay(100, disable("group"))
			}
			else {
				shinyjs::disable("group")
			}
			
			if (input$model == "calib") {
				if (input$lreg == T) {
					updateCheckboxInput(session, "lreg", value = F)
					shinyjs::delay(100, disable("lreg"))
				}
				else {
					shinyjs::disable("lreg")
				}
				
				if (input$conf_ellipsoid == T) {
					updateCheckboxInput(session, "conf_ellipsoid", value = F)
					shinyjs::delay(100, disable("conf_ellipsoid"))
				}
				else {
					shinyjs::disable("conf_ellipsoid")
				}
				
				if (input$centroid == T) {
					updateCheckboxInput(session, "centroid", value = F)
					shinyjs::delay(100, disable("centroid"))
				}
				else {
					shinyjs::disable("centroid")
				}
			}
			
			shinyjs::disable("display_button")
		}
		
		if (isolate(o_reset$model2) == 1) {
			o_reset$model2 <- 0
			o_cond$del[2] <- 0
			
			if (input$ref_radio == "yes") {
				updateRadioButtons(session, "ref_radio", selected = "no")
				shinyjs::delay(100, disable("ref_radio"))
			}
			else {
				shinyjs::disable("ref_radio")
			}
			
			if (input$wres_radio == "yes") {
				updateRadioButtons(session, "wres_radio", selected = "no")
				shinyjs::delay(100, disable("wres_radio"))
			}
			else {
				shinyjs::disable("wres_radio")
			}
		}
		
		if (isolate(o_click_button$display) == 1) {
			o_parameter$plot_type <- NA
			o_parameter$dim_num <- NA
			o_parameter$model <- NA
			o_parameter$ref <- NA
			o_parameter$wres_group <- NA
			o_parameter$wres_vfun <- NA
			o_parameter$id <- NA
			o_parameter$concat1 <- F
			o_parameter$concat1_group <- NA
			o_parameter$x <- NA
			o_parameter$f <- NA
			o_parameter$date_format <- NA
			o_parameter$y <- NA
			o_parameter$g <- NA
			o_parameter$z <- NA
			o_parameter$h <- NA
			o_parameter$concat2 <- F
			o_parameter$concat2_group <- NA
			o_parameter$group <- NA
			o_parameter$corplot_group <- NA
			
			o_cond$display <- 0
			o_cond$flag <- 0
			o_cond$qc1 <- 0
			o_cond$qc2 <- 0
			o_cond$save1 <- 0
			o_cond$save2 <- 0	
			o_cond$legend <- 0
			
			o_w_message$lreg <- 0
			o_w_message$conf_ellipsoid <- 0
			o_w_message$dens_curve <- 0
			o_w_message$norm_dens_curve <- 0
			
			o_plot$data <- data.frame()
			o_plot$model <- NA
			o_plot$code_freq <- NA
			o_plot$id_group <- NA
			o_plot$y_coord <- NULL
			o_plot$color <- NA
			o_plot$add_pt <- T
			o_plot$pt_pos <- NA
			o_plot$var_pt <- NA
			o_plot$var_pt_color <- NA
			o_plot$data_qc1 <- NA
			o_plot$data_qc2 <- NA
			o_plot$var_qc1 <- NA
			o_plot$var_qc2 <- NA
			o_plot$leg_name_qc <- NA
			o_plot$elt <- NA
			o_plot$elt_pt_pos <- NA
			o_click_button$display <- 0
			
			o_click_legend$item <- NULL
			o_zoom$coord <- NULL
			js$resetClick_leg()
			
			o_legend_group$lreg <- c()
			o_legend_group$conf_ellipsoid <- c()
			o_legend_group$dens_curve <- c()
			o_legend_group$norm_dens_curve <- c()
			
			o_picture_info$filename <- "Picture_name"
			o_picture_info$format <- "png"
			o_picture_info$height <- 800
			o_picture_info$width <- 1000
			
			if (input$picture_name != "Picture_name") {
				updateTextInput(session, "picture_name", value = "Picture_name")
			}
			
			if (input$picture_height != 800) {
				updateNumericInput(session, "picture_height", value = 800)
			}
			
			if (input$picture_width != 1000) {
				updateNumericInput(session, "picture_width", value = 1000)
			}
			
			if (input$picture_format != "png") {
				updateSelectInput(session, "picture_format", selected = "png")
			}
			
			o_lreg_info$xpos <- 0
			o_lreg_info$ypos <- 1
			o_lreg_info$elt <- NA
			
			if (input$lreg_info_xpos != 0) {
				updateRadioButtons(session, "lreg_info_xpos", selected = "left")
			}
			
			if (input$lreg_info_ypos != 1) {
				updateRadioButtons(session, "lreg_info_ypos", selected = "top")
			}
			
			updateSelectizeInput(session, "lreg_info_elt", choices = " ")
			shinyjs::disable("reset3_button")
			
			if (input$select_graph != " ") {
				o_parameter$select_graph <- NA
				o_cond$select_graph1 <- 0
				updateSelectInput(session, "select_graph", choices = " ")
				shinyjs::hide(id = "select_graph")
			}
			
			o_modal_plot$num <- 0
			o_modal_plot$x <- NA
			o_modal_plot$y <- NA
			
			if (length(ls(e_previous_flag)) > 0) {
				rm(list = ls(e_previous_flag), envir = e_previous_flag)
			}		
			
			if (length(ls(e_current_flag)) > 0) {
				rm(list = ls(e_current_flag), envir = e_current_flag)
				o_click_graph$prev_date <- NULL
				o_click_graph$prev_var <- NULL
				shinyjs::disable("clear1_button")
				shinyjs::disable("clear2_button")
				shinyjs::disable("save_button")
				js$resetClick()
			}
			
			output$graphic <<- NULL
			shinyjs::delay(100, hide(id = "graphic"))
			shinyjs::delay(100, hide(id = "sh_popup1"))
			shinyjs::delay(100, hide(id = "sh_popup2"))
			shinyjs::delay(100, hide(id = "sh_reset"))
		}
		
		if (isolate(o_reset$code) == 1) {
			o_reset$code <- 0
			
			o_cond$del <- rep(0, 5)
			o_cond$concat1 <- 0
			o_cond$concat2 <- 0
			o_cond$flag_msg <- 0
			
			o_parameter$webgl <- "yes"
			o_parameter$mode <- "marker"
			o_parameter$autoop <- T
			o_parameter$op <- NA
			o_parameter$autobin <- T
			o_parameter$bw <- NA
			o_parameter$y_scale <- "none"
			o_parameter$autodec_num <- T
			o_parameter$dec_num <- NA
			o_parameter$autotitle <- T
			o_parameter$title <- NA
			o_parameter$autoxlab <- T
			o_parameter$xlab <- "x"
			o_parameter$autoylab <- T
			o_parameter$ylab <- "y"
			o_parameter$autozlab <- T
			o_parameter$zlab <- "z"
			o_parameter$lreg <- F
			o_parameter$conf_ellipsoid <- F
			o_parameter$centroid <- F
			o_parameter$boxmean <- "NULL"
			o_parameter$dens_curve <- F
			o_parameter$norm_dens_curve <- F
			o_parameter$mean_spect <- F
			
			# = Disable left panel inputs =
			
			if (input$plot_type != "plot") {
				updateRadioButtons(session, "plot_type", selected = "plot")
				shinyjs::delay(100, disable("plot_type"))
			}
			else {
				shinyjs::disable("plot_type")
			}
			
			if (input$dim_num == "3d") {
				updateRadioButtons(session, "dim_num", selected = "2d")
				shinyjs::delay(100, disable("dim_num"))
				updateSelectizeInput(session, "var_z", choices = " ")
				shinyjs::delay(100, disable("var_z"))
			}
			else {
				shinyjs::disable("dim_num")
			}
			
			if (input$model != "none") {
				updateRadioButtons(session, "model", selected = "none")
				shinyjs::delay(100, disable("model"))
				
				if (input$data_path3 != "") {
					updateTextInput(session, "data_path3", value = "")
					shinyjs::delay(100, disable("data_path3"))
				}
				else {
					shinyjs::disable("data_path3")
				}
				
				js$backgroundCol("data_path3", "transparent")
				shinyjs::disable("browse3_button")
				shinyjs::disable("load3_button")
			}
			else {
				shinyjs::disable("model")
			}
			
			if (input$id == "yes") {
				updateRadioButtons(session, "id", selected = "no")
				
				if (input$var_id != " ") {
					shinyjs::enable("var_id")
					updateSelectizeInput(session, "var_id", choices = " ")
					shinyjs::delay(100, disable("var_id"))
				}
				
				shinyjs::delay(100, disable("id"))
			}
			else {
				shinyjs::disable("id")
			}
			
			if (input$concat1 == T) {
				updateCheckboxInput(session, "concat1", value = F)
				shinyjs::delay(100, disable("concat1"))
			}
			else {
				shinyjs::disable("concat1")
			}
			
			if (is.null(input$var_x)) {
				updateSelectizeInput(session, "var_x", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
				shinyjs::delay(100, disable("var_x"))
			}
			else {
				if (input$var_x[1] != " ") {
					updateSelectizeInput(session, "var_x", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
					shinyjs::delay(100, disable("var_x"))
				}
			}
			
			if (input$f_radio == "yes") {
				updateRadioButtons(session, "f_radio", selected = "no")
				shinyjs::delay(100, disable("f_radio"))
				updateTextInput(session, "f_text", value = character(0))
				shinyjs::delay(100, disable("f_text"))
			}
			else {
				shinyjs::disable("f_radio")
			}
			
			if (input$date_format != " ") {
				updateSelectInput(session, "date_format", choices = " ")
				shinyjs::delay(100, disable("date_format"))
			}
			
			if (is.null(input$var_y)) {
				updateSelectizeInput(session, "var_y", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
				shinyjs::delay(100, disable("var_y"))
			}
			else {
				if (input$var_y[1] != " ") {
					updateSelectizeInput(session, "var_y", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
					shinyjs::delay(100, disable("var_y"))
				}
			}
			
			if (input$g_radio == "yes") {
				updateRadioButtons(session, "g_radio", selected = "no")
				shinyjs::delay(100, disable("g_radio"))
				updateTextInput(session, "g_text", value = character(0))
				shinyjs::delay(100, disable("g_text"))
			}
			else {
				shinyjs::disable("g_radio")
			}
			
			if (input$h_radio == "yes") {
				updateRadioButtons(session, "h_radio", selected = "no")
				shinyjs::delay(100, disable("h_radio"))
				updateTextInput(session, "h_text", value = character(0))
				shinyjs::delay(100, disable("h_text"))
			}
			else {
				shinyjs::disable("h_radio")
			}
			
			if (input$ref_radio == "yes") {
				updateRadioButtons(session, "ref_radio", selected = "no")
				updateSelectizeInput(session, "ref", choices = " ", options = list(maxOptions = 9999, maxItems = 9999))
				shinyjs::delay(100, disable("ref_radio"))
				shinyjs::delay(100, disable("ref"))
			}
			else {
				shinyjs::disable("ref_radio")
			}
			
			if (input$wres_radio == "yes") {
				updateRadioButtons(session, "wres_radio", selected = "no")
				shinyjs::delay(100, disable("wres_radio"))
				
				if (input$wres_cbox == T) {
					updateCheckboxInput(session, "wres_cbox", value = F)
					updateSelectizeInput(session, "wres_group", choices = " ", options = list(maxOptions = 9999, maxItems = 9999))
					shinyjs::delay(100, disable("wres_cbox"))
					shinyjs::delay(100, disable("wres_group"))
				}
				else {
					shinyjs::disable("wres_cbox")
				}
				
				updateTextInput(session, "wres_vfun", value = character(0))
				shinyjs::delay(100, disable("wres_vfun"))
			}
			else {
				shinyjs::disable("wres_radio")
			}
			
			if (input$group == "yes") {
				updateRadioButtons(session, "group", selected = "no")
				shinyjs::delay(100, disable("group"))
			}
			else {
				shinyjs::disable("group")
			}
			
			if (input$concat2 == T) {
				updateCheckboxInput(session, "concat2", value = F)
				shinyjs::delay(100, disable("concat2"))
			}
			else {
				shinyjs::disable("concat2")
			}
			
			if (is.null(input$var_group)) {
				updateSelectizeInput(session, "var_group", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
				shinyjs::delay(100, disable("var_group"))
			}
			else {
				if (input$var_group[1] != " ") {
					updateSelectizeInput(session, "var_group", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
					shinyjs::delay(100, disable("var_group"))
				}
			}
			
			shinyjs::disable("display_button")
			
			# = Disable top panel inputs =
			# - Graphic tab -
			
			if (input$webgl != "yes") {
				shinyjs::enable("webgl")
				updateRadioButtons(session, "webgl", selected = "yes")
				shinyjs::delay(100, disable("webgl"))
			}
			else {
				shinyjs::disable("webgl")
			}
			
			if (input$mode != "marker") {
				updateRadioButtons(session, "mode", selected = "marker", inline = F)
				shinyjs::delay(100, disable("mode"))
			}
			else {
				shinyjs::disable("mode")
			}
			
			if (input$op_radio == "manual") {
				updateRadioButtons(session, "op_radio", selected = "auto")
				shinyjs::delay(100, disable("op_radio"))
			}
			else {
				shinyjs::disable("op_radio")
			}
			
			if (length(input$op) > 0) {
				updateNumericInput(session, "op", value = numeric(0), min = 0, max = 1)
				shinyjs::delay(100, disable("op"))
			}
			else {
				shinyjs::disable("op")
			}
			
			shinyjs::disable("op_button")
			
			if (input$bin_radio == "manual") {
				updateRadioButtons(session, "bin_radio", selected = "auto")
				shinyjs::delay(100, disable("bin_radio"))
			}
			else {
				shinyjs::disable("bin_radio")
			}
			
			if (length(input$bw) > 0) {
				updateTextInput(session, "bw", value = character(0))
				shinyjs::delay(100, disable("bw"))
			}
			else {
				shinyjs::disable("bw")
			}
			
			shinyjs::disable("bw_button")
			
			if (input$y_scale != "none") {
				updateRadioButtons(session, "y_scale", selected = "none", inline = F)
				shinyjs::delay(100, disable("y_scale"))
			}
			else {
				shinyjs::disable("y_scale")
			}
			
			if (length(input$fraction) > 0) {
				updateNumericInput(session, "fraction", value = numeric(0), min = 0, max = 0.1, step = 0.01)
				shinyjs::delay(100, disable("fraction"))
			}
			else {
				shinyjs::disable("fraction")
			}
			
			if (length(input$ymin) > 0) {
				updateTextInput(session, "ymin", value = character(0))
				shinyjs::delay(100, disable("ymin"))
			}
			else {
				shinyjs::disable("ymin")
			}
			
			if (length(input$ymax) > 0) {
				updateTextInput(session, "ymax", value = character(0))
				shinyjs::delay(100, disable("ymax"))
			}
			else {
				shinyjs::disable("ymax")
			}
			
			shinyjs::disable("yscale_button")
			
			if (input$dec_num_radio == "manual") {
				updateRadioButtons(session, "dec_num_radio", selected = "auto")
				shinyjs::delay(100, disable("dec_num_radio"))
			}
			else {
				shinyjs::disable("dec_num_radio")
			}
			
			if (length(input$dec_num) > 0) {
				updateNumericInput(session, "dec_num", value = numeric(0))
				shinyjs::delay(100, disable("dec_num"))
			}
			else {
				shinyjs::disable("dec_num")
			}
			
			shinyjs::disable("dec_num_button")
			
			if (input$edit1_radio == "yes") {
				updateRadioButtons(session, "edit1_radio", selected = "no")
				shinyjs::delay(100, disable("edit1_radio"))
			}
			else {
				shinyjs::disable("edit1_radio")
			}
			
			if (length(input$edit1) > 0) {
				updateTextInput(session, "edit1", value = character(0))
				shinyjs::delay(100, disable("edit1"))
			}
			else {
				shinyjs::disable("edit1")
			}
			
			if (input$edit2_radio == "yes") {
				updateRadioButtons(session, "edit2_radio", selected = "no")
				shinyjs::delay(100, disable("edit2_radio"))
			}
			else {
				shinyjs::disable("edit2_radio")
			}
			
			if (length(input$edit2) > 0) {
				updateTextInput(session, "edit2", value = character(0))
				shinyjs::delay(100, disable("edit2"))
			}
			else {
				shinyjs::disable("edit2")
			}
			
			if (input$edit3_radio == "yes") {
				updateRadioButtons(session, "edit3_radio", selected = "no")
				shinyjs::delay(100, disable("edit3_radio"))
			}
			else {
				shinyjs::disable("edit3_radio")
			}
			
			if (length(input$edit3) > 0) {
				updateTextInput(session, "edit3", value = character(0))
				shinyjs::delay(100, disable("edit3"))
			}
			else {
				shinyjs::disable("edit3")
			}
			
			if (input$edit4_radio == "yes") {
				updateRadioButtons(session, "edit4_radio", selected = "no")
				shinyjs::delay(100, disable("edit4_radio"))
			}
			else {
				shinyjs::disable("edit4_radio")
			}
			
			if (length(input$edit4) > 0) {
				updateTextInput(session, "edit4", value = character(0))
				shinyjs::delay(100, disable("edit4"))
			}
			else {
				shinyjs::disable("edit4")
			}
			
			shinyjs::disable("apply_button")
			
			# - Flag tab -
			
			if (input$action == "replace_qc") {
				updateRadioButtons(session, "action", selected = "add_flag", inline = F)
				shinyjs::delay(100, disable("action"))
			}
			else {
				shinyjs::disable("action")
			}
			
			if (!is.null(input$var_flag_1)) {
				updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
				shinyjs::delay(100, disable("var_flag_1"))
			}
			else {
				shinyjs::disable("var_flag_1")
			}
			
			if (input$var_flag_2 == T) {
				updateCheckboxInput(session, "var_flag_2", value = F)
				shinyjs::delay(100, disable("var_flag_2"))
			}
			else {
				shinyjs::disable("var_flag_2")
			}
			
			if (input$draw == "mpt") {
				updateRadioButtons(session, "draw", selected = "pt", inline = F)
				shinyjs::delay(100, disable("draw"))
			}
			else {
				shinyjs::disable("draw")
			}
			
			if (input$qc == "1") {
				updateRadioButtons(session, "qc", selected = "2", inline = F)
				shinyjs::delay(100, disable("qc"))
			}
			else {
				shinyjs::disable("qc")
			}
			
			if (input$comment != "") {
				updateTextInput(session, "comment", value = "")
				shinyjs::delay(100, disable("comment"))	
			}
			else {
				shinyjs::disable("comment")
			}
			
			# - Statistics tab -
			
			if (input$lreg == T) {
				updateCheckboxInput(session, "lreg", value = F)
				shinyjs::delay(100, disable("lreg"))
			}
			else {
				shinyjs::disable("lreg")
			}
			
			if (input$conf_ellipsoid == T) {
				updateCheckboxInput(session, "conf_ellipsoid", value = F)
				shinyjs::delay(100, disable("conf_ellipsoid"))
			}
			else {
				shinyjs::disable("conf_ellipsoid")
			}
			
			if (input$centroid == T) {
				updateCheckboxInput(session, "centroid", value = F)
				shinyjs::delay(100, disable("centroid"))
			}
			else {
				shinyjs::disable("centroid")
			}
			
			if (input$box_mean_sd == T) {
				updateCheckboxInput(session, "box_mean_sd", value = F)
				shinyjs::delay(100, disable("box_mean_sd"))
			}
			else {
				shinyjs::disable("box_mean_sd")
			}
			
			if (input$dens_curve == T) {
				updateCheckboxInput(session, "dens_curve", value = F)
				shinyjs::delay(100, disable("dens_curve"))
			}
			else {
				shinyjs::disable("dens_curve")
			}
			
			if (input$norm_dens_curve == T) {
				updateCheckboxInput(session, "norm_dens_curve", value = F)
				shinyjs::delay(100, disable("norm_dens_curve"))
			}
			else {
				shinyjs::disable("norm_dens_curve")
			}
			
			if (input$mean_spect == T) {
				updateCheckboxInput(session, "mean_spect", value = F)
				shinyjs::delay(100, disable("mean_spect"))
			}
			else {
				shinyjs::disable("mean_spect")
			}
		}
	})
	
	# 1.4. Add events of update button
	# ================================
	
	observeEvent(input$update_button, {
		if ("all" %in% ls(e_data)) {
			if (input$data_type == "normal") {
				if (!isolate(o_parameter$model) %in% c("calib", "valid")) {
					if (isolate(o_parameter$plot_type) %in% c("plot", "boxplot", "histplot", "barplot")) {
						if (!isolate(o_parameter$plot_type) %in% c("histplot", "barplot")) {
							if (isolate(o_parameter$plot_type) == "plot") {
								if (!is.na(isolate(o_parameter$id))) {
									if (input$id == "no") {
										updateRadioButtons(session, "id", selected = "yes")
									}
									
									if (isolate(o_parameter$id) != input$var_id) {
										updateSelectizeInput(session, "var_id", selected = isolate(o_parameter$id))
									}
								}
								else {
									if (input$id == "yes") {
										updateRadioButtons(session, "id", selected = "no")
									}
								}
								
								if (isolate(o_parameter$dim_num) == "3d") {
									if (isolate(o_parameter$z) != input$var_z) {
										updateSelectizeInput(session, "var_z", selected = isolate(o_parameter$z))
									}
									
									if (!is.na(isolate(o_parameter$h))) {
										if (input$h_radio == "no") {
											updateRadioButtons(session, "h_radio", selected = "yes")
										}
										
										if (isolate(o_parameter$h) != input$h_text) {
											updateTextInput(session, "h_text", value = isolate(o_parameter$h))
										}
									}
									else {
										if (input$h_radio == "yes") {
											updateRadioButtons(session, "h_radio", selected = "no")
										}
									}
								}
							}
							
							if (isolate(o_parameter$y) != input$var_y) {
								updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
							}
							
							if (!is.na(isolate(o_parameter$g))) {
								if (input$g_radio == "no") {
									updateRadioButtons(session, "g_radio", selected = "yes")
								}
								
								if (isolate(o_parameter$g) != input$g_text) {
									updateTextInput(session, "g_text", value = isolate(o_parameter$g))
								}
							}
							else {
								if (input$g_radio == "yes") {
									updateRadioButtons(session, "g_radio", selected = "no")
								}
							}
						}
						
						if (isolate(o_parameter$plot_type) %in% c("boxplot", "barplot")) {
							if (input$concat1 != isolate(o_parameter$concat1)) {
								o_cond$update <- 1
								updateCheckboxInput(session, "concat1", value = isolate(o_parameter$concat1))
							}
							else {
								if (isolate(o_parameter$concat1) == T) {
									if (length(input$var_x) != length(isolate(o_parameter$concat1_group))) {
										updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$concat1_group))
									}
									else {
										if (length(which(!input$var_x %in% isolate(o_parameter$concat1_group))) > 0) {
											updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$concat1_group))
										}
									}
								}
								else {
									if (length(input$var_x) != length(isolate(o_parameter$x))) {
										updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
									}
									else {
										if (length(which(!input$var_x %in% isolate(o_parameter$x))) > 0) {
											updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
										}
									}
								}
							}
						}
						else {
							if (isolate(o_parameter$x) != input$var_x) {
								updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
							}
						}
						
						if (isolate(o_parameter$plot_type) != "barplot") {
							if (!is.na(isolate(o_parameter$f))) {
								if (input$f_radio == "no") {
									updateRadioButtons(session, "f_radio", selected = "yes")
								}
								
								if (isolate(o_parameter$f) != input$f_text) {
									updateTextInput(session, "f_text", value = isolate(o_parameter$f))
								}
							}
							else {
								if (input$f_radio == "yes") {
									updateRadioButtons(session, "f_radio", selected = "no")
								}
							}
						}
					}
					else {
						if (length(input$var_y) != length(isolate(o_parameter$y))) {
							updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
						}
						else {
							if (length(which(!input$var_y %in% isolate(o_parameter$y))) > 0) {
								updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
							}
						}
					}
				}
				else {
					if (length(which(!is.na(isolate(o_parameter$ref)))) > 0) {
						if (input$ref_radio == "no") {
							o_cond$update <- 1
							updateRadioButtons(session, "ref_radio", selected = "yes")
						}
						else {
							if (length(input$ref) != length(isolate(o_parameter$ref))) {
								updateSelectizeInput(session, "ref", selected = isolate(o_parameter$ref))
							}
							else {
								if (length(which(!input$ref %in% isolate(o_parameter$ref))) > 0) {
									updateSelectizeInput(session, "ref", selected = isolate(o_parameter$ref))
								}
							}
						}
					}
					else {
						if (input$ref_radio == "yes") {
							updateRadioButtons(session, "ref_radio", selected = "no")
						}
					}
					
					if (isolate(o_parameter$model) == "calib") {
						if (!is.na(isolate(o_parameter$wres_vfun))) {
							if (length(which(!is.na(isolate(o_parameter$wres_group)))) > 0) {
								if (input$wres_radio == "no") {
									o_cond$update <- 1
									updateRadioButtons(session, "wres_radio", selected = "yes")
								}
								else {
									if (isolate(o_parameter$wres_vfun) != input$wres_vfun) {
										updateTextInput(session, "wres_vfun", value = isolate(o_parameter$wres_vfun))
									}
									
									if (input$wres_cbox == F) {
										o_cond$update <- 1
										updateCheckboxInput(session, "wres_cbox", value = T)
									}
									else {
										if (length(input$wres_group) != length(isolate(o_parameter$wres_group))) {
											updateSelectizeInput(session, "wres_group", selected = isolate(o_parameter$wres_group))
										}
										else {
											if (length(which(!input$wres_group %in% isolate(o_parameter$wres_group))) > 0) {
												updateSelectizeInput(session, "wres_group", selected = isolate(o_parameter$wres_group))
											}
										}
									}
								}
							}
							else {
								if (input$wres_radio == "no") {
									o_cond$update <- 1
									updateRadioButtons(session, "wres_radio", selected = "yes")
								}
								else {
									if (input$wres_cbox == T) {
										updateCheckboxInput(session, "wres_cbox", value = F)
									}
									
									if (isolate(o_parameter$wres_vfun) != input$wres_vfun) {
										updateTextInput(session, "wres_vfun", value = isolate(o_parameter$wres_vfun))
									}
								}
							}
						}
						else {
							if (input$wres_radio == "yes") {
								updateRadioButtons(session, "wres_radio", selected = "no")
							}
						}
					}
					
					if (length(input$var_x) != length(isolate(o_parameter$x))) {
						updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
					}
					else {
						if (length(which(!input$var_x %in% isolate(o_parameter$x))) > 0) {
							updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
						}
					}
					
					if (isolate(o_parameter$f) != input$f_text) {
						updateTextInput(session, "f_text", value = isolate(o_parameter$f))
					}
					
					if (isolate(o_parameter$y) != input$var_y) {
						updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
					}
					
					if (!is.na(isolate(o_parameter$g))) {
						if (input$g_radio == "no") {
							updateRadioButtons(session, "g_radio", selected = "yes")
						}
						
						if (isolate(o_parameter$g) != input$g_text) {
							updateTextInput(session, "g_text", value = isolate(o_parameter$g))
						}
					}
					else {
						if (input$g_radio == "yes") {
							updateRadioButtons(session, "g_radio", selected = "no")
						}
					}
				}
				
				if (!is.na(isolate(o_parameter$group))) {
					if (input$group == "no") {
						o_cond$update <- 1
						updateRadioButtons(session, "group", selected = "yes")
					}
					else {
						if (isolate(o_parameter$concat2) != input$concat2) {
							o_cond$update <- 1
							updateCheckboxInput(session, "concat2", value = isolate(o_parameter$concat2))
						}
						else {
							if (isolate(o_parameter$concat2) == T) {
								if (length(input$var_group) != length(isolate(o_parameter$concat2_group))) {
									updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$concat2_group))
								}
								else {
									if (length(which(!input$var_group %in% isolate(o_parameter$concat2_group))) > 0) {
										updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$concat2_group))
									}
								}
							}
							else {
								if (length(input$var_group) != length(isolate(o_parameter$group))) {
									updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$group))
								}
								else {
									if (length(which(!input$var_group %in% isolate(o_parameter$group))) > 0) {
										updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$group))
									}
								}
							}
						}
					}
				}
				else {
					if (input$group == "yes") {
						updateRadioButtons(session, "group", selected = "no")
					}
				}
			}
			else if (input$data_type == "temporal") {
				if (isolate(o_parameter$x) != input$var_x) {
					updateSelectizeInput(session, "var_x", selected = isolate(o_parameter$x))
				}
				
				if (isolate(o_parameter$date_format) != input$date_format) {
					updateSelectInput(session, "date_format", selected = isolate(o_parameter$date_format))
				}
				
				if (length(input$var_y) != length(isolate(o_parameter$y))) {
					updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
				}
				else {
					if (length(which(!input$var_y %in% isolate(o_parameter$y))) > 0) {
						updateSelectizeInput(session, "var_y", selected = isolate(o_parameter$y))
					}
				} 
				
				if (!is.na(isolate(o_parameter$g))) {
					if (input$g_radio == "no") {
						updateRadioButtons(session, "g_radio", selected = "yes")
					}
					
					if (isolate(o_parameter$g) != input$g_text) {
						updateTextInput(session, "g_text", value = isolate(o_parameter$g))
					}
				}
				else {
					if (input$g_radio == "yes") {
						updateRadioButtons(session, "g_radio", selected = "no")
					}
				}
			}
			else {
				if (!is.na(isolate(o_parameter$id))) {
					if (input$id == "no") {
						updateRadioButtons(session, "id", selected = "yes")
					}
					
					if (isolate(o_parameter$id) != input$var_id) {
						updateSelectizeInput(session, "var_id", selected = isolate(o_parameter$id))
					}
				}
				else {
					if (input$id == "yes") {
						updateRadioButtons(session, "id", selected = "no")
					}
				}
				
				if (!is.na(isolate(o_parameter$group))) {
					if (input$group == "no") {
						o_cond$update <- 1
						updateRadioButtons(session, "group", selected = "yes")
					}
					else {
						if (isolate(o_parameter$concat2) != input$concat2) {
							o_cond$update <- 1
							updateCheckboxInput(session, "concat2", value = isolate(o_parameter$concat2))
						}
						else {
							if (isolate(o_parameter$concat2) == T) {
								if (length(input$var_group) != length(isolate(o_parameter$concat2_group))) {
									updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$concat2_group))
								}
								else {
									if (length(which(!input$var_group %in% isolate(o_parameter$concat2_group))) > 0) {
										updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$concat2_group))
									}
								}
							}
							else {
								if (length(input$var_group) != length(isolate(o_parameter$group))) {
									updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$group))
								}
								else {
									if (length(which(!input$var_group %in% isolate(o_parameter$group))) > 0) {
										updateSelectizeInput(session, "var_group", selected = isolate(o_parameter$group))
									}
								}
							}
						}
					}
				}
				else {
					if (input$group == "yes") {
						updateRadioButtons(session, "group", selected = "no")
					}
				}
			}
		}
	})
	
	# 1.5. Enable/disable multiple buttons in left panel 
	# ==================================================
	
	observe({
		shinyjs::toggleState("load1_button", !is.null(input$data_path1) && input$data_path1 != "")
		shinyjs::toggleState("load2_button", !is.null(input$data_path2) && input$data_path2 != "")
		shinyjs::toggleState("load3_button", !is.null(input$data_path3) && input$data_path3 != "")
		shinyjs::toggleState("del1_button", length(which(!is.na(input$ref))) > 0 && isolate(o_cond$del)[1] == 1)
		shinyjs::toggleState("del2_button", length(which(!is.na(input$var_x))) > 0 && isolate(o_cond$del)[2] == 1)
		shinyjs::toggleState("del3_button", length(which(!is.na(input$var_y))) > 0 && isolate(o_cond$del)[3] == 1)
		shinyjs::toggleState("del4_button", length(which(!is.na(input$wres_group))) > 0 && isolate(o_cond$del)[4] == 1)
		shinyjs::toggleState("del5_button", length(which(!is.na(input$var_group))) > 0 && isolate(o_cond$del)[5] == 1)
		shinyjs::toggleState("expand1_button", input$f_radio == "yes")
		shinyjs::toggleState("expand2_button", input$g_radio == "yes")
		shinyjs::toggleState("expand3_button", input$h_radio == "yes")
		shinyjs::toggleState("expand4_button", input$wres_radio == "yes")
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
		o_path$name1_prev <- NA
		o_path$name2_prev <- NA
		o_path$name2 <- NA
		o_data_info$mtime <- rep(NA, 3)
		o_data_info$code <- NA
		
		if (isolate(o_click_button$load1) == 1 | isolate(o_click_button$load2) == 1) {
			rm(list = ls(e_data), envir = e_data)
			js$backgroundCol("data_path1", "white")
			o_path$color1 <- "white"
			o_flag$name <- NA
			o_load_error$code <- rep(0, 3)
			updateTextInput(session, "data_path2", value = "")
			o_click_button$load1 <- 0
			o_click_button$load2 <- 0
			o_click_button$browse2 <- 0
			o_cond$label <- rep(0, 4)
			
			if (isolate(o_path$color3) != "transparent") {
				o_path$name3_prev <- NA
				o_path$name3 <- NA
				o_path$color3 <- "transparent"
				o_click_button$load3 <- 0
				o_click_button$browse3 <- 0
			}
			
			if (!is.na(isolate(o_norm_select$type))) {
				o_norm_select$type <- NA
			}
			
			o_reset$code <- 1
			click("reset1_button")
		}
		
		if (input$data_type != "ir") {
			js$backgroundCol("data_path2", "transparent")
			o_path$color2 <- "transparent"
		
			if (input$data_path2 != "") {
				updateTextInput(session, "data_path2", value = "")
				shinyjs::delay(100, disable("data_path2"))
			}
			else {
				shinyjs::disable("data_path2")
			}
			
			shinyjs::disable("browse2_button")
		}
		else {
			js$backgroundCol("data_path2", "white")
			o_path$color2 <- "white"
			shinyjs::enable("data_path2")
			shinyjs::enable("browse2_button")
		}
	})
	
	# ------------
	# Data loading
	# ------------
	
	# 2.2. Add events of flag checkbox
	# ================================
	
	observeEvent(input$flag, {
		if (isolate(o_click_button$load1) == 1 & isolate(o_cond$save1) == 0) {
			o_cond$label <- rep(0, 4)
			o_flag$name <- NA
			o_load_error$code[1] <- 0
			js$backgroundCol("data_path1", "white")
			o_path$name1_prev <- NA
			o_path$color1 <- "white" 
			o_click_button$load1 <- 0
			o_data_info$mtime[1] <- NA
			o_data_info$code <- NA
			
			if (isolate(o_path$color3) != "transparent") {
				o_path$name3_prev <- NA
				o_path$name3 <- NA
				o_path$color3 <- "transparent"
				o_click_button$load3 <- 0
				o_click_button$browse3 <- 0
				o_data_info$mtime[3] <- NA
			}
			
			if ("all" %in% ls(e_data)) {
				rm(list = "all", envir = e_data)
			}
			
			if ("flag" %in% ls(e_data)) {
				rm(list = "flag", envir = e_data)
			}
			
			o_reset$code <- 1
			click("reset1_button")
		}
		else {
			if (isolate(o_cond$save1) == 1) {
				o_cond$save1 <- 0
			}
		}
	})
	
	# 2.3. Add events on data path inputs 
	# ===================================
	
	# data_path1
	
	observeEvent(input$data_path1, {
		if (!is.na(isolate(o_path$name1))) {
			o_path$name1_prev <- isolate(o_path$name1)
		}
		
		if (!is.na(isolate(o_path$name1_prev))) {
			if (isolate(o_click_button$browse1) == 1 & input$data_path1 == "") {
				updateTextInput(session, "data_path1", value = isolate(o_path$name1))
			}
			else {
				o_path$name1 <- input$data_path1
			}
		}
		else {
			if (input$data_path1 != "") {
				o_path$name1 <- input$data_path1
			}
		}
		
		if (isolate(o_click_button$browse1) == 1) {
			o_click_button$browse1 <- 0
		}
		
		if (isolate(o_click_button$load1) == 1) {
			if (length(unique(c(isolate(o_path$name1_prev), isolate(o_path$name1)))) == 1 & !is.na(isolate(o_path$name1_prev))) {
				js$backgroundCol("data_path1", isolate(o_path$color1))
			}
			else {
				o_load_error$code[1] <- 0
				o_cond$label <- rep(0, 4)
				js$backgroundCol("data_path1", "white")
				o_path$color1 <- "white"
				o_click_button$load1 <- 0
				o_flag$name <- NA
				o_data_info$mtime[1] <- NA
				o_data_info$code <- NA
				
				if (isolate(o_path$color3) != "transparent") {
					o_path$name3_prev <- NA
					o_path$name3 <- NA
					o_path$color3 <- "transparent"
					o_click_button$load3 <- 0
					o_click_button$browse3 <- 0
					o_data_info$mtime[3] <- NA
				}
				
				if ("all" %in% ls(e_data)) {
					rm(list = "all", envir = e_data)
				}
				
				if ("flag" %in% ls(e_data)) {
					rm(list = "flag", envir = e_data)
				}
				
				o_reset$code <- 1
				click("reset1_button")
			}
		}
	})
	
	# data_path2
	
	observeEvent(input$data_path2, {
		if (!is.na(isolate(o_path$name2))) {
			o_path$name2_prev <- isolate(o_path$name2)
		}
		
		if (!is.na(isolate(o_path$name2_prev))) {
			if (isolate(o_click_button$browse2) == 1 & input$data_path2 == "") {
				updateTextInput(session, "data_path2", value = isolate(o_path$name2))
			}
			else {
				o_path$name2 <- input$data_path2
			}
		}
		else {
			if (input$data_path2 != "") {
				o_path$name2 <- input$data_path2
			}
		}
		
		if (isolate(o_click_button$browse2) == 1) {
			o_click_button$browse2 <- 0
		}
		
		if (isolate(o_click_button$load2) == 1) {
			if (length(unique(c(isolate(o_path$name2_prev), isolate(o_path$name2)))) == 1 & !is.na(isolate(o_path$name2_prev))) {
				js$backgroundCol("data_path2", isolate(o_path$color2))
			}
			else {
				o_load_error$code[2] <- 0
				o_cond$label <- rep(0, 4)
				js$backgroundCol("data_path2", "white")
				o_path$color2 <- "white"
				o_click_button$load2 <- 0
				o_data_info$mtime[2] <- NA
				
				if ("code_freq" %in% ls(e_data)) {
					rm(list = "code_freq", envir = e_data)
				}
				
				o_reset$code <- 1
				click("reset1_button")
			}
		}
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
	
	observeEvent(input$load1_button, {
		if (isolate(o_path$color1) == "lightgreen") {
			n_time <- as.numeric(file.info(input$data_path1)$mtime)
			
			if (n_time == isolate(o_data_info$mtime)[1]) {
				b_cond <- F
			}
			else {
				b_cond <- T
			}
		}
		else {
			b_cond <- F 
		}
		
		if ((length(unique(c(isolate(o_path$name1_prev), isolate(o_path$name1)))) > 1 & !is.na(isolate(o_path$name1_prev))) | isolate(o_click_button$load1) == 0 | isolate(o_load_error$code)[1] == 1 | b_cond == T) {
			if (b_cond == T) {
				if ("all" %in% ls(e_data)) {
					rm(list = "all", envir = e_data)
				}
				
				if ("flag" %in% ls(e_data)) {
					rm(list = "flag", envir = e_data)
				}
				
				o_reset$code <- 1
				click("reset1_button")
			}
			
			o_click_button$load1 <- 1
			s_e_message <- NA
			df_1 <- tryCatch({suppressWarnings(fread(input$data_path1, data.table = F))}, error = function(e) F) 
			
			if (!is.data.frame(df_1)) {
				s_e_message <- "Path/file doesn't exist"
			}
			else {
				if (dim(df_1)[1] == 0) {
					s_e_message <- "Data is empty"
				}
				else {
					v_split_path <- unlist(strsplit(input$data_path1, split = "[.]"))
					o_flag$name <- NA
					
					if (input$data_type == "normal") {
						s_flag_path_1 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_norm_flag.", v_split_path[length(v_split_path)])
						s_flag_path_2 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_norm_flag_withID.", v_split_path[length(v_split_path)])
						v_split_path_1 <- unlist(strsplit(s_flag_path_1, split = "/"))
						v_split_path_2 <- unlist(strsplit(s_flag_path_2, split = "/"))
						b_cond_1 <- v_split_path_1[length(v_split_path_1)] %in% list.files(paste0(paste(v_split_path_1[1:(length(v_split_path_1) - 1)], collapse = "/"), "/"))
						b_cond_2 <- v_split_path_2[length(v_split_path_2)] %in% list.files(paste0(paste(v_split_path_2[1:(length(v_split_path_2) - 1)], collapse = "/"), "/"))
						b_cond <- b_cond_1 | b_cond_2
					}
					else if (input$data_type == "temporal") {
						s_flag_path <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_temp_flag.", v_split_path[length(v_split_path)])
						v_split_path <- unlist(strsplit(s_flag_path, split = "/"))
						b_cond <- v_split_path[length(v_split_path)] %in% list.files(paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/"))
						o_flag$name <- ifelse(b_cond == T, v_split_path[length(v_split_path)], NA)
					}
					else {
						s_flag_path_1 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_ir_flag.", v_split_path[length(v_split_path)])
						s_flag_path_2 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_ir_flag_withID.", v_split_path[length(v_split_path)])
						v_split_path_1 <- unlist(strsplit(s_flag_path_1, split = "/"))
						v_split_path_2 <- unlist(strsplit(s_flag_path_2, split = "/"))
						b_cond_1 <- v_split_path_1[length(v_split_path_1)] %in% list.files(paste0(paste(v_split_path_1[1:(length(v_split_path_1) - 1)], collapse = "/"), "/"))
						b_cond_2 <- v_split_path_2[length(v_split_path_2)] %in% list.files(paste0(paste(v_split_path_2[1:(length(v_split_path_2) - 1)], collapse = "/"), "/"))
						b_cond <- b_cond_1 | b_cond_2
					}
					
					if (input$data_type == "normal") { 
						v_pos <- which(c(".concat1.", ".concat2.") %in% names(df_1) == T)
						
						if (length(v_pos) > 0) {
							if (length(v_pos) == 2) {
								o_cond$concat1 <- 1
								o_cond$concat2 <- 1
								showNotification("The concatenation option will be disabled for X and Group variables, because variables named .concat1. and .concat2. already exists in loaded data", duration = 15, type = "warning")
							}
							else {
								if (v_pos == 1) {
									o_cond$concat1 <- 1
									showNotification("The concatenation option will be disabled for X variable, because the variable named .concat1. already exists in loaded data", duration = 15, type = "warning")
								}
								else {
									o_cond$concat2 <- 1
									showNotification("The concatenation option will be disabled for Group variable, because the variable named .concat2. already exists in loaded data", duration = 15, type = "warning")
								}
							}
						}
					}
					
					if (input$flag == T) {
						if (b_cond == T) {
							if (input$data_type == "normal") {
								if (b_cond_1 == T & b_cond_2 == T) {
									s_e_message <- paste0("The flag file is not unique. Please keep one file (", v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)], ").")
								}
								else {
									if (b_cond_2 == T) {
										o_flag$name <- v_split_path_2[length(v_split_path_2)]
										df_flag <- fread(s_flag_path_2, data.table = F)
										
										if (dim(df_flag)[1] == 0) {
											s_e_message <- "Flag data is empty"
										}
										else {
											if (names(df_flag)[1] %in% names(df_1)) {
												e_data$all <- df_1
												e_data$flag <- df_flag
												shinyjs::enable("plot_type")
												shinyjs::enable("dim_num")
												shinyjs::enable("model")
												shinyjs::enable("id")
												updateRadioButtons(session, "id", selected = "yes")
												shinyjs::delay(100, disable("id"))
												shinyjs::enable("var_x")
												updateSelectizeInput(session, "var_x", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
												shinyjs::enable("f_radio")
												shinyjs::enable("var_y")
												updateSelectizeInput(session, "var_y", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
												shinyjs::enable("g_radio")
												shinyjs::enable("group")
												shinyjs::enable("display_button")
												shinyjs::enable("webgl")
												shinyjs::enable("op_radio")
												shinyjs::enable("dec_num_radio")
												shinyjs::enable("edit1_radio")
												shinyjs::enable("edit2_radio")
												shinyjs::enable("edit3_radio")
												shinyjs::enable("lreg")
												shinyjs::enable("conf_ellipsoid")
												shinyjs::enable("centroid")
												o_norm_select$type <- "plot"
											}
											else {
												s_e_message <- paste0("ID flag variable (", names(e_data$flag)[1], ") is missing in data")
											}
										}
									}
									else {
										o_flag$name <- v_split_path_1[length(v_split_path_1)]
										df_flag <- fread(s_flag_path_1, data.table = F)
										
										if (dim(df_flag)[1] == 0) {
											s_e_message <- "Flag data is empty"
										}
										else {
											e_data$all <- df_1
											e_data$flag <- df_flag
											shinyjs::enable("plot_type")
											shinyjs::enable("dim_num")
											shinyjs::enable("model")
											
											if (input$id == "yes") {
												updateRadioButtons(session, "id", selected = "no")
												shinyjs::delay(100, disable("id"))
											}
											else {
												shinyjs::disable("id")
											}
											
											shinyjs::enable("var_x")
											updateSelectizeInput(session, "var_x", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
											shinyjs::enable("f_radio")
											shinyjs::enable("var_y")
											updateSelectizeInput(session, "var_y", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
											shinyjs::enable("g_radio")
											shinyjs::enable("group")
											shinyjs::enable("display_button")
											shinyjs::enable("webgl")
											shinyjs::enable("op_radio")
											shinyjs::enable("dec_num_radio")
											shinyjs::enable("edit1_radio")
											shinyjs::enable("edit2_radio")
											shinyjs::enable("edit3_radio")
											shinyjs::enable("lreg")
											shinyjs::enable("conf_ellipsoid")
											shinyjs::enable("centroid")
											o_norm_select$type <- "plot"
										}
									}
								}
							}
							else if (input$data_type == "temporal") {
								df_flag <- fread(s_flag_path, data.table = F)
								
								if (dim(df_flag)[1] == 0) {
									s_e_message <- "Flag data is empty"
								}
								else {
									o_cond$del[3] <- 1
									e_data$all <- df_1
									e_data$flag <- df_flag
									shinyjs::enable("var_x")
									updateSelectizeInput(session, "var_x", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
									shinyjs::enable("date_format")
									updateSelectInput(session, "date_format", choices = c("%Y%m%d", "%Y%m%d%H%M", "%d%m%Y", "%d%m%Y%H%M"))
									shinyjs::enable("var_y")
									updateSelectizeInput(session, "var_y", choices = names(df_1), options = list(maxOptions = 9999, maxItems = 9999))
									shinyjs::enable("g_radio")
									shinyjs::enable("webgl")
									shinyjs::enable("mode")
									shinyjs::enable("op_radio")
									shinyjs::enable("y_scale")
									shinyjs::enable("dec_num_radio")
									shinyjs::enable("display_button")
									shinyjs::enable("edit1_radio")
									shinyjs::enable("edit3_radio")
								}
							}
							else {
								if (b_cond_1 == T & b_cond_2 == T) {
									s_e_message <- paste0("The flag file is not unique. Please keep one file (", v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)], ").")
								}
								else {
									if (b_cond_2 == T) {
										o_flag$name <- v_split_path_2[length(v_split_path_2)]
										df_flag <- fread(s_flag_path_2, data.table = F)
										
										if (dim(df_flag)[1] == 0) {
											s_e_message <- "Flag data is empty"
										}
										else {
											if (!names(df_flag)[1] %in% names(df_1)) {
												s_e_message <- paste0("ID flag variable (", names(e_data$flag)[1], ")")
											}
											
											v_pos_1 <- which(substr(names(df_1), 1, 1) %in% c("M", "N"))
											
											if (length(v_pos_1) > 0) {
												v_pos_2 <- which(!is.na(suppressWarnings(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))) 
												
												if (length(v_pos_2) > 0) {
													v_pos_1 <- v_pos_1[v_pos_2]
													b_cond_3 <- T
												}
												else {
													b_cond_3 <- F
												}
											}
											else {
												b_cond_3 <- F
											}
											
											if (b_cond_3 == T) {
												eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_1$", names(df_1)[v_pos_1], ") | length(which(!is.na(df_1$", names(df_1)[v_pos_1], "))) == 0"), collapse = ", "), ")")))
												
												if (length(which(v_cond == F)) > 0) {
													if (!is.na(s_e_message)) {
														s_e_message <- paste0(s_e_message, "is missing in data<br/>", paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(v_cond == F)], collapse = ", "))
													}
													else {
														s_e_message <- paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(v_cond == F)], collapse = ", ")
													}
												}
												else {
													if (!is.na(s_e_message)) {
														s_e_message <- paste0(s_e_message, "is missing in data")
													}
													else {
														e_data$all <- df_1
														e_data$flag <- df_flag
														v_code_range <- paste0(substr(names(df_1)[v_pos_1[1]], 1, 1), range(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))
														shinyjs::enable("id")
														updateRadioButtons(session, "id", selected = "yes")
														shinyjs::delay(100, disable("id"))
														shinyjs::enable("group")
														
														if ("code_freq" %in% ls(e_data)) {
															shinyjs::enable("display_button")
															shinyjs::enable("webgl")
															shinyjs::enable("mode")
															shinyjs::enable("op_radio")
															shinyjs::enable("y_scale")
															shinyjs::enable("dec_num_radio")
															shinyjs::enable("edit1_radio")
															shinyjs::enable("edit3_radio")
															shinyjs::enable("mean_spect")
														}
													}
												}
											}
											else {
												if (!is.na(s_e_message)) {
													s_e_message <- paste0(s_e_message, "and all code variables are missing in data")
												}
												else {
													s_e_message <- "All code variables are missing in data"
												}
											}
										}
									}
									else {
										o_flag$name <- v_split_path_1[length(v_split_path_1)]
										df_flag <- fread(s_flag_path_1, data.table = F)
										
										if (dim(df_flag)[1] == 0) {
											s_e_message <- "Flag data is empty"
										}
										else {
											v_pos_1 <- which(substr(names(df_1), 1, 1) %in% c("M", "N"))
											
											if (length(v_pos_1) > 0) {
												v_pos_2 <- which(!is.na(suppressWarnings(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))) 
												
												if (length(v_pos_2) > 0) {
													v_pos_1 <- v_pos_1[v_pos_2]
													b_cond_3 <- T
												}
												else {
													b_cond_3 <- F
												}
											}
											else {
												b_cond_3 <- F
											}
											
											if (b_cond_3 == T) {
												eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_1$", names(df_1)[v_pos_1], ") | length(which(!is.na(df_1$", names(df_1)[v_pos_1], "))) == 0"), collapse = ", "), ")")))

												if (length(which(v_cond == F)) > 0) {
													s_e_message <- paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(v_cond == F)], collapse = ", ")
												}
												else {
													e_data$all <- df_1
													e_data$flag <- df_flag
													v_code_range <- paste0(substr(names(df_1)[v_pos_1[1]], 1, 1), range(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))
													
													if (input$id == "yes") {
														updateRadioButtons(session, "id", selected = "no")
														shinyjs::delay(100, disable("id"))
													}
													else {
														shinyjs::disable("id")
													}
													
													shinyjs::enable("group")
													
													if ("code_freq" %in% ls(e_data)) {
														shinyjs::enable("display_button")
														shinyjs::enable("webgl")
														shinyjs::enable("mode")
														shinyjs::enable("op_radio")
														shinyjs::enable("y_scale")
														shinyjs::enable("dec_num_radio")
														shinyjs::enable("edit1_radio")
														shinyjs::enable("edit3_radio")
														shinyjs::enable("mean_spect")
													}
												}
											}
											else {
												s_e_message <- "All code variables are missing in data"
											}
										}
									}
								}
							}
						}
						else {
							s_e_message <- paste0("Flag data doesn't exist (", ifelse(input$data_type == "temporal", v_split_path[length(v_split_path)], paste0(v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)])), "). Please uncheck the flag box.")
						}
					}
					else {
						if (b_cond == T) {
							if (input$data_type == "temporal") {
								s_flag_name <- v_split_path[length(v_split_path)]
							}
							else {
								if (b_cond_1 == T) {
									s_flag_name <- v_split_path_1[length(v_split_path_1)]
								}
								else {
									s_flag_name <- v_split_path_2[length(v_split_path_2)]
								}
							}
							
							s_message <- paste0("A flag file (", s_flag_name, ") exists but the corresponding box is not checked. If you save new flags, the previous flag(s) will be removed. Please check the box, if you want to keep previous flag(s).")
							showNotification(s_message, duration = 15, type = "warning")
						}
						
						if (input$data_type != "ir") {
							e_data$all <- df_1
							shinyjs::enable("var_x")
							updateSelectizeInput(session, "var_x", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
							shinyjs::enable("var_y")
							
							if (input$data_type == "temporal") {
								updateSelectizeInput(session, "var_y", choices = names(df_1), options = list(maxOptions = 9999, maxItems = 9999))
							}
							else {
								updateSelectizeInput(session, "var_y", choices = names(df_1), selected = names(df_1)[1], options = list(maxOptions = 9999, maxItems = 1))
							}
							
							shinyjs::enable("g_radio")
							shinyjs::enable("display_button")
							shinyjs::enable("webgl")
							shinyjs::enable("op_radio")
							shinyjs::enable("dec_num_radio")
							shinyjs::enable("edit1_radio")
							shinyjs::enable("edit3_radio")
							
							if (input$data_type == "temporal") {
								o_cond$del[3] <- 1
								shinyjs::enable("date_format")
								updateSelectInput(session, "date_format", choices = c("%Y%m%d", "%Y%m%d%H%M", "%d%m%Y", "%d%m%Y%H%M"))
								shinyjs::enable("mode")
								shinyjs::enable("y_scale")
							}
							else {
								shinyjs::enable("plot_type")
								shinyjs::enable("dim_num")
								shinyjs::enable("model")
								shinyjs::enable("id")
								shinyjs::enable("f_radio")
								shinyjs::enable("group")
								shinyjs::enable("edit2_radio")
								shinyjs::enable("lreg")
								shinyjs::enable("conf_ellipsoid")
								shinyjs::enable("centroid")
								o_norm_select$type <- "plot"
							}
						}
						else {
							v_pos_1 <- which(substr(names(df_1), 1, 1) %in% c("M", "N"))
							
							if (length(v_pos_1) > 0) {
								v_pos_2 <- which(!is.na(suppressWarnings(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))) 
								
								if (length(v_pos_2) > 0) {
									v_pos_1 <- v_pos_1[v_pos_2]
									b_cond_3 <- T
								}
								else {
									b_cond_3 <- F
								}
							}
							else {
								b_cond_3 <- F
							}
							
							if (b_cond_3 == T) {
								eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_1$", names(df_1)[v_pos_1], ") | length(which(!is.na(df_1$", names(df_1)[v_pos_1], "))) == 0"), collapse = ", "), ")")))
								
								if (length(which(v_cond == F)) > 0) {
									s_e_message <- paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(v_cond == F)], collapse = ", ")
								}
								else {
									e_data$all <- df_1
									v_code_range <- paste0(substr(names(df_1)[v_pos_1[1]], 1, 1), range(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))
									shinyjs::enable("id")
									shinyjs::enable("group")
									
									if ("code_freq" %in% ls(e_data)) {
										shinyjs::enable("display_button")
										shinyjs::enable("webgl")
										shinyjs::enable("mode")
										shinyjs::enable("op_radio")
										shinyjs::enable("y_scale")
										shinyjs::enable("dec_num_radio")
										shinyjs::enable("edit1_radio")
										shinyjs::enable("edit3_radio")
										shinyjs::enable("mean_spect")
									}
								}
							}
							else {
								s_e_message <- "All code variables are missing in data"
							}
						}
					}
				}
			}
			
			if (!is.na(s_e_message)) {
				f_showNotification(s_e_message, duration = 15, type = "error")
				js$backgroundCol("data_path1", "salmon")
				o_path$color1 <- "salmon"
				o_load_error$code[1] <- 1
				o_data_info$mtime[1] <- NA
				
				if (input$data_type == "ir") {
					o_data_info$code <- NA
				}
			}
			else {
				js$backgroundCol("data_path1", "lightgreen")
				o_path$color1 <- "lightgreen"
				o_load_error$code[1] <- 0
				o_data_info$mtime[1] <- as.numeric(file.info(input$data_path1)$mtime)
				
				if (input$data_type == "ir") {
					o_data_info$code <- v_code_range
				}
			}
		}
	})
	
	# load2 button (only IR data)
	
	observeEvent(input$load2_button, {
		if (isolate(o_path$color2) == "lightgreen") {
			n_time <- as.numeric(file.info(input$data_path2)$mtime)
			
			if (n_time == isolate(o_data_info$mtime)[2]) {
				b_cond <- F
			}
			else {
				b_cond <- T
			}
		}
		else {
			b_cond <- F 
		}
		
		if ((length(unique(c(isolate(o_path$name2_prev), isolate(o_path$name2)))) > 1 & !is.na(isolate(o_path$name2_prev))) | isolate(o_click_button$load2) == 0 | isolate(o_load_error$code)[2] == 1 | b_cond == T) {
			if (b_cond == T) {
				if ("code_freq" %in% ls(e_data)) {
					rm(list = "code_freq", envir = e_data)
				}
				
				o_reset$code <- 1
				click("reset1_button")
			}
			
			o_click_button$load2 <- 1
			s_e_message <- NA
			df_1 <- tryCatch({suppressWarnings(fread(input$data_path2, data.table = F))}, error = function(e) F) 
			
			if (!is.data.frame(df_1)) {
				s_e_message <- "Path/file doesn't exist"
				showNotification(s_e_message, duration = 15, type = "error")
			}
			else {
				if (dim(df_1)[1] == 0) {
					s_e_message <- "Code/frequency data is empty"
					showNotification(s_e_message, duration = 15, type = "error")
				}
				else {
					v_cond <- c("Code", "Frequency") %in% names(df_1)
					v_pos <- which(v_cond == F)
					
					if (length(v_pos) > 0) {
						s_e_message <- paste0(paste(c("Code", "Frequency")[v_pos], sep = " and "), " variable")
						
						if (length(v_pos) == 2) {
							s_e_message <- paste0(s_e_message, "s")
						}
						
						s_e_message <- paste0(s_e_message, " missing in code-frequency data")
						showNotification(s_e_message, duration = 15, type = "error")
					}
					else {
						e_data$code_freq <- df_1
						
						if ("all" %in% ls(e_data)) {
							shinyjs::enable("display_button")
							shinyjs::enable("webgl")
							shinyjs::enable("mode")
							shinyjs::enable("op_radio")
							shinyjs::enable("y_scale")
							shinyjs::enable("dec_num_radio")
							shinyjs::enable("edit1_radio")
							shinyjs::enable("edit3_radio")
							shinyjs::enable("mean_spect")
						}
					}
				}
			}
			
			if (!is.na(s_e_message)) {
				js$backgroundCol("data_path2", "salmon")
				o_path$color2 <- "salmon"
				o_load_error$code[2] <- 1
				o_data_info$mtime[2] <- NA
			}
			else {
				js$backgroundCol("data_path2", "lightgreen")
				o_path$color2 <- "lightgreen"
				o_load_error$code[2] <- 0
				o_data_info$mtime[2] <- as.numeric(file.info(input$data_path2)$mtime)
			}
		}
	})
	
	# ---------------------
	# Normal plot selection
	# ---------------------
	
	# 2.6. Add events of plot type radio buttons 
	# ==========================================
	
	observeEvent(input$plot_type, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
			o_cond$label <- rep(0, 4)
		
			if (isolate(o_click_button$display) == 1 & isolate(o_norm_select$type) == "plot") {
				if (!is.null(input$var_flag_1)) {
					updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
					shinyjs::delay(100, disable("var_flag_1"))
				}
				else {
					shinyjs::disable("var_flag_1")
				}
				
				if (input$var_flag_2 == T) {
					updateCheckboxInput(session, "var_flag_2", value = F)
					shinyjs::delay(100, disable("var_flag_2"))
				}
				else {
					shinyjs::disable("var_flag_2")
				}
			}
			
			if (input$ref_radio == "yes" | input$wres_radio == "yes" | isolate(o_cond$del)[2] == 1) {
				o_reset$model2 <- 1
			}
			
			click("reset1_button")
			o_cond$del[3] <- ifelse(input$plot_type == "corplot", 1, 0)
			
			if (isolate(o_cond$concat1) == 0 & ".concat1." %in% names(e_data$all)) {
				e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat1.")]
			}
			
			if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data$all)) {
				e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat2.")]
			}
			
			if (input$plot_type == "plot") {
				shinyjs::enable("dim_num")
				shinyjs::enable("model")
				
				if ("flag" %in% ls(e_data)) {
					if (substr(isolate(o_flag$name), nchar(isolate(o_flag$name)) - 9, nchar(isolate(o_flag$name)) - 4) == "withID") {
						shinyjs::enable("id")
						updateRadioButtons(session, "id", selected = "yes")
						shinyjs::delay(100, disable("id"))
					}
					else {
						if (input$id == "yes") {
							updateRadioButtons(session, "id", selected = "no")
							shinyjs::delay(100, disable("id"))
						}
						else {
							shinyjs::disable("id")
						}
					}
				}
				else {
					shinyjs::enable("id")
				}
				
				if (isolate(o_cond$concat1) == 1) {
					if (isolate(o_norm_select$type) == "corplot") {
						shinyjs::enable("var_x")
						updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
					}
				}
				else {
					if (isolate(o_norm_select$type) != "corplot") {
						if (isolate(o_norm_select$type) != "histplot") {
							if (input$concat1 == T) {
								updateCheckboxInput(session, "concat1", value = F)
								shinyjs::delay(100, disable("concat1"))
							}
							else {
								shinyjs::disable("concat1")
							}
						}
					}
					else {
						shinyjs::enable("var_x")
						updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
					}
				}
				
				shinyjs::enable("f_radio")
				
				if (isolate(o_norm_select$type) %in% c("histplot", "barplot", "corplot")) {
					if (isolate(o_norm_select$type) != "corplot") {
						shinyjs::enable("var_y")
					}
					
					updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
				}
				
				o_norm_select$type <- "plot"
				shinyjs::enable("g_radio")
				shinyjs::enable("webgl")
				
				if (input$webgl == "no") {
					updateRadioButtons(session, "webgl", selected = "yes")
				}
				
				shinyjs::enable("op_radio")
				shinyjs::enable("edit1_radio")
				shinyjs::enable("edit2_radio")
				shinyjs::enable("edit3_radio")
				
				if (input$op_radio != "auto") {
					updateNumericInput(session, "op", min = 0, max = 1)
				}
				
				shinyjs::enable("lreg")
				shinyjs::enable("conf_ellipsoid")
				shinyjs::enable("centroid")
				
				if (input$box_mean_sd == T) {
					updateCheckboxInput(session, "box_mean_sd", value = F)
					shinyjs::delay(100, disable("box_mean_sd"))
				}
				else {
					shinyjs::disable("box_mean_sd")
				}
				
				if (input$bin_radio == "manual") {
					updateRadioButtons(session, "bin_radio", selected = "auto")
					shinyjs::delay(100, disable("bin_radio"))
				}
				else {
					shinyjs::disable("bin_radio")
				}
				
				if (input$dens_curve == T) {
					updateCheckboxInput(session, "dens_curve", value = F)
					shinyjs::delay(100, disable("dens_curve"))
				}
				else {
					shinyjs::disable("dens_curve")
				}
				
				if (input$norm_dens_curve == T) {
					updateCheckboxInput(session, "norm_dens_curve", value = F)
					shinyjs::delay(100, disable("norm_dens_curve"))
				}
				else {
					shinyjs::disable("norm_dens_curve")
				}
			}
			else {
				if (input$dim_num == "3d") {
					updateRadioButtons(session, "dim_num", selected = "2d")
					shinyjs::delay(100, disable("dim_num"))
				}
				else {
					shinyjs::disable("dim_num")
				}
				
				if (input$model != "none") {
					updateRadioButtons(session, "model", selected = "none")
					shinyjs::delay(100, disable("model"))
				}
				else {
					shinyjs::disable("model")
				}
				
				if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
					shinyjs::enable("display_button")
				}
				
				if (input$id == "yes") {
					updateRadioButtons(session, "id", selected = "no")
					shinyjs::delay(100, disable("id"))
				}
				else {
					shinyjs::disable("id")
				}
				
				if (input$webgl != "no") {
					updateRadioButtons(session, "webgl", selected = "no")
					shinyjs::delay(100, disable("webgl"))
				}
				else {
					shinyjs::disable("webgl")
				}
				
				if (input$lreg == T) {
					updateCheckboxInput(session, "lreg", value = F)
					shinyjs::delay(100, disable("lreg"))
				}
				else {
					shinyjs::disable("lreg")
				}
				
				if (input$conf_ellipsoid == T) {
					updateCheckboxInput(session, "conf_ellipsoid", value = F)
					shinyjs::delay(100, disable("conf_ellipsoid"))
				}
				else {
					shinyjs::disable("conf_ellipsoid")
				}
				
				if (input$centroid == T) {
					updateCheckboxInput(session, "centroid", value = F)
					shinyjs::delay(100, disable("centroid"))
				}
				else {
					shinyjs::disable("centroid")
				}
				
				if (input$plot_type == "boxplot") {
					if (isolate(o_cond$concat1) == 1) {
						if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
							if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
								shinyjs::enable("var_x")
							}
							
							updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
						}
					}
					else {
						if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
							if (isolate(o_norm_select$type) != "barplot") {
								shinyjs::enable("concat1")
							}
						}
						else {
							shinyjs::enable("concat1")
							
							if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
								shinyjs::enable("var_x")
							}
							
							updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
						}
					}
					
					if (input$f_radio == "yes") {
						if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
							shinyjs::enable("f_radio")
						}
						
						updateRadioButtons(session, "f_radio", selected = "no")
						shinyjs::delay(100, disable("f_radio"))
					}
					else {
						shinyjs::disable("f_radio")
					}
					
					if (isolate(o_norm_select$type) %in% c("histplot", "barplot", "corplot", "calib_off", "valid_off")) {
						if (isolate(o_norm_select$type) != "corplot") {
							shinyjs::enable("var_y")
						}
						
						updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
						
						if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
							shinyjs::enable("group")
						}
					}
					
					o_norm_select$type <- "boxplot"
					shinyjs::enable("g_radio")
					shinyjs::enable("op_radio")
					
					if (input$op_radio != "auto") {
						updateNumericInput(session, "op", min = 0, max = 1)
					}
					
					shinyjs::enable("edit1_radio")
					
					if (input$edit2_radio == "yes") {
						updateRadioButtons(session, "edit2_radio", selected = "no")
						shinyjs::delay(100, disable("edit2_radio"))
					}
					else {
						shinyjs::disable("edit2_radio")
					}
					
					shinyjs::enable("edit3_radio")
					shinyjs::enable("box_mean_sd")
					
					if (input$bin_radio == "manual") {
						updateRadioButtons(session, "bin_radio", selected = "auto")
						shinyjs::delay(100, disable("bin_radio"))
					}
					else {
						shinyjs::disable("bin_radio")
					}
					
					if (input$dens_curve == T) {
						updateCheckboxInput(session, "dens_curve", value = F)
						shinyjs::delay(100, disable("dens_curve"))
					}
					else {
						shinyjs::disable("dens_curve")
					}
					
					if (input$norm_dens_curve == T) {
						updateCheckboxInput(session, "norm_dens_curve", value = F)
						shinyjs::delay(100, disable("norm_dens_curve"))
					}
					else {
						shinyjs::disable("norm_dens_curve")
					}
				}
				else {
					if (input$plot_type == "histplot") {
						if (isolate(o_cond$concat1) == 1) {
							if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
								if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
									shinyjs::enable("var_x")
								}
								
								updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
							}
						}
						else {
							if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
								if (isolate(o_norm_select$type) != "plot") {
									if (input$concat1 == T) {
										updateCheckboxInput(session, "concat1", value = F)
										shinyjs::delay(100, disable("concat1"))
									}
									else {
										shinyjs::disable("concat1")
									}
								}
							}
							else {
								if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
									shinyjs::enable("var_x")
								}
								
								updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
							}
						}
						
						shinyjs::enable("f_radio")
						
						if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
							updateRadioButtons(session, "f_radio", selected = "no")
						}
						
						if (!isolate(o_norm_select$type) %in% c("barplot", "calib_off", "valid_off")) {
							updateSelectizeInput(session, "var_y", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
							shinyjs::delay(100, disable("var_y"))
						}
						
						if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
							shinyjs::enable("group")
						}
						
						o_norm_select$type <- "histplot"
						shinyjs::enable("op_radio")
						
						if (input$op_radio != "auto") {
							if (input$op > 0.7) {
								updateNumericInput(session, "op", value = 0.7, min = 0, max = 0.7)
							}
							else {
								updateNumericInput(session, "op", min = 0, max = 0.7)
							}
						}
						
						shinyjs::enable("edit1_radio")
						shinyjs::enable("edit2_radio")
						
						if (input$edit3_radio == "yes") {
							updateRadioButtons(session, "edit3_radio", selected = "no")
							shinyjs::delay(100, disable("edit3_radio"))
						}
						else {
							shinyjs::disable("edit3_radio")
						}
						
						shinyjs::enable("bin_radio")
						shinyjs::enable("dens_curve")
						shinyjs::enable("norm_dens_curve")
					}
					else {
						if (input$plot_type == "corplot") {
							if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
								if (isolate(o_norm_select$type) %in% c("boxplot", "barplot")) {
									if (input$concat1 == T) {
										updateCheckboxInput(session, "concat1", value = F)
										shinyjs::delay(100, disable("concat1"))
										shinyjs::delay(100, disable("var_x"))
									}
									else {
										shinyjs::disable("concat1")
										updateSelectizeInput(session, "var_x", choices = " ")
										shinyjs::delay(100, disable("var_x"))
									}
								}
								else {
									updateSelectizeInput(session, "var_x", choices = " ")
									shinyjs::delay(100, disable("var_x"))
								}
							}
							
							if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
								shinyjs::enable("f_radio")
							}
							
							if (isolate(o_norm_select$type) %in% c("histplot", "barplot", "calib_off", "valid_off")) {
								shinyjs::enable("var_y")
								
								if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
									shinyjs::enable("group")
								}
							}
							
							updateSelectizeInput(session, "var_y", choices = names(e_data$all), options = list(maxOptions = 9999, maxItems = 9999))
							o_norm_select$type <- "corplot"
							
							if (input$op_radio == "manual") {
								updateRadioButtons(session, "op_radio", selected = "auto")
								shinyjs::delay(100, disable("op_radio"))
							}
							else {
								shinyjs::disable("op_radio")
							}
						}
						else {
							if (isolate(o_cond$concat1) == 1) {
								if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
									if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
										shinyjs::enable("var_x")
									}
									
									updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
								}
							}
							else {
								if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "calib_on", "valid_on", "corplot")) {
									if (isolate(o_norm_select$type) != "boxplot") {
										shinyjs::enable("concat1")
										updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
									}
								}
								else {
									shinyjs::enable("concat1")
									
									if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "corplot")) {
										shinyjs::enable("var_x")
									}
									
									updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
								}
							}
							
							if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
								shinyjs::enable("f_radio")
							}
							
							if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off", "histplot")) {
								updateSelectizeInput(session, "var_y", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
								shinyjs::delay(100, disable("var_y"))
							}
							
							if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
								shinyjs::enable("group")
							}
							
							o_norm_select$type <- "barplot"
							shinyjs::enable("op_radio")
							
							if (input$op_radio != "auto") {
								if (input$op > 0.7) {
									updateNumericInput(session, "op", value = 0.7, min = 0, max = 0.7)
								}
								else {
									updateNumericInput(session, "op", min = 0, max = 0.7)
								}
							}
						}
						
						if (input$f_radio == "yes") {
							updateRadioButtons(session, "f_radio", selected = "no")
							shinyjs::delay(100, disable("f_radio"))
						}
						else {
							shinyjs::disable("f_radio")
						}
						
						shinyjs::enable("edit1_radio")
						
						if (input$edit2_radio == "yes") {
							updateRadioButtons(session, "edit2_radio", selected = "no")
							shinyjs::delay(100, disable("edit2_radio"))
						}
						else {
							shinyjs::disable("edit2_radio")
						}
						
						if (input$edit3_radio == "yes") {
							updateRadioButtons(session, "edit3_radio", selected = "no")
							shinyjs::delay(100, disable("edit3_radio"))
						}
						else {
							shinyjs::disable("edit3_radio")
						}
						
						if (input$bin_radio == "manual") {
							updateRadioButtons(session, "bin_radio", selected = "auto")
							shinyjs::delay(100, disable("bin_radio"))
						}
						else {
							shinyjs::disable("bin_radio")
						}
						
						if (input$dens_curve == T) {
							updateCheckboxInput(session, "dens_curve", value = F)
							shinyjs::delay(100, disable("dens_curve"))
						}
						else {
							shinyjs::disable("dens_curve")
						}
						
						if (input$norm_dens_curve == T) {
							updateCheckboxInput(session, "norm_dens_curve", value = F)
							shinyjs::delay(100, disable("norm_dens_curve"))
						}
						else {
							shinyjs::disable("norm_dens_curve")
						}
					}
					
					if (input$g_radio == "yes") {
						updateRadioButtons(session, "g_radio", selected = "no")
						shinyjs::delay(100, disable("g_radio"))
					}
					else {
						shinyjs::disable("g_radio")
					}
					
					if (input$box_mean_sd == T) {
						updateCheckboxInput(session, "box_mean_sd", value = F)
						shinyjs::delay(100, disable("box_mean_sd"))
					}
					else {
						shinyjs::disable("box_mean_sd")
					}
				}
			}
			
			if (!input$plot_type %in% c("histplot", "barplot", "corplot")) {
				shinyjs::enable("dec_num_radio")
			}
			else {
				if (input$dec_num_radio == "manual") {
					updateRadioButtons(session, "dec_num_radio", selected = "auto")
					shinyjs::delay(100, disable("dec_num_radio"))
				}
				else {
					shinyjs::disable("dec_num_radio")
				}
			}
			
			shinyjs::disable("op_button")
			shinyjs::disable("bw_button")
			shinyjs::disable("dec_num_button")
			shinyjs::disable("apply_button")
		}
	})
	
	# 2.7. Add events of dimension number radio button
	# ================================================
	
	observeEvent(input$dim_num, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
			o_cond$label <- rep(0, 4)
		
			if (isolate(o_click_button$display) == 1 & isolate(o_norm_select$type) == "plot") {
				if (!is.null(input$var_flag_1)) {
					updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
					shinyjs::delay(100, disable("var_flag_1"))
				}
				else {
					shinyjs::disable("var_flag_1")
				}
				
				if (input$var_flag_2 == T) {
					updateCheckboxInput(session, "var_flag_2", value = F)
					shinyjs::delay(100, disable("var_flag_2"))
				}
				else {
					shinyjs::disable("var_flag_2")
				}
			}
			
			if (input$ref_radio == "yes" | input$wres_radio == "yes" | isolate(o_cond$del)[2] == 1) {
				o_reset$model2 <- 1
			}
			
			click("reset1_button")
			
			if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data$all)) {
				e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat2.")]
			}
			
			if (input$dim_num == "3d") {
				if (input$webgl != "no") {
					updateRadioButtons(session, "webgl", selected = "no")
					shinyjs::delay(100, disable("webgl"))
				}
				else {
					shinyjs::disable("webgl")
				}
				
				if (input$model != "none") {
					updateRadioButtons(session, "model", selected = "none")
					
					if ("flag" %in% ls(e_data)) {
						if (substr(isolate(o_flag$name), nchar(isolate(o_flag$name)) - 9, nchar(isolate(o_flag$name)) - 4) == "withID") {
							shinyjs::enable("id")
							updateRadioButtons(session, "id", selected = "yes")
							shinyjs::delay(100, disable("id"))
						}
						else {
							if (input$id == "yes") {
								updateRadioButtons(session, "id", selected = "no")
								shinyjs::delay(100, disable("id"))
							}
							else {
								shinyjs::disable("id")
							}
						}
					}
					else {
						shinyjs::enable("id")
					}
					
					shinyjs::enable("f_radio")
					
					if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
						shinyjs::enable("var_x")
						shinyjs::enable("var_y")
						shinyjs::enable("g_radio")
						shinyjs::enable("group")
						shinyjs::enable("display_button")
					}
					
					if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
						updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
						updateRadioButtons(session, "f_radio", selected = "no")
					}
				}
				
				shinyjs::enable("var_z")
				updateSelectizeInput(session, "var_z", choices = names(e_data$all), selected = names(e_data$all)[1])
				shinyjs::enable("h_radio")
				shinyjs::enable("edit4_radio")
				
				if (input$lreg == T) {
					updateCheckboxInput(session, "lreg", value = F)
					shinyjs::delay(100, disable("lreg"))
				}
				else {
					shinyjs::disable("lreg")
				}
				
				if (input$conf_ellipsoid == T) {
					updateCheckboxInput(session, "conf_ellipsoid", value = F)
					shinyjs::delay(100, disable("conf_ellipsoid"))
				}
				else {
					shinyjs::disable("conf_ellipsoid")
				}
				
				shinyjs::enable("centroid")
				
				if (isolate(o_norm_select$type) != "plot") {
					o_norm_select$type <- "plot"
				}
			}
			else {
				if (input$plot_type == "plot") {
					shinyjs::enable("webgl")
					updateRadioButtons(session, "webgl", selected = "yes")
				}
				
				updateSelectizeInput(session, "var_z", choices = " ")
				shinyjs::delay(100, disable("var_z"))
				updateRadioButtons(session, "h_radio", selected = "no")
				shinyjs::delay(100, disable("h_radio"))
				
				if (input$edit4_radio == "yes") {
					updateRadioButtons(session, "edit4_radio", selected = "no")
					shinyjs::delay(100, disable("edit4_radio"))
				}
				else {
					shinyjs::disable("edit4_radio")
				}
				
				if (input$plot_type == "plot" & input$model != "calib") {
					shinyjs::enable("lreg")
					shinyjs::enable("conf_ellipsoid")
					shinyjs::enable("centroid")
				}
			}
			
			shinyjs::disable("op_button")
		}
	})
	
	# 2.8. Add events of model radio button 
	# =====================================
	
	observeEvent(input$model, {
		if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
			o_cond$label <- rep(0, 4)
			o_path$name3_prev <- NA
			
			if (isolate(o_click_button$display) == 1 & isolate(o_norm_select$type) == "plot") {
				if (!is.null(input$var_flag_1)) {
					updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
					shinyjs::delay(100, disable("var_flag_1"))
				}
				else {
					shinyjs::disable("var_flag_1")
				}
				
				if (input$var_flag_2 == T) {
					updateCheckboxInput(session, "var_flag_2", value = F)
					shinyjs::delay(100, disable("var_flag_2"))
				}
				else {
					shinyjs::disable("var_flag_2")
				}
			}
			
			if (input$model %in% c("calib", "valid")) {
				if (!isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
					js$backgroundCol("data_path3", "white")
					o_path$color3 <- "white"
					o_load_error$code[3] <- 0
					o_data_info$mtime[3] <- NA
					o_click_button$load3 <- 0
					o_reset$model1 <- 1					
					o_reset$model2 <- 1					
					click("reset1_button")
					
					if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data$all)) {
						e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat2.")]
					}
					
					if (input$id == "yes") {
						updateRadioButtons(session, "id", selected = "no")
						shinyjs::delay(100, disable("id"))
					}
					else {
						shinyjs::disable("id")
					}
					
					if (isolate(o_norm_select$type) %in% c("calib_on", "valid_on")) {
						updateSelectizeInput(session, "var_x", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
						rm(list = "m_param", envir = e_data)
					}
					else {
						updateSelectizeInput(session, "var_x", choices = " ")
						shinyjs::enable("data_path3")
						shinyjs::enable("browse3_button")
					}
					
					shinyjs::delay(100, disable("var_x"))
				}
				else {
					if (isolate(o_path$color3) != "white") {
						js$backgroundCol("data_path3", "white")
						o_path$color3 <- "white"
						o_load_error$code[3] <- 0
						o_data_info$mtime[3] <- NA
						o_click_button$load3 <- 0
					}
				}
				
				if (input$dim_num == "3d") {
					updateRadioButtons(session, "dim_num", selected = "2d")
					
					if (input$model == "calib") {
						if (input$centroid == T) {
							updateCheckboxInput(session, "centroid", value = F)
							shinyjs::delay(100, disable("centroid"))
						}
						else {
							shinyjs::disable("centroid")
						}
					}
				}
				else {
					if (input$model == "calib") {
						if (input$lreg == T) {
							updateCheckboxInput(session, "lreg", value = F)
							shinyjs::delay(100, disable("lreg"))
						}
						else {
							shinyjs::disable("lreg")
						}
						
						if (input$conf_ellipsoid == T) {
							updateCheckboxInput(session, "conf_ellipsoid", value = F)
							shinyjs::delay(100, disable("conf_ellipsoid"))
						}
						else {
							shinyjs::disable("conf_ellipsoid")
						}
						
						if (input$centroid == T) {
							updateCheckboxInput(session, "centroid", value = F)
							shinyjs::delay(100, disable("centroid"))
						}
						else {
							shinyjs::disable("centroid")
						}
					}
					else {
						shinyjs::enable("lreg")
						shinyjs::enable("conf_ellipsoid")
						shinyjs::enable("centroid")
					}
				}
				
				if (input$model == "calib") {
					o_norm_select$type <- "calib_off"
				}
				else {
					o_norm_select$type <- "valid_off"
				}
			}
			else {
				js$backgroundCol("data_path3", "transparent")
				o_path$color3 <- "transparent"
				o_path$name3 <- NA
				o_load_error$code[3] <- 0
				o_data_info$mtime[3] <- NA
				o_click_button$load3 <- 0
				o_click_button$browse3 <- 0
				
				if (input$data_path3 != "") {
					updateTextInput(session, "data_path3", value = "")
					shinyjs::delay(100, disable("data_path3"))
				}
				else {
					shinyjs::disable("data_path3")
				}
				
				shinyjs::disable("browse3_button")
				
				if (input$plot_type == "plot" & input$dim_num == "2d") {
					o_reset$model2 <- 1					
					click("reset1_button")
					
					if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data$all)) {
						e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat2.")]
					}
					
					if ("flag" %in% ls(e_data)) {
						if (substr(isolate(o_flag$name), nchar(isolate(o_flag$name)) - 9, nchar(isolate(o_flag$name)) - 4) == "withID") {
							shinyjs::enable("id")
							updateRadioButtons(session, "id", selected = "yes")
							shinyjs::delay(100, disable("id"))
						}
						else {
							if (input$id == "yes") {
								updateRadioButtons(session, "id", selected = "no")
								shinyjs::delay(100, disable("id"))
							}
							else {
								shinyjs::disable("id")
							}
						}
					}
					else {
						shinyjs::enable("id")
					}
					
					shinyjs::enable("f_radio")
					
					if (isolate(o_norm_select$type) %in% c("calib_off", "valid_off")) {
						shinyjs::enable("var_x")
						updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1])
						shinyjs::enable("var_y")
						updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = names(e_data$all)[1])
						shinyjs::enable("g_radio")
						shinyjs::enable("group")
						shinyjs::enable("display_button")
					}
					else {
						updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
						updateRadioButtons(session, "f_radio", selected = "no")
					}
					
					shinyjs::enable("lreg")
					shinyjs::enable("conf_ellipsoid")
					shinyjs::enable("centroid")
					o_norm_select$type <- "plot"
				}
			}
			
			shinyjs::disable("op_button")
		}
	})
	
	# -----------------------
	# Model parameter loading
	# -----------------------
	
	# 2.9. Add events of model parameter input/buttons 
	# ================================================
	
	# data_path3
	
	observeEvent(input$data_path3, {
		if (!is.na(isolate(o_path$name3))) {
			o_path$name3_prev <- isolate(o_path$name3)
		}
			
		if (!is.na(isolate(o_path$name3_prev))) {
			if (isolate(o_click_button$browse3) == 1 & input$data_path3 == "") {
				updateTextInput(session, "data_path3", value = isolate(o_path$name3))
			}
			else {
				o_path$name3 <- input$data_path3
			}
		}
		else {
			if (is.na(isolate(o_path$name3_prev)) & input$data_path3 != "") {
				o_path$name3 <- input$data_path3
			}
		}
		
		if (isolate(o_click_button$browse3) == 1) {
			o_click_button$browse3 <- 0
		}
		
		if (isolate(o_click_button$load3) == 1) {
			if (length(unique(c(isolate(o_path$name3_prev), isolate(o_path$name3)))) == 1 & !is.na(isolate(o_path$name3_prev))) {
				js$backgroundCol("data_path3", isolate(o_path$color3))
			}
			else {
				if (input$model %in% c("calib", "valid")) {
					js$backgroundCol("data_path3", "white")
					o_path$color3 <- "white"
				}
				else {
					js$backgroundCol("data_path3", "transparent")
					o_path$color3 <- "transparent"
				}
				
				o_load_error$code[3] <- 0
				o_data_info$mtime[3] <- NA
				o_click_button$load3 <- 0
				
				if ("m_param" %in% ls(e_data)) {
					rm(list = "m_param", envir = e_data)
				}
				
				if (input$model != "none") {
					o_reset$model1 <- 1
				}
				
				o_reset$model2 <- 1
				click("reset1_button")
				
				if (input$model == "calib") {
					o_norm_select$type <- "calib_off"
				}
				else {
					o_norm_select$type <- "valid_off"
				}
			}
		}
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
			if (isolate(o_path$color3) == "lightgreen") {
				n_time <- as.numeric(file.info(input$data_path3)$mtime)
				
				if (n_time == isolate(o_data_info$mtime)[3]) {
					b_cond <- F
				}
				else {
					b_cond <- T
				}
			}
			else {
				b_cond <- F 
			}
			
			if ((length(unique(c(isolate(o_path$name3_prev), isolate(o_path$name3)))) > 1 & !is.na(isolate(o_path$name3_prev))) | isolate(o_click_button$load3) == 0 | isolate(o_load_error$code)[3] == 1 | b_cond == T) {
				if (b_cond == T) {
					if ("m_param" %in% ls(e_data)) {
						rm(list = "m_param", envir = e_data)
					}
					
					if (input$model == "calib") {
						o_norm_select$type <- "calib_off"
					}
					else {
						o_norm_select$type <- "valid_off"
					}
					
					o_reset$model1 <- 1
					o_reset$model2 <- 1
					click("reset1_button")
				}
				
				o_click_button$load3 <- 1
				s_e_message <- NA
				s_w_message <- NA
				df_1 <- tryCatch({suppressWarnings(fread(input$data_path3, data.table = F))}, error = function(e) F) 
				
				if (!is.data.frame(df_1)) {
					s_e_message <- "Path/file doesn't exist"
				}
				else {
					if (dim(df_1)[1] == 0) {
						s_e_message <- "Data is empty"
					}
					else {
						v_name <- c("parameter", "value")
						v_pos_1 <- which(v_name %in% names(df_1))
						
						if (length(v_pos_1) < 2) {
							if (length(v_pos_1) == 1) {
								s_e_message <- paste0("The following variable is missing in model parameter data: ", v_name[-v_pos_1])
							}
							else {
								s_e_message <- "The following variables are missing in model parameter data: parameter, value"
							}
						}
						else {
							if (!is.numeric(df_1$value)) {
								s_e_message <- "The value variable is not numeric (decimal is a point)"
							}
							else {
								v_param <- as.vector(unique(df_1$parameter)) 
								v_pos_1 <- which(v_param == "sigma")
								
								if (length(v_pos_1) > 0) {
									v_num <- gsub("a|d|re|gr|[|]|:", "", v_param[-v_pos_1])
								}
								else {
									v_num <- gsub("a|d|re|gr|[|]|:", "", v_param)
								}
								
								v_num <- suppressWarnings(as.numeric(v_num))
								
								if (length(which(is.na(v_num))) == 0) {
									v_pos_2 <- grep("a", v_param)
									v_pos_3 <- grep("d", v_param)
									v_pos_4 <- grep("re", v_param)
									v_pos_5 <- grep("gr", v_param)
									
									if (length(v_pos_2) > 0) {
										if (length(which(v_pos_2 %in% v_pos_3)) == 0 & length(which(v_pos_2 %in% v_pos_5)) == 0 & length(which(v_pos_3 %in% v_pos_4)) == 0 & length(which(v_pos_4 %in% v_pos_5)) == 0) {
											v_pos_6 <- grep("[|]", v_param)
											v_pos_7 <- grep(":", v_param)
										
											if (length(v_pos_6) > 0) {
												v_name <- substr(v_param[v_pos_6], unlist(gregexpr("[|]", v_param[v_pos_6])) + 1, nchar(v_param[v_pos_6]))
												
												if (length(v_pos_7) > 0) {
													v_name <- c(v_name[-grep(":", v_name)], substr(v_name[grep(":", v_name)], 1, unlist(gregexpr(":", v_name[grep(":", v_name)])) - 1), substr(v_name[grep(":", v_name)], unlist(gregexpr(":", v_name[grep(":", v_name)])) + 1, nchar(v_name[grep(":", v_name)])))
												}
												
												v_name <- unique(v_name)
												
												if (length(which(v_name %in% names(df_1))) < length(v_name)) {
													s_e_message <- paste0("The following variable is missing in model parameter data: ", paste(v_name[which(!v_name %in% names(df_1))], collapse = ", "))
													b_cond <- F
												}
												else {
													b_cond <- T
												}
											}
											else {
												b_cond <- T
											}
											
											if (b_cond == T & length(v_pos_1) == 0 & input$model == "calib") {
												s_w_message <- "As the residual standard deviation (sigma) is missing in model parameters, then:<br/>(1) The standardized residuals are not calculated ;<br/>(2) Fields corresponding to the weighted residuals are disabled ;<br/>(3) The qqplot will be not displayed." 
											}
										}
										else {
											s_e_message <- "Invalid association between model parameters"
										}
									}
									else {
										s_e_message <- "Model with no coefficient (parameter beginning with a)" 
									}
								}
								else {
									s_e_message <- "Invalid model parameters"
								}
							}
						}
					}
				}
				
				if (is.na(s_e_message)) {
					if (!is.na(s_w_message)) {
						f_showNotification(s_w_message, duration = 15, type = "warning")
					}
					
					e_data$m_param <- df_1
					
					js$backgroundCol("data_path3", "lightgreen")
					o_path$color3 <- "lightgreen"
					o_load_error$code[3] <- 0
					o_data_info$mtime[3] <- as.numeric(file.info(input$data_path3)$mtime)
				
					shinyjs::enable("ref_radio")
					
					if (input$model == "calib") {
						if (is.na(s_w_message)) {
							shinyjs::enable("wres_radio")
						}
						else {
							updateRadioButtons(session, "wres_radio", selected = "no")
							shinyjs::delay(100, disable("wres_radio"))
						}
						
						o_norm_select$type <- "calib_on"
					}
					else {
						updateRadioButtons(session, "wres_radio", selected = "no")
						shinyjs::delay(100, disable("wres_radio"))
						o_norm_select$type <- "valid_on"
					}
					
					shinyjs::enable("var_x")
					updateSelectizeInput(session, "var_x", choices = names(e_data$all), options = list(maxOptions = 9999, maxItems = 9999))
					o_cond$del[2] <- 1
					shinyjs::enable("f_radio")
					updateRadioButtons(session, "f_radio", selected = "yes")
					shinyjs::delay(100, disable("f_radio"))
					shinyjs::enable("var_y")
					updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = names(e_data$all)[1], options = list(maxOptions = 9999, maxItems = 1))
					shinyjs::enable("g_radio")
					shinyjs::enable("group")
					shinyjs::enable("display_button")
				}
				else {
					js$backgroundCol("data_path3", "salmon")
					o_path$color3 <- "salmon"
					o_load_error$code[3] <- 1
					o_data_info$mtime[3] <- NA
					showNotification(s_e_message, duration = 15, type = "error")
				}
			}
		}
	})
	
	# ------------------
	# Variable selection
	# ------------------
	
	# 2.10. Add events on variables/functions radio buttons and checkbox 
	# ==================================================================
	
	# ID: enable/disable the corresponding field
	
	observeEvent(input$id, {
		if ("all" %in% ls(e_data)) {
			if (input$id == "yes") {
				shinyjs::enable("var_id")
				
				if ("flag" %in% ls(e_data)) {
					updateSelectizeInput(session, "var_id", choices = names(e_data$flag)[1])
					shinyjs::delay(100, disable("var_id"))
				}
				else {
					if (isolate(o_cond$concat1) == 0 & isolate(o_cond$concat2) == 0) {
						v_pos <- which(names(e_data$all) %in% c(".concat1.", ".concat2."))
					}
					else if (isolate(o_cond$concat1) == 0 & isolate(o_cond$concat2) == 1) {
						v_pos <- which(names(e_data$all) == ".concat1.")
					}
					else if (isolate(o_cond$concat1) == 1 & isolate(o_cond$concat2) == 0) {
						v_pos <- which(names(e_data$all) == ".concat2.")
					}
					else {
						v_pos <- c()
					}
					
					if (length(v_pos) > 0) {
						v_name <- names(e_data$all)[-v_pos]
					}
					else {
						v_name <- names(e_data$all)
					}
					
					updateSelectizeInput(session, "var_id", choices = v_name)
				}
			}
			else {
				updateSelectizeInput(session, "var_id", choices = " ")
				shinyjs::delay(100, disable("var_id"))
			}
		}
	})
	
	# random effects: enable/disable the corresponding field
	
	observeEvent(input$ref_radio, {
		if ("all" %in% ls(e_data)) {
			if (input$ref_radio == "yes") {
				o_cond$del[1] <- 1
				
				if (isolate(o_cond$update) == 1) {
					o_cond$update <- 0
					updateSelectizeInput(session, "ref", choices = names(e_data$all), selected = isolate(o_parameter$ref))
				}
				else {
					shinyjs::enable("ref")
					updateSelectizeInput(session, "ref", choices = names(e_data$all))
				}
			}
			else {
				o_cond$del[1] <- 0
				updateSelectizeInput(session, "ref", choices = " ")
				shinyjs::delay(100, disable("ref"))
			}
		}
	})
	
	# f(x): enable/disable the corresponding field
	
	observeEvent(input$f_radio, {
		if ("all" %in% ls(e_data)) {
			if (input$f_radio == "yes") {
				shinyjs::enable("f_text")
			}
			else {
				if (length(input$f_text) > 0) {
					updateTextInput(session, "f_text", value = character(0))
					shinyjs::delay(100, disable("f_text"))
				}
				else {
					shinyjs::disable("f_text")
				}
			}
		}
	})
	
	# g(y): enable/disable the corresponding field
	
	observeEvent(input$g_radio, {
		if ("all" %in% ls(e_data)) {
			if (input$g_radio == "yes") {
				shinyjs::enable("g_text")
			}
			else {
				if (length(input$g_text) > 0) {
					updateTextInput(session, "g_text", value = character(0))
					shinyjs::delay(100, disable("g_text"))
				}
				else {
					shinyjs::disable("g_text")
				}
			}
		}
	})
	
	# h(z): enable/disable the corresponding field
	
	observeEvent(input$h_radio, {
		if ("all" %in% ls(e_data)) {
			if (input$h_radio == "yes") {
				shinyjs::enable("h_text")
			}
			else {
				if (length(input$h_text) > 0) {
					updateTextInput(session, "h_text", value = character(0))
					shinyjs::delay(100, disable("h_text"))
				}
				else {
					shinyjs::disable("h_text")
				}
			}
		}
	})
	
	# weighted residuals: enable/disable variance function field
	
	observeEvent(input$wres_radio, {
		if ("all" %in% ls(e_data)) {
			if (input$wres_radio == "yes") {
				shinyjs::enable("wres_cbox")
				shinyjs::enable("wres_vfun")
				
				if (isolate(o_cond$update) == 1) {
					if (length(which(!is.na(isolate(o_parameter$wres_group)))) > 0) {
						updateCheckboxInput(session, "wres_cbox", value = T)
					}
					else {
						o_cond$update <- 0
					}
					
					updateTextInput(session, "wres_vfun", value = isolate(o_parameter$wres_vfun))
				}
			}
			else {
				if (input$wres_cbox == T) {
					updateCheckboxInput(session, "wres_cbox", value = F)
					shinyjs::delay(100, disable("wres_cbox"))
				}
				else {
					shinyjs::disable("wres_cbox")
				}
				
				if (nchar(input$wres_vfun) > 0) {
					updateTextInput(session, "wres_vfun", value = character(0))
					shinyjs::delay(100, disable("wres_vfun"))
				}
				else {
					shinyjs::disable("wres_vfun")
				}
			}
		}
	})
	
	# weighted residuals: add or not group variables on residual variance function
	
	observeEvent(input$wres_cbox, {
		if ("all" %in% ls(e_data)) {
			if (input$wres_cbox == T) {
				shinyjs::enable("wres_group")
				o_cond$del[4] <- 1
				
				if (isolate(o_cond$update) == 1) {
					o_cond$update <- 0
					updateSelectizeInput(session, "wres_group", choices = names(e_data$all), selected = isolate(o_parameter$wres_group))
				}
				else {
					updateSelectizeInput(session, "wres_group", choices = names(e_data$all))
				}
			}
			else {
				updateSelectizeInput(session, "wres_group", choices = " ")
				shinyjs::delay(100, disable("wres_group"))
				o_cond$del[4] <- 0
			}
		}
	})
	
	# Group: enable/disable the corresponding field
	
	observeEvent(input$group, {
		if ("all" %in% ls(e_data)) {
			if (input$group == "yes") {
				shinyjs::enable("concat2")
				shinyjs::enable("var_group")
				
				if (isolate(o_cond$concat1) == 0) {
					v_pos <- which(names(e_data$all) %in% c(".concat1.", ".concat2."))
				}
				else {
					v_pos <- which(names(e_data$all) == ".concat2.")
				}
				
				if (length(v_pos) > 0) {
					v_name <- names(e_data$all)[-v_pos]
				}
				else {
					v_name <- names(e_data$all)
				}
				
				if (isolate(o_cond$update) == 1) {
					if (isolate(o_parameter$concat2) == T) {
						updateCheckboxInput(session, "concat2", value = T)
					}
					else {
						updateSelectizeInput(session, "var_group", choices = v_name, selected = isolate(o_parameter$group))
						o_cond$update <- 0
					}
				}
				else {
					updateSelectizeInput(session, "var_group", choices = v_name)
				}
			}
			else {
				if (input$concat2 == T) {
					updateCheckboxInput(session, "concat2", value = F)
					shinyjs::delay(100, disable("concat2"))
				}
				else {
					shinyjs::disable("concat2")
					updateSelectizeInput(session, "var_group", choices = " ")
					shinyjs::delay(100, disable("var_group"))
				}
			}
		}
	})
	
	# 2.11. Add events of del buttons
	# ===============================
	
	observeEvent(input$del1_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "ref", choices = names(e_data$all), selected = NULL)
		}
	})
	
	observeEvent(input$del2_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_x", choices = names(e_data$all), selected = NULL)
		}
	})
	
	observeEvent(input$del3_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_y", choices = names(e_data$all), selected = NULL)
		}
	})
	
	observeEvent(input$del4_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "wres_group", choices = names(e_data$all), selected = NULL)
		}
	})
	
	observeEvent(input$del5_button, {
		if ("all" %in% ls(e_data)) {
			updateSelectizeInput(session, "var_group", choices = names(e_data$all), selected = NULL)
		}
	})
	
	# 2.12. Add events of expand buttons
	# ==================================
	
	observeEvent(input$expand1_button, {
		if ("all" %in% ls(e_data)) {
			showModal(modalDialog(
				title = "f(x):",
				easyClose = T,
				size = "l",
				textAreaInput("f_text_modal", NULL, input$f_text, width = "855px", height = validateCssUnit("auto"), resize = "none"),
				footer = tagList(actionButton("ok_button_exp1", "Ok"), modalButton("Close"))
			))
		}
	})
	
	observeEvent(input$expand2_button, {
		if ("all" %in% ls(e_data)) {
			showModal(modalDialog(
				title = "g(y):",
				easyClose = T,
				size = "l",
				textAreaInput("g_text_modal", NULL, input$g_text, width = "855px", height = validateCssUnit("auto"), resize = "none"),
				footer = tagList(actionButton("ok_button_exp2", "Ok"), modalButton("Close"))
			))
		}
	})
	
	observeEvent(input$expand3_button, {
		if ("all" %in% ls(e_data)) {
			showModal(modalDialog(
				title = "h(z):",
				easyClose = T,
				size = "l",
				textAreaInput("h_text_modal", NULL, input$h_text, width = "855px", height = validateCssUnit("auto"), resize = "none"),
				footer = tagList(actionButton("ok_button_exp3", "Ok"), modalButton("Close"))
			))
		}
	})
	
	observeEvent(input$expand4_button, {
		if ("all" %in% ls(e_data)) {
			showModal(modalDialog(
				title = "variance function:",
				easyClose = T,
				size = "l",
				textAreaInput("wres_vfun_modal", NULL, input$wres_vfun, width = "855px", height = validateCssUnit("auto"), resize = "none"),
				footer = tagList(actionButton("ok_button_exp4", "Ok"), modalButton("Close"))
			))
		}
	})
	
	# 2.13. Add events of ok buttons (modal dialog: expand button)
	# ==============================
	
	observeEvent(input$ok_button_exp1, {
		if ("all" %in% ls(e_data)) {
			updateTextInput(session, "f_text", value = input$f_text_modal)
			removeModal()
		}
	})
	
	observeEvent(input$ok_button_exp2, {
		if ("all" %in% ls(e_data)) {
			updateTextInput(session, "g_text", value = input$g_text_modal)
			removeModal()
		}
	})
	
	observeEvent(input$ok_button_exp3, {
		if ("all" %in% ls(e_data)) {
			updateTextInput(session, "h_text", value = input$h_text_modal)
			removeModal()
		}
	})
	
	observeEvent(input$ok_button_exp4, {
		if ("all" %in% ls(e_data)) {
			updateTextInput(session, "wres_vfun", value = input$wres_vfun_modal)
			removeModal()
		}
	})
	
	# 2.14. Add events on concatenation checkbox for X/Group variables
	# ================================================================
	
	# concat1
	
	observeEvent(input$concat1, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_cond$concat2) == 0) {
				v_pos <- which(names(e_data$all) %in% c(".concat1.", ".concat2."))
			}
			else {
				v_pos <- which(names(e_data$all) == ".concat1.")
			}
			
			if (length(v_pos) > 0) {
				v_name <- names(e_data$all)[-v_pos]
			}
			else {
				v_name <- names(e_data$all)
			}
			
			if (input$concat1 == T) {
				o_cond$del[2] <- 1
				
				if (isolate(o_cond$update) == 1) {
					updateSelectizeInput(session, "var_x", choices = v_name, selected = isolate(o_parameter$concat1_group), options = list(maxOptions = 9999, maxItems = 9999))
					o_cond$update <- 0
				}
				else {
					updateSelectizeInput(session, "var_x", choices = v_name, options = list(maxOptions = 9999, maxItems = 9999))
				}
			}
			else {
				o_cond$del[2] <- 0
				
				if (isolate(o_cond$update) == 1) {
					updateSelectizeInput(session, "var_x", choices = v_name, selected = isolate(o_parameter$x), options = list(maxOptions = 9999, maxItems = 1))
					o_cond$update <- 0
				}
				else {
					if (input$plot_type != "corplot") {
						updateSelectizeInput(session, "var_x", choices = v_name, selected = v_name[1], options = list(maxOptions = 9999, maxItems = 1))
					}
					else {
						updateSelectizeInput(session, "var_x", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
					}
				}
			}
		}
	})
	
	# concat2
	
	observeEvent(input$concat2, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_cond$concat1) == 0) {
				v_pos <- which(names(e_data$all) %in% c(".concat1.", ".concat2."))
			}
			else {
				v_pos <- which(names(e_data$all) == ".concat2.")
			}
			
			if (length(v_pos) > 0) {
				v_name <- names(e_data$all)[-v_pos]
			}
			else {
				v_name <- names(e_data$all)
			}
			
			if (input$concat2 == T) {
				o_cond$del[5] <- 1
				
				if (isolate(o_cond$update) == 1) {
					updateSelectizeInput(session, "var_group", choices = v_name, selected = isolate(o_parameter$concat2_group), options = list(maxOptions = 9999, maxItems = 9999))
					o_cond$update <- 0
				}
				else {
					updateSelectizeInput(session, "var_group", choices = v_name, options = list(maxOptions = 9999, maxItems = 9999))
				}
			}
			else {
				o_cond$del[5] <- 0
				
				if (input$group == "no") {
					updateSelectizeInput(session, "var_group", choices = " ", options = list(maxOptions = 9999, maxItems = 1))
					shinyjs::delay(100, disable("var_group"))
				}
				else {
					if (isolate(o_cond$update) == 1) {
						updateSelectizeInput(session, "var_group", choices = v_name, selected = isolate(o_parameter$group), options = list(maxOptions = 9999, maxItems = 1))
						o_cond$update <- 0
					}
					else {
						updateSelectizeInput(session, "var_group", choices = v_name, selected = v_name[1], options = list(maxOptions = 9999, maxItems = 1))
					}
				}
			}
		}
	})
	
	# 2.15. Add events of display button
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
			
			shinyjs::delay(i_ms, 
				if (input$data_type == "normal") {
					# A. Type: Normal
					# ---------------
					
					if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) { # executed with the display button or the Flag tab save button
						o_cond$qc2 <- 0
						o_cond$legend <- 0
						
						o_plot$data <- data.frame()
						o_plot$model <- NA
						o_plot$color <- NA
						o_plot$data_qc2 <- NA
						o_plot$elt <- NA
						
						if (isolate(o_cond$display) == 0) { # executed with the display button
							o_w_message$lreg <- 0
							o_w_message$conf_ellipsoid <- 0
							o_w_message$dens_curve <- 0
							o_w_message$norm_dens_curve <- 0
							
							o_legend_group$lreg <- c()
							o_legend_group$conf_ellipsoid <- c()
							o_legend_group$dens_curve <- c()
							o_legend_group$norm_dens_curve <- c()
							
							if (length(e_current_flag) > 0) {
								rm(list = ls(e_current_flag), envir = e_current_flag)
								shinyjs::disable("clear1_button")
								shinyjs::disable("clear2_button")
								shinyjs::disable("save_button")
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							if (input$plot_type == "plot") {
								if (input$dim_num == "2d") {
									if (input$model == "none") {
										v_cond <- c(input$id == "yes" & input$var_id == "", input$var_x == "", input$f_radio == "yes" & (length(which(input$f_text == "")) == 1 | length(input$f_text) == 0), length(which(input$var_y == "")) == 1 | is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group))))
										names(v_cond) <- c("ID", "X", "f(x)", "Y", "g(y)", "Group")
									}
									else if (input$model == "calib") {
										v_cond <- c(input$ref_radio == "yes" & is.null(input$ref), input$wres_radio == "yes" & (length(which(input$wres_vfun == "")) == 1 | length(input$wres_vfun) == 0), input$wres_radio == "yes" & input$wres_cbox == T & is.null(input$wres_group), is.null(input$var_x), length(which(input$f_text == "")) == 1 | length(input$f_text) == 0, length(which(input$var_y == "")) == 1 | is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
										names(v_cond) <- c("Random", "Weighted residuals - variance function", "Weighted residuals - group", "X", "f(x)", "Y", "g(y)", "Group")
									}
									else {
										v_cond <- c(input$ref_radio == "yes" & is.null(input$ref), is.null(input$var_x), length(which(input$f_text == "")) == 1 | length(input$f_text) == 0, length(which(input$var_y == "")) == 1 | is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
										names(v_cond) <- c("Random", "X", "f(x)", "Y", "g(y)", "Group")
									}
								}
								else {
									v_cond <- c(input$id == "yes" & input$var_id == "", input$var_x == "", input$f_radio == "yes" & (length(which(input$f_text == "")) == 1 | length(input$f_text) == 0), length(which(input$var_y == "")) == 1 | is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0), input$var_z == "", input$h_radio == "yes" & (length(which(input$h_text == "")) == 1 | length(input$h_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
									names(v_cond) <- c("ID", "X", "f(x)", "Y", "g(y)", "Z", "h(z)", "Group")
								}
							}
							else if (input$plot_type == "boxplot") {
								v_cond <- c((input$concat1 == F & (length(which(input$var_x == "")) == 1 | is.null(input$var_x))) | (input$concat1 == T & is.null(input$var_x)), length(which(input$var_y == "")) == 1 | is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
								names(v_cond) <- c("X", "Y", "g(y)", "Group")
							}
							else if (input$plot_type == "histplot") {
								v_cond <- c(input$var_x == "", input$f_radio == "yes" & (length(which(input$f_text == "")) == 1 | length(input$f_text) == 0), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
								names(v_cond) <- c("X", "f(x)", "Group")
							}
							else if (input$plot_type == "barplot") {
								v_cond <- c((input$concat1 == F & (length(which(input$var_x == "")) == 1 | is.null(input$var_x))) | (input$concat1 == T & is.null(input$var_x)), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
								names(v_cond) <- c("X", "Group")
							}
							else {
								v_cond <- c(is.null(input$var_y), input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group)))) 
								names(v_cond) <- c("Y", "Group")
							}
							
							v_pos <- which(v_cond == T) 
							
							if (length(v_pos) == 0) {
								v_var_x <- NULL
								v_var_group <- NULL
								
								# Parameter value assignment: (o_parameter reactive value)
								# ---------------------------
								
								o_parameter$plot_type <- input$plot_type
								o_parameter$dim_num <- NA
								o_parameter$model <- NA
								o_parameter$concat1 <- input$concat1
								
								if (input$concat1 == T) {
									o_parameter$concat1_group <- input$var_x  
								}
								
								o_parameter$concat2 <- input$concat2
								
								if (input$concat2 == T) {
									o_parameter$concat2_group <- input$var_group  
								}
								
								if (input$plot_type != "corplot") {
									if (input$plot_type != "barplot") {
										if (input$plot_type  == "histplot") {
											shinyjs::enable("bw_button")
											
											if (input$f_radio == "yes") {
												o_parameter$f <- input$f_text
											}
											else {
												o_parameter$f <- NA
											}
										}
										else {
											if (input$plot_type == "plot") {
												o_parameter$dim_num <- input$dim_num
												o_parameter$model <- input$model
												
												if (input$id == "yes") {
													o_parameter$id <- input$var_id
												}
												else {
													o_parameter$id <- NA
												}
												
												if (input$f_radio == "yes") {
													o_parameter$f <- input$f_text
												}
												else {
													o_parameter$f <- NA
												}
												
												if (input$dim_num == "3d") {
													o_parameter$z <- input$var_z
													
													if (input$h_radio == "yes") {
														o_parameter$h <- input$h_text
													}
													else {
														o_parameter$h <- NA
													}
												}
												else {
													if (input$model %in% c("calib", "valid")) {
														if (input$ref_radio == "yes") {
															o_parameter$ref <- input$ref
														}
														else {
															o_parameter$ref <- NA
														}
														
														if (input$model == "calib") {
															if (input$wres_radio == "yes") {
																o_parameter$wres_vfun <- input$wres_vfun
																
																if (input$wres_cbox == T) {
																	o_parameter$wres_group <- input$wres_group
																}
																else {
																	o_parameter$wres_group <- NA
																}
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
											
											if (input$g_radio == "yes") {
												o_parameter$g <- input$g_text
											}
											else {
												o_parameter$g <- NA
											}
											
											o_parameter$y <- input$var_y
										}
									}
									
									if (input$concat1 == T) {
										eval(parse(text = paste0("v_row <- unique(c(", paste(paste0("which(is.na(as.vector(e_data$all$", input$var_x, ")))"), collapse = ", "), "))")))
										eval(parse(text = paste0("e_data$all$.concat1. <- as.vector(paste(", paste(paste0("as.vector(e_data$all$", input$var_x, ")"), collapse = ", "), ", sep = \" \"))")))
										e_data$all[v_row, ".concat1."] <- NA
										o_parameter$x <- ".concat1."
										v_var_x <- input$var_x
									}
									else {
										o_parameter$x <- input$var_x
									}
								}
								else {
									o_parameter$y <- input$var_y
								}
								
								if (input$group == "yes") {
									if (input$concat2 == T) {
										eval(parse(text = paste0("v_row <- unique(c(", paste(paste0("which(is.na(as.vector(e_data$all$", input$var_group, ")))"), collapse = ", "), "))")))
										eval(parse(text = paste0("e_data$all$.concat2. <- as.vector(paste(", paste(paste0("as.vector(e_data$all$", input$var_group, ")"), collapse = ", "), ", sep = \" \"))")))
										e_data$all[v_row, ".concat2."] <- NA
										o_parameter$group <- ".concat2."
										v_var_group <- input$var_group
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
								
								s_e_message <- f_check_variables("normal", e_data$all, o_parameter, v_var_x, v_var_group)
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {
							s_e_message <- character(0)
						}
						
						if (length(s_e_message) > 0) { # error message returned by checking processes
							f_showNotification(s_e_message, duration = 15, type = "error")
						}
						else { # no error
							# Data preparation: (process 1)
                            # -----------------
							
							eval(parse(text = paste0("l_results <- f_prepare_data(\"normal\", 1, e_data$all, NULL, NULL, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", isolate(o_flag$name), isolate(o_cond$flag_msg), NULL, NULL, NULL, NULL, o_parameter, NULL)")))
							
							if (length(l_results[[4]]) > 0) { # error (all values with a qc = 2)
								f_showNotification(l_results[[4]], duration = 15, type = "error")
							}
							else { # no error
								df_all <- l_results[[1]]
								
								# if (sum(dim(l_results[[2]])) > 0) {
								if (dim(l_results[[2]])[1] > 0) {
									o_plot$data_qc2 <- l_results[[2]]
								}
								
								o_cond$qc2 <- l_results[[3]][1]
								o_cond$flag_msg <- l_results[[3]][2]
								s_w_message <- l_results[[5]] 
								rm(list = "l_results")
								
								if (isolate(o_parameter$plot_type) != "corplot") { # corplot not concerned
									# Graph color palette assignment: (o_plot reactive value)
									# -------------------------------
									
									o_plot$color <- f_create_col_palette("normal", df_all, o_parameter)
									
									if (isolate(o_parameter$model) %in% c("calib", "valid")) { # calibration/validation model added
										# Checking process: (check model)
										# -----------------
										
										l_results <- f_check_model(df_all, e_data$m_param, o_parameter)
										s_e_message <- l_results[[4]]
									}
									else { # no model added
										if (!is.na(isolate(o_parameter$f)) | !is.na(isolate(o_parameter$g)) | !is.na(isolate(o_parameter$h))) { # (f, g, h) function(s) added
											# Checking process: (check transformed variables)
											# -----------------
											
											l_results <- f_check_trsf_variables("normal", df_all, o_parameter)
											s_e_message <- l_results[[2]]
										}
										else { # no function added
											l_results <- list()
											s_e_message <- NULL
										}
									}
									
									if (length(s_e_message) > 0) { # error with model/transformed variables
										f_showNotification(s_e_message, duration = 15, type = "error")
									}
									else { # no error
										if (length(l_results) > 0) {
											if (isolate(o_parameter$model) %in% c("calib", "valid")) {
												# Creating model data: (fitted values, residual variance)
												# --------------------
												
												o_plot$model <- f_create_model_data(l_results[[1]], l_results[[3]])
												l_fun_val <- list("f" = NULL, "g" = l_results[[2]], "h" = NULL)
											}
											else {
												l_fun_val <- l_results[[1]] 
											}
										}
										else {
											l_fun_val <- list("f" = NULL, "g" = NULL, "h" = NULL)
										}
										
										# Data preparation: (process 2)
										# -----------------
										
										o_plot$data <- f_prepare_data(s_data_type = "normal", i_proc_num = 2, df_all = df_all, o_parameter = o_parameter, l_fun_val = l_fun_val)
										rm(list = c("l_results", "l_fun_val"))
										
										v_e_message <- c()
										
										if (isolate(o_cond$display) == 0) {	# executed with the display button		
											o_zoom$coord <- NULL
											l_axis_layout <- NULL
											
											# edit graph labels
											
											if (isolate(o_parameter$autotitle) == F) {
												o_parameter$title <- input$edit1
												o_cond$label[1] <- 1
											}
											
											if (isolate(o_parameter$autoxlab) == F) {
												o_parameter$xlab <- input$edit2
												o_cond$label[2] <- 1
											}
											else {
												if (input$f_radio == "yes") {
													o_parameter$xlab <- "f(x)"
												}
												else {
													o_parameter$xlab <- "x"
												}
											}
											
											if (isolate(o_parameter$autoylab) == F) {
												o_parameter$ylab <- input$edit3
												o_cond$label[3] <- 1
											}
											else {
												if (input$g_radio == "yes") {
													o_parameter$ylab <- "g(y)"
												}
												else {
													o_parameter$ylab <- "y"
												}
											}
											
											if (isolate(o_parameter$autozlab) == F) {
												o_parameter$zlab <- input$edit4
												o_cond$label[4] <- 1
											}
											else {
												if (input$h_radio == "yes") {
													o_parameter$zlab <- "h(z)"
												}
												else {
													o_parameter$zlab <- "z"
												}
											}
											
											# Checking process: (check graphic options)
											# -----------------
											
											v_pos <- which(c(!isolate(o_parameter$autoop), !isolate(o_parameter$autodec_num), !isolate(o_parameter$autobin)))
											
											if (length(v_pos) > 0) {
												l_opt_name <- list(input$op, input$dec_num, tryCatch({suppressWarnings(eval(parse(text = input$bw)))}, error = function(e) FALSE))[v_pos]
												names(l_opt_name) <- c("op", "dec_num", "bw")[v_pos]
												v_e_message <- f_check_graphic_opt(l_opt_name, o_plot, o_parameter)
												
												if (length(v_e_message) == 0) {
													eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))
												}
												
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
											
											l_results <- f_build_legend_items("normal", 1, isolate(o_plot$data), o_parameter, o_cond, o_legend_group)
											o_legend_group <- l_results[[1]]
											o_click_legend$item <- l_results[[2]]
											rm(list = "l_results")
											
											# Create element data
											# -------------------
											
											if (isolate(o_parameter$plot_type) == "plot") {
												if (isolate(o_parameter$model) == "none") {
													o_plot$elt <- f_create_element_data(s_data_type = "normal", df_all = isolate(o_plot$data), o_parameter = o_parameter, o_cond = o_cond)
												}
											}
										}
									}
								}
								else { # corplot
									if (isolate(o_cond$display) == 0) { # executed with the display button
										o_zoom$coord <- NULL
									}
									
									# edit graph labels
									
									if (isolate(o_parameter$autotitle) == F) {
										o_parameter$title <- input$edit1
										o_cond$label[1] <- 1
									}
									
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
							
							l_results <- f_build_legend_items("normal", 2, isolate(o_plot$data), o_parameter, o_cond, o_legend_group, NULL, isolate(o_click_legend$item), l_traces)
							o_legend_group <- l_results[[1]]
							o_click_legend$item <- l_results[[2]]
							rm(list = "l_results")
							
							o_cond$stat <- 0
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
										updateSelectInput(session, "select_graph", choices = v_group, selected = v_group[1])
									}
									else {
										if (isolate(o_parameter$select_graph) != v_group[1]) {
											o_cond$select_graph1 <- 0
											o_parameter$select_graph <- v_group[1]
											updateSelectInput(session, "select_graph", choices = v_group, selected = v_group[1])
										}
										else {
											o_cond$select_graph1 <- 1
										}
									}
									
									shinyjs::show(id = "select_graph")
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
											updateSelectInput(session, "select_graph", choices = v_group, selected = v_group[1])
											shinyjs::show(id = "select_graph")
										}
										else {
											if (length(isolate(o_parameter$corplot_group)) != length(v_group)) {
												o_parameter$corplot_group <- v_group
												o_cond$select_graph1 <- 0
												o_parameter$select_graph <- v_group[1]
												updateSelectInput(session, "select_graph", choices = v_group, selected = v_group[1])
											}
											else {
												if (length(which(!isolate(o_parameter$corplot_group) %in% v_group)) > 0) {
													o_parameter$corplot_group <- v_group
													o_cond$select_graph1 <- 0
													o_parameter$select_graph <- v_group[1]
													updateSelectInput(session, "select_graph", choices = v_group, selected = v_group[1])
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
											updateSelectInput(session, "select_graph", choices = " ")
											shinyjs::hide(id = "select_graph")
										}
									}
								}
							}
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({
							v_dimension <- input$dimension
							df_click_legend <- o_click_legend$item
							
							# build graph:
							
							ply_1 <- f_build_graph("normal", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info)
							
							# add flags:
							
							v_cond <- c(rep(0, 2), isolate(o_cond$qc2))
							
							ply_1 <- f_add_flag("normal", ply_1, v_cond, df_click_legend, o_plot, o_parameter, NULL, e_current_flag)
							
							# add statistics:
							
							v_stat_method <- c("lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve")[which(c(isolate(o_parameter$lreg), isolate(o_parameter$conf_ellipsoid), isolate(o_parameter$centroid), isolate(o_parameter$dens_curve), isolate(o_parameter$norm_dens_curve)))]
							
							if (length(v_stat_method) > 0) {
								for (i in v_stat_method) {
									eval(parse(text = paste0("v_group <- isolate(o_legend_group$", i, ")")))
									
									if (i == "centroid") {
										ply_1 <- f_add_centroid(ply_1, df_click_legend, o_plot, o_parameter)
									}
									else {
										eval(parse(text = paste0("l_results <- f_add_", i, "(ply_1, v_group, df_click_legend, o_plot, o_parameter, ", ifelse(i == "lreg", "o_lreg_info, ", ""), "o_w_message$", i, ")")))
										
										if (length(l_results[[3]]) > 0) {
											f_showNotification(l_results[[3]], duration = 15, type = "error")
											eval(parse(text = paste0("o_cond$", i, " <- 1")))
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
						
						if ((!is.na(isolate(o_parameter$dim_num)) & isolate(o_parameter$dim_num) == "2d") | isolate(o_parameter$plot_type) == "histplot") {
							shinyjs::show(id = "sh_reset")
						}
					}
				}
				else if (input$data_type == "temporal") {
					# B. Type : Temporal
					# ------------------
					
					if (isolate(o_cond$display) == 0 | isolate(o_cond$save2) == 1) {
						o_cond$flag <- 0
						o_cond$qc1 <- 0
						o_cond$qc2 <- 0

						o_plot$data <- data.frame()
						o_plot$y_coord <- NULL
						o_plot$color <- NA
						o_plot$add_pt <- T
						o_plot$pt_pos <- NA
						o_plot$var_pt <- NA
						o_plot$var_pt_color <- NA
						o_plot$data_qc1 <- NA
						o_plot$data_qc2 <- NA
						o_plot$var_qc1 <- NA
						o_plot$var_qc2 <- NA
						o_plot$leg_name_qc <- NA
						o_plot$elt <- NA
						o_plot$elt_pt_pos <- NA
						
						if ("data" %in% ls(e_previous_flag)) {
							rm("data", envir = e_previous_flag)
						}
						
						s_w_message <- character(0)
						
						if (isolate(o_cond$display) == 0) { # executed with the display button
							if (length(e_current_flag) > 0) {
								o_click_graph$prev_date <- NULL
								o_click_graph$prev_var <- NULL
								rm(list = ls(e_current_flag), envir = e_current_flag)
								shinyjs::disable("clear1_button")
								shinyjs::disable("clear2_button")
								shinyjs::disable("save_button")
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							v_cond <- c(input$var_x == "", is.null(input$var_y), input$g_radio == "yes" & (length(which(input$g_text == "")) == 1 | length(input$g_text) == 0)) 
							names(v_cond) <- c("X", "Y", "g(y)")
							v_pos <- which(v_cond == T)
							
							if (length(v_pos) == 0) {
								# Parameter value assignment: (o_parameter reactive value)
								# ---------------------------
								
								o_parameter$x <- input$var_x
								o_parameter$date_format <- input$date_format
								o_parameter$y <- input$var_y
								
								if (input$g_radio == "yes") {
									o_parameter$g <- input$g_text
								}
								else {
									o_parameter$g <- NA
								}
							
								# Checking process: (check selected variables)
								# -----------------
								
								l_results <- f_check_variables("temporal", e_data$all, o_parameter)
								
								s_e_message <- l_results[[1]]
								s_w_message <- l_results[[2]] 
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {
							s_e_message <- NULL
						}
						
						if (length(s_e_message) > 0) { # error message returned by the checking process
							f_showNotification(s_e_message, duration = 15, type = "error")
							
							if (length(s_w_message) > 0) {
								f_showNotification(s_w_message, duration = 15, type = "warning")
							}
						}
						else { # no error
							# Graph color palette assignment: (o_plot reactive value)
							# -------------------------------
							
							o_plot$color <- f_create_col_palette("temporal", data.frame(), o_parameter)
							
							v_e_message <- c()
							
							if (isolate(o_cond$display) == 0) {
								o_zoom$coord <- NULL
								l_axis_layout <- NULL
								
								# edit graph labels
								
								if (isolate(o_parameter$autotitle) == F) {
									o_parameter$title <- input$edit1
									o_cond$label[1] <- 1
								}
								
								if (isolate(o_parameter$autoylab) == F) {
									o_parameter$ylab <- input$edit3
									o_cond$label[3] <- 1
								}
								else {
									if (input$g_radio == "yes") {
										o_parameter$ylab <- "g(y)"
									}
									else {
										o_parameter$ylab <- "y"
									}
								}
								
								# Checking process: (check graphic options)
								# -----------------
								
								v_pos <- which(c(!isolate(o_parameter$autoop), !isolate(o_parameter$autodec_num)))
								
								if (length(v_pos) > 0) {
									l_opt_name <- list(input$op, input$dec_num)[v_pos]
									names(l_opt_name) <- c("op", "dec_num")[v_pos]
									v_e_message <- f_check_graphic_opt(l_opt_name, NULL, o_parameter)
									
									if (length(v_e_message) == 0) {
										eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))
									}
									
									rm(list = "l_opt_name")
								}
							}
							
							if (length(v_e_message) > 0) { # error with graphic options
								f_showNotification(paste(v_e_message, collapse = "<br/>"), duration = 15, type = "error")
								
								if (length(s_w_message) > 0) {
									f_showNotification(s_w_message, duration = 15, type = "warning")
								}
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
									
									if (length(s_w_message) > 0) {
										f_showNotification(s_w_message, duration = 15, type = "warning")
									}
								}
								else { # no error
									v_w_message <- c()
									
									if (length(s_w_message) > 0) {
										v_w_message <- s_w_message
									}
									
									df_all <- e_data$all[, c(isolate(o_parameter$x), isolate(o_parameter$y))]
									
									if (isolate(o_cond$display) == 0) {
										df_all[, isolate(o_parameter$x)] <- l_results[[3]]
										rm(list = "l_results")
									}
									else {
										df_all[, isolate(o_parameter$x)] <- f_create_date_variable(df_all, isolate(o_parameter$x), isolate(o_parameter$date_format))
									}
									
									# Data preparation: (process 1)
									# -----------------
									
									eval(parse(text = paste0("l_results <- f_prepare_data(\"temporal\", 1, df_all, NULL, NULL, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", NULL, 0, ", ifelse("data" %in% ls(e_previous_flag), "e_previous_flag$data", "NULL"), ", NULL, NULL, NULL, o_parameter)")))
									
									if (length(l_results[[9]]) > 0) { # error (all values with a qc = 2)
										f_showNotification(l_results[[9]], duration = 15, type = "error")
										
										if (length(s_w_message) > 0) {
											f_showNotification(s_w_message, duration = 15, type = "warning")
										}
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
											
											if (length(s_w_message) > 0) {
												f_showNotification(s_w_message, duration = 15, type = "warning")
											}
										}
										else { # no error
											if (length(v_w_message) > 0) {
												f_showNotification(paste(v_w_message, collapse = "<br/>"), duration = 15, type = "warning")
											}
											
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
												o_plot$var_pt_color <- isolate(o_plot$color)[v_pos]
												o_plot$elt_pt_pos <- c(length(isolate(o_parameter$y)) + 1, length(isolate(o_parameter$y)) + length(isolate(o_plot$var_pt)))
												
												if (input$mode == "line") {
													o_plot$add_pt <- T
												}
												else {
													o_plot$add_pt <- F
												}
											}
											else {
												o_plot$add_pt <- F
											}
											
											# Build legend items (process 1) 
											# ------------------
											
											o_click_legend$item <- f_build_legend_items("temporal", 1,  NULL, o_parameter, o_cond, NULL, o_plot)
											
											# Create element data (process 1)
											# -------------------
											
											o_plot$elt <- f_create_element_data("temporal", 1, NULL, o_parameter, o_cond, o_plot)
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
										
										o_plot$elt <- f_create_element_data("temporal", 2, NULL, o_parameter, o_cond, o_plot, input$mode)
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
							
							o_click_legend$item <- f_build_legend_items("temporal", 2, NULL, o_parameter, o_cond, NULL, o_plot, isolate(o_click_legend$item), l_traces)
							
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
								if (input$ymin != l_results[[1]][1]) {
									updateTextInput(session, "ymin", value = as.character(l_results[[1]][1]))
								}
								
								if (input$ymax != l_results[[1]][2]) {
									updateTextInput(session, "ymax", value = as.character(l_results[[1]][2]))
								}
							}
							
							if (length(l_results[[length(l_results)]]) > 0) {
								f_showNotification(l_results[[length(l_results)]], duration = 15, type = "warning")
							}
							
							rm(list = "l_results")
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({
							v_dimension <- input$dimension
							df_click_legend <- isolate(o_click_legend$item)
							
							# build graph:
							
							ply_1 <- f_build_graph("temporal", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info)
							
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
						o_cond$flag <- 0
						o_cond$qc1 <- 0
						o_cond$qc2 <- 0
						
						o_plot$data <- data.frame()
						o_plot$id_group <- NA
						o_plot$y_coord <- NULL
						o_plot$color <- NA
						o_plot$add_pt <- F
						o_plot$pt_pos <- NA
						o_plot$data_qc1 <- NA
						o_plot$data_qc2 <- NA
						o_plot$elt <- NA
						
						if (isolate(o_cond$display) == 0) {
							if (length(e_current_flag) > 0) {
								rm(list = ls(e_current_flag), envir = e_current_flag)
								shinyjs::disable("clear1_button")
								shinyjs::disable("clear2_button")
								shinyjs::disable("save_button")
								js$resetClick()
							}
							
							# Checking process: all fields are filled ?
							# -----------------
							
							v_cond <- c(input$var_id == "", input$group == "yes" & ((input$concat2 == F & (length(which(input$var_group == "")) == 1 | is.null(input$var_group))) | (input$concat2 == T & is.null(input$var_group))))
							names(v_cond) <- c("ID", "Group")
							v_pos <- which(v_cond == T)
							
							if (length(v_pos) == 0) {
								if (input$id == "yes") {
									o_parameter$id <- input$var_id
								}
								else {
									o_parameter$id <- NA
								}
								
								o_parameter$concat2 <- input$concat2
								
								if (input$concat2 == T) {
									o_parameter$concat2_group <- input$var_group  
								}
								
								if (input$group == "yes") {
									if (input$concat2 == T) {
										eval(parse(text = paste0("v_row <- unique(c(", paste(paste0("which(is.na(as.vector(e_data$all$", input$var_group, ")))"), collapse = ", "), "))")))
										eval(parse(text = paste0("e_data$all$.concat2. <- as.vector(paste(", paste(paste0("as.vector(e_data$all$", input$var_group, ")"), collapse = ", "), ", sep = \" \"))")))
										e_data$all[v_row, ".concat2."] <- NA
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
								
								s_e_message <- f_check_variables("ir", e_data$all, o_parameter)
							}
							else {
								v_name <- names(v_cond)[v_pos]
								s_e_message <- paste0("Please complete the following field(s): ", paste(v_name, collapse = ", "))
							}
						}
						
						if (isolate(o_cond$save2) == 1) {
							s_e_message <- NULL
						}
						
						if (length(s_e_message) > 0) {
							f_showNotification(s_e_message, duration = 15, type = "error")
						}
						else {
							v_e_message <- c()
							
							if (isolate(o_cond$display) == 0) {
								o_zoom$coord <- NULL
								
								# edit graph labels
								
								if (isolate(o_parameter$autotitle) == F) {
									o_parameter$title <- input$edit1
									o_cond$label[1] <- 1
								}
								
								if (isolate(o_parameter$autoylab) == F) {
									o_parameter$ylab <- input$edit3
									o_cond$label[3] <- 1
								}
								else {
									o_parameter$ylab <- "y"
								}
								
								# Checking process: (check graphic options)
								# -----------------
								
								v_pos <- which(c(!isolate(o_parameter$autoop), !isolate(o_parameter$autodec_num)))
								
								if (length(v_pos) > 0) {
									l_opt_name <- list(input$op, input$dec_num)[v_pos]
									names(l_opt_name) <- c("op", "dec_num")[v_pos]
									v_e_message <- f_check_graphic_opt(l_opt_name, NULL, o_parameter)
									
									if (length(v_e_message) == 0) {
										eval(parse(text = paste(paste0("o_parameter$", names(l_opt_name), " <- l_opt_name[[", 1:length(l_opt_name), "]]"), collapse = "; ")))
									}
									
									rm(list = "l_opt_name")
								}
							}
							
							if (length(v_e_message) > 0) { # error with graphic options
								f_showNotification(paste(v_e_message, collapse = "<br/>"), duration = 15, type = "error")
							} 
							else { # no error
								# Data preparation: (process 1)
								# -----------------
								
								eval(parse(text = paste0("l_results <- f_prepare_data(\"ir\", 1, e_data$all, e_data$code_freq, isolate(o_data_info$code), ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", NULL, 0, NULL, NULL, NULL, NULL, o_parameter)")))
								
								if (length(l_results[[8]]) > 0) { # error with data preparation
									f_showNotification(l_results[[8]], duration = 15, type = "error")
								} 
								else { # no error
									eval(parse(text = paste(paste0("df_", c("all", "code_freq", "qc1", "qc2"), " <- l_results[[", 1:4, "]]"), collapse = "; ")))
									eval(parse(text = paste(paste0("o_cond$", c("flag", "qc1", "qc2"), " <- l_results[[5]][", 1:3, "]"), collapse = "; ")))
									l_id <- l_results[[6]]
									o_plot$pt_pos <- l_results[[7]]
									
									if (isolate(o_cond$display) == 0) {
										l_axis_layout <- list(rev(range(as.vector(df_code_freq$Frequency))), NULL)
									}
									
									# Graph color palette assignment: (o_plot reactive value)
									# -------------------------------
									
									o_plot$color <- f_create_col_palette("ir", l_results[[1]], o_parameter)
									
									# Data preparation: (process 2)
									# -----------------
									
									l_results <- f_prepare_data("ir", 2, df_all, df_code_freq, NULL, NULL, NULL, 0, NULL, list(df_qc1, df_qc2), l_id, NULL, o_parameter, isolate(o_plot$color))
									eval(parse(text = paste(paste0("o_plot$", c("data", "data_qc1", "data_qc2", "id_group"), " <- l_results[[", 1:4, "]]"), collapse = "; ")))
									rm(list = c("l_results", "df_all", "df_code_freq", "df_qc1", "df_qc2", "l_id"))
									
									# Create element data (process 1)
									# -------------------
									
									o_plot$elt <- f_create_element_data("ir", 1, NULL, NULL, o_cond, o_plot, input$mode)
									
									if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0 & input$mode == "line") {
										o_plot$add_pt <- T
									}
									
									# Build legend items (process 1)
									# ------------------
									
									o_click_legend$item <- f_build_legend_items("ir", 1,  NULL, o_parameter, NULL, NULL, o_plot)
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
										
										o_plot$elt <- f_create_element_data("ir", 2, NULL, o_parameter, o_cond, o_plot, input$mode)
										
										o_plot$add_pt <- ifelse(input$mode == "line", T, F)
									}
									
									o_cond$mode <- 0
								}
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
							
							o_click_legend$item <- f_build_legend_items("ir", 2, NULL, o_parameter, NULL, NULL, NULL, isolate(o_click_legend$item), l_traces)
							
							o_cond$display <- 0
						}
						
						if (input$y_scale %in% c("auto", "manual")) {
							# Update Y axis range (y_scale radio button: auto or manual)
							# -------------------
							
							eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(\"ir\", input$y_scale, e_data$all, o_parameter, o_zoom, o_plot, NULL, isolate(o_click_legend$item), ifelse(input$y_scale == \"auto\", input$fraction, NULL), ", ifelse(input$y_scale == "manual", "c(input$ymin, input$ymax)", "NULL"), ")")))
							
							l_axis_layout[[2]] <- l_results[[1]]
							
							if (input$y_scale == "auto") {
								o_plot$y_coord <- l_results[[2]] 
							}
							else {
								if (input$ymin != l_results[[1]][1]) {
									updateTextInput(session, "ymin", value = as.character(l_results[[1]][1]))
								}
								
								if (input$ymax != l_results[[1]][2]) {
									updateTextInput(session, "ymax", value = as.character(l_results[[1]][2]))
								}
							}
							
							if (length(l_results[[length(l_results)]]) > 0) {
								f_showNotification(l_results[[length(l_results)]], duration = 15, type = "warning")
							}
							
							rm(list = "l_results")
						}
						
						# Output (main panel)
						# -------------------
						
						output$graphic <- renderPlotly({	
							v_dimension <- input$dimension
							df_click_legend <- o_click_legend$item
							
							# build graph:
							
							ply_1 <- f_build_graph("ir", "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout, o_picture_info)
							
							# add flags:
							
							v_cond <- c(isolate(o_cond$flag), isolate(o_cond$qc1), isolate(o_cond$qc2))
							
							eval(parse(text = paste0("ply_1 <- f_add_flag(\"ir\", ply_1, v_cond, df_click_legend, o_plot, o_parameter, ", ifelse(v_cond[1] == 1, "e_data$flag", "NULL"), ", e_current_flag)")))
							
							# add statistics:
							
							if (isolate(o_parameter$mean_spect) == T) {
								ply_1 <- f_add_mean_spect(ply_1, df_click_legend, o_plot, o_parameter)
							}
							
							# add java script to save legend item status:
							
							ply_1 %>% onRender(s_js_2)
						})
						
						shinyjs::show(id = "graphic")
						shinyjs::show(id = "sh_popup1")
						shinyjs::show(id = "sh_reset")
					}
				}
			)
			
			shinyjs::delay(i_ms,
				if (dim(isolate(o_plot$data))[1] > 0) {
					if (isolate(o_click_button$display) == 0) {
						if (isolate(o_parameter$autoop) == F) {
							shinyjs::enable("op_button")
						}
						
						if (isolate(o_parameter$autobin) == F) {
							shinyjs::enable("bw_button")
						}
						
						if (isolate(o_parameter$y_scale) == "manual") {
							shinyjs::enable("yscale_button")
						}
						
						if (isolate(o_parameter$autodec_num) == F) {
							shinyjs::enable("dec_num_button")
						}
					}
					
					o_click_button$display <- 1
					
					if (input$edit1_radio == "yes" | input$edit2_radio == "yes" | input$edit3_radio == "yes" | input$edit4_radio == "yes") {
						shinyjs::enable("apply_button")
					}
					
					if (input$data_type != "normal") {
						if (input$mean_spect == F & is.na(isolate(o_parameter$g))) {
							shinyjs::enable("action")
							
							if (input$data_type == "temporal") {
								shinyjs::enable("draw")
							}
							
							if (input$action != "replace_qc") {
								shinyjs::enable("qc")
							}
							
							shinyjs::enable("comment")
						}
						else {
							if (input$action != "add_flag") {
								updateRadioButtons(session, "action", selected = "add_flag", inline = F)
								shinyjs::delay(100, disable("action"))
							}
							else {
								shinyjs::disable("action")
								
								if (input$qc != "2") {
									updateRadioButtons(session, "qc", selected = "2", inline = F)
									shinyjs::delay(100, disable("qc"))
								}
								else {
									shinyjs::disable("qc")
								}
							}
							
							if (input$data_type == "temporal") {
								if (input$draw == "mpt") {
									updateRadioButtons(session, "draw", selected = "pt", inline = F)
									shinyjs::delay(100, disable("draw"))
								}
								else {
									shinyjs::disable("draw")
								}
							}
							
							shinyjs::disable("comment")
						}
					}
					else {
						if (input$plot_type == "plot") {
							if (input$dim_num == "2d") {
								if (input$f_radio == "no" & input$g_radio == "no" & input$lreg == F & input$conf_ellipsoid == F & input$centroid == F) {
									shinyjs::enable("var_flag_1")
									
									if (is.null(input$var_flag_1)) {
										updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))
									}
								}
								else {
									if (!is.null(input$var_flag_1)) {
										updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
										shinyjs::delay(100, disable("var_flag_1"))
									}
									else {
										shinyjs::disable("var_flag_1")
									}
								}
								
								if (input$var_flag_2 == T) {
									updateCheckboxInput(session, "var_flag_2", value = F)
									shinyjs::delay(100, disable("var_flag_2"))
								}
								else {
									shinyjs::disable("var_flag_2")
								}
							}
							else {
								if (input$f_radio == "no" & input$g_radio == "no" & input$h_radio == "no" & input$centroid == F) {
									shinyjs::enable("var_flag_1")
									shinyjs::enable("var_flag_2")
									
									if (is.null(input$var_flag_1) & input$var_flag_2 == F) {
										updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))
										updateCheckboxInput(session, "var_flag_2", value = T)
									}
								}
								else {
									if (!is.null(input$var_flag_1)) {
										updateCheckboxGroupInput(session, "var_flag_1", selected = character(0))
										shinyjs::delay(100, disable("var_flag_1"))
									}
									else {
										shinyjs::disable("var_flag_1")
									}
									
									if (input$var_flag_2 == T) {
										updateCheckboxInput(session, "var_flag_2", value = F)
										shinyjs::delay(100, disable("var_flag_2"))
									}
									else {
										shinyjs::disable("var_flag_2")
									}
								}
							}
						}
						
						shinyjs::disable("action")
						shinyjs::disable("qc")
						shinyjs::disable("comment")
					}
				}
				else {
					click("reset1_button")
					
					if (isolate(o_cond$concat1) == 0 & ".concat1." %in% names(e_data$all)) {
						e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat1.")]
					}
					
					if (isolate(o_cond$concat2) == 0 & ".concat2." %in% names(e_data$all)) {
						e_data$all <- e_data$all[, -which(names(e_data$all) == ".concat2.")]
					}
				}
			)
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
			
			if (isolate(o_click_button$display) == 1) {
				o_cond$webgl <- 1
				click("update_button")
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.2. Add events for the "mode" radio button 
	# ===========================================
	
	observeEvent(input$mode, {
		if ("all" %in% ls(e_data)) {
			o_parameter$mode <- input$mode  
			
			if (isolate(o_click_button$display) == 1) {
				o_cond$mode <- 1
				click("update_button")
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.3. Add events on the opacity radio button 
	# ===========================================
	
	observeEvent(input$op_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$op_radio == "auto") {
					o_parameter$autoop <- T
					o_parameter$op <- NA
					
					if (length(input$op) > 0) {
						updateNumericInput(session, "op", value = numeric(0), min = 0, max = 1)
						shinyjs::delay(100, disable("op"))
					}
					else {
						shinyjs::disable("op")
					}
					
					shinyjs::disable("op_button")
					
					click("update_button")
					o_cond$display <- 1
					click("display_button")
				}
				else {
					o_parameter$autoop <- F
					shinyjs::enable("op")
					updateNumericInput(session, "op", value = ifelse(input$plot_type %in% c("plot", "boxplot") | input$data_type %in% c("temporal", "ir"), 0.7, 0.5), min = 0, max = ifelse(input$plot_type %in% c("plot", "boxplot") | input$data_type %in% c("temporal", "ir"), 1, 0.7))
					shinyjs::enable("op_button")
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$op_radio == "auto") {
						o_parameter$autoop <- T
						o_parameter$op <- NA
						
						if (length(input$op) > 0) {
							updateNumericInput(session, "op", value = numeric(0), min = 0, max = 1)
							shinyjs::delay(100, disable("op"))
						}
						else {
							shinyjs::disable("op")
						}
					}
					else {
						o_parameter$autoop <- F
						shinyjs::enable("op")
						updateNumericInput(session, "op", value = ifelse(input$plot_type %in% c("plot", "boxplot") | input$data_type %in% c("temporal", "ir"), 0.7, 0.5), min = 0, max = ifelse(input$plot_type %in% c("plot", "boxplot") | input$data_type %in% c("temporal", "ir"), 1, 0.7))
					}
				}
			}
		}
	})
	
	# 3.4. Add events on "double-arrows" button corresponding to opacity adjustment
	# =============================================================================
	
	observeEvent(input$op_button, {
		if ("all" %in% ls(e_data)) {
			n_op <- input$op
			
			if (!is.na(n_op)) {
				n_max <- ifelse(isolate(o_parameter$plot_type) %in% c("plot", "boxplot") | input$data_type %in% c("temporal", "ir"), 1, 0.7)
				
				if (n_op >= 0 & n_op <= n_max) {
					o_parameter$op <- n_op
					
					click("update_button")
					o_cond$display <- 1
					click("display_button")
				}
				else {
					showNotification(paste0("The opacity value must be between 0 and ", n_max), duration = 15, type = "error")
				}
			}
			else {
				showNotification("The opacity value is not numeric", duration = 15, type = "error")
			}
		}
	})
	
	# 3.5. Add events on the bin width radio button
	# =============================================
	
	observeEvent(input$bin_radio, {
		if (isolate(o_click_button$display) == 1) {
			if (input$bin_radio == "auto") {
				o_parameter$autobin <- T
				o_parameter$bw <- NA
				
				if (length(input$bw) > 0) {
					updateTextInput(session, "bw", value = character(0))
					shinyjs::delay(100, disable("bw"))
				}
				else {
					shinyjs::disable("bw")
				}
				
				shinyjs::disable("bw_button")
				
				click("update_button")
				o_cond$display <- 1
				click("display_button")
			}
			else {
				o_parameter$autobin <- F
				shinyjs::enable("bw")
				shinyjs::enable("bw_button")
			}
		}
		else {
			if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
				if (input$bin_radio == "auto") {
					o_parameter$autobin <- T
					o_parameter$bw <- NA
					
					if (length(input$bw) > 0) {
						updateTextInput(session, "bw", value = character(0))
						shinyjs::delay(100, disable("bw"))
					}
					else {
						shinyjs::disable("bw")
					}
				}
				else {
					o_parameter$autobin <- F
					shinyjs::enable("bw")
				}
			}
		}
	})
	
	# 3.6. Add events on "double-arrows" button to change histplot bin width
	# ======================================================================
	
	observeEvent(input$bw_button, {
		if ("all" %in% ls(e_data)) {
			n_bw <- tryCatch({suppressWarnings(eval(parse(text = input$bw)))}, error = function(e) FALSE)
			n_val <- abs(diff(range(as.vector(isolate(o_plot$data)[, isolate(o_parameter$x)]))))
			
			if (is.numeric(n_bw)) {
				if (n_bw > 0 & n_bw < n_val) {
					o_parameter$bw <- n_bw
					
					click("update_button")
					o_cond$display <- 1
					click("display_button")
				}
				else {
					showNotification(paste0("The bin width must be between 0 and ", round(n_val, digits = 2)), duration = 15, type = "error")
				}
			}
			else {
				showNotification("Incorrect bin width", duration = 15, type = "error")
			}
		}
	})
	
	# 3.7. Add events to Y scale radio button
	# =======================================
	
	observeEvent(input$y_scale, {
		if ("all" %in% ls(e_data)) {
			if (input$y_scale == "auto") {
				shinyjs::enable("fraction")
				updateNumericInput(session, "fraction", value = 0.05)
			
				if (isolate(o_parameter$y_scale) == "manual") {
					updateTextInput(session, "ymin", value = character(0))
					shinyjs::delay(100, disable("ymin"))
					updateTextInput(session, "ymax", value = character(0))
					shinyjs::delay(100, disable("ymax"))
					shinyjs::disable("yscale_button")
				}
				
				o_parameter$y_scale <- "auto"
			}
			else if (input$y_scale == "manual") { 
				shinyjs::enable("ymin")
				shinyjs::enable("ymax")
				
				if (isolate(o_click_button$display) == 1) {
					shinyjs::enable("yscale_button")
				}
				
				if (isolate(o_parameter$y_scale) == "auto") {
					updateNumericInput(session, "fraction", value = numeric(0))
					shinyjs::delay(100, disable("fraction"))
				}
				
				o_parameter$y_scale <- "manual"
				
				if (isolate(o_click_button$display) == 1) {
					if (input$data_type == "temporal") {
						v_y_range <- range(e_data$all[, isolate(o_parameter$y)], na.rm = T)
					}
					else {
						v_code <- as.vector(e_data$code_freq$Code)
						v_y_range <- range(e_data$all[, which(names(e_data$all) %in% v_code)], na.rm = T)
					}
					
					updateTextInput(session, "ymin", value = v_y_range[1])
					updateTextInput(session, "ymax", value = v_y_range[2])
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = isolate(o_parameter$ylab), range = v_y_range)))
				}
			}
			else {
				o_plot$y_coord <- NULL
				
				if (isolate(o_parameter$y_scale) == "auto") {
					updateNumericInput(session, "fraction", value = numeric(0))
					shinyjs::delay(100, disable("fraction"))
				}
				
				if (isolate(o_parameter$y_scale) == "manual") {
					updateTextInput(session, "ymin", value = character(0))
					shinyjs::delay(100, disable("ymin"))
					updateTextInput(session, "ymax", value = character(0))
					shinyjs::delay(100, disable("ymax"))
					shinyjs::disable("yscale_button")
				}
				
				o_parameter$y_scale <- "none"
				
				if (isolate(o_click_button$display) == 1) {
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = isolate(o_parameter$ylab), range = NULL)))
				}
			}
		}
	})
	
	# 3.8. Add events on fraction numeric input
	# =========================================
	
	observeEvent(input$fraction, {
		if ("all" %in% ls(e_data)) {
			if (input$y_scale == "auto") {
				if (is.na(input$fraction)) {
					updateNumericInput(session, "fraction", value = 0.05)
					showNotification("Fraction value is not numeric. So, the value is replaced by the standard value: 0.05.", type = "warning")
				}
				else {
					if (input$fraction < 0) {
						updateNumericInput(session, "fraction", value = 0)
						showNotification("Fraction value is inferior to the minimal value (0). So, the value is replaced by the minimal value.", type = "warning")
					}
					
					if (input$fraction > 0.1) {
						updateNumericInput(session, "fraction", value = 0.1)
						showNotification("Fraction value is superior to the maximal value (0.1). So, the value is replaced by the maximal value.", type = "warning")
					}
				}
				
				if (isolate(o_click_button$display) == 1) {
					if (!is.na(input$fraction) & input$fraction >= 0 & input$fraction <= 0.1) { 
						eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data$all, o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", isolate(o_click_legend$item), input$fraction, NULL)")))
						o_plot$y_coord <- l_results[[2]] 
						
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = isolate(o_parameter$ylab), range = l_results[[1]])))
						
						if (length(l_results[[3]]) > 0) {
							showNotification(l_results[[3]], duration = 15, type = "warning")
						}
						
						rm(list = "l_results")
					}
				}
			}
		}
	})
	
	# 3.9. Add events on "double-arrows" button for manual y scale adjustment 
	# =======================================================================
	
	observeEvent(input$yscale_button, {
		if (isolate(o_click_button$display) == 1) {
			click("update_button")
			o_cond$display <- 1
			click("display_button")
		}
	})
	
	# 3.10. Add events on decimal number radio button
	# ===============================================
	
	observeEvent(input$dec_num_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$dec_num_radio == "auto") {
					o_parameter$autodec_num <- T
					o_parameter$dec_num <- NA
					
					if (length(input$dec_num) > 0) {
						updateNumericInput(session, "dec_num", value = numeric(0))
						shinyjs::delay(100, disable("dec_num"))
					}
					else {
						shinyjs::disable("dec_num")
					}
					
					shinyjs::disable("dec_num_button")
					
					click("update_button")
					o_cond$display <- 1
					click("display_button")
				}
				else {
					o_parameter$autodec_num <- F
					shinyjs::enable("dec_num")
					updateNumericInput(session, "dec_num", value = 2)
					shinyjs::enable("dec_num_button")
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$dec_num_radio == "auto") {
						o_parameter$autodec_num <- T
						o_parameter$dec_num <- NA
						
						if (length(input$dec_num) > 0) {
							updateNumericInput(session, "dec_num", value = numeric(0))
							shinyjs::delay(100, disable("dec_num"))
						}
						else {
							shinyjs::disable("dec_num")
						}
					}
					else {
						o_parameter$autodec_num <- F
						shinyjs::enable("dec_num")
						updateNumericInput(session, "dec_num", value = 2)
					}
				}
			}
		}
	})
	
	# 3.11. Add events on "double-arrows" button corresponding to decimal number
	# ==========================================================================
	
	observeEvent(input$dec_num_button, {
		if ("all" %in% ls(e_data)) {
			i_dec_num <- input$dec_num
			
			if (!is.na(i_dec_num)) {
				if (is.numeric(i_dec_num) == T & length(grep("[.]", i_dec_num)) == 0) {
					if (i_dec_num >= 0) {
						o_parameter$dec_num <- i_dec_num
						
						click("update_button")
						o_cond$display <- 1
						click("display_button")
					}
					else {
						showNotification("The decimal number must be an integer superior or equal to 0", duration = 15, type = "error")
					}
				}
				else {
					showNotification("The decimal number must be an integer superior or equal to 0", duration = 15, type = "error")
				}
			}
			else {
				showNotification("The decimal number must be an integer superior or equal to 0", duration = 15, type = "error")
			}
		}
	})
	
	# 3.12. Add events on title/axis label radio buttons
	# ==================================================
	
	# Title:
	
	observeEvent(input$edit1_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$edit1_radio == "no") {
					o_parameter$autotitle <- T
					
					if (length(input$edit1) > 0) {
						updateTextInput(session, "edit1", value = character(0))
						shinyjs::delay(100, disable("edit1"))
					}
					else {
						shinyjs::disable("edit1")
					}
					
					if (input$edit2_radio == "no" & input$edit3_radio == "no" & input$edit4_radio == "no") {
						shinyjs::disable("apply_button")
					}
					
					if (!is.na(isolate(o_parameter$title))) {
						o_parameter$title <- NA
						o_cond$label[1] <- 0
						click("update_button")
						o_cond$display <- 1
						click("display_button")
					}
				}
				else {
					o_parameter$autotitle <- F
					shinyjs::enable("edit1")
					
					if (input$edit2_radio == "no" & input$edit3_radio == "no" & input$edit4_radio == "no") {
						shinyjs::enable("apply_button")
					}
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$edit1_radio == "no") {
						o_parameter$autotitle <- T
						o_parameter$title <- NA
						
						if (length(input$edit1) > 0) {
							updateTextInput(session, "edit1", value = character(0))
							shinyjs::delay(100, disable("edit1"))
						}
						else {
							shinyjs::disable("edit1")
						}
					}
					else {
						o_parameter$autotitle <- F
						shinyjs::enable("edit1")
					}
				}
			}
		}
	})
	
	# X:
	
	observeEvent(input$edit2_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$edit2_radio == "no") {
					o_parameter$autoxlab <- T
					
					if (length(input$edit2) > 0) {
						updateTextInput(session, "edit2", value = character(0))
						shinyjs::delay(100, disable("edit2"))
					}
					else {
						shinyjs::disable("edit2")
					}
					
					if (input$edit1_radio == "no" & input$edit3_radio == "no" & input$edit4_radio == "no") {
						shinyjs::disable("apply_button")
					}
					
					if (isolate(o_cond$label)[2] == 1) {
						if (!is.na(o_parameter$f)) {
							o_parameter$xlab <- "f(x)"
						}
						else {
							o_parameter$xlab <- "x"
						}
						
						o_cond$label[2] <- 0
						click("update_button")
						o_cond$display <- 1
						click("display_button")
					}
				}
				else {
					o_parameter$autoxlab <- F
					shinyjs::enable("edit2")
					
					if (input$edit1_radio == "no" & input$edit3_radio == "no" & input$edit4_radio == "no") {
						shinyjs::enable("apply_button")
					}
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$edit2_radio == "no") {
						o_parameter$autoxlab <- T
						o_parameter$xlab <- "x"
						
						if (length(input$edit2) > 0) {
							updateTextInput(session, "edit2", value = character(0))
							shinyjs::delay(100, disable("edit2"))
						}
						else {
							shinyjs::disable("edit2")
						}
					}
					else {
						o_parameter$autoxlab <- F
						shinyjs::enable("edit2")
					}
				}
			}
		}
	})
	
	# Y:
	
	observeEvent(input$edit3_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$edit3_radio == "no") {
					o_parameter$autoylab <- T
					
					if (length(input$edit3) > 0) {
						updateTextInput(session, "edit3", value = character(0))
						shinyjs::delay(100, disable("edit3"))
					}
					else {
						shinyjs::disable("edit3")
					}
					
					if (input$edit1_radio == "no" & input$edit2_radio == "no" & input$edit4_radio == "no") {
						shinyjs::disable("apply_button")
					}
					
					if (isolate(o_cond$label)[3] == 1) {
						if (!is.na(o_parameter$g)) {
							o_parameter$ylab <- "g(y)"
						}
						else {
							o_parameter$ylab <- "y"
						}
						
						o_cond$label[3] <- 0
						click("update_button")
						o_cond$display <- 1
						click("display_button")
					}
				}
				else {
					o_parameter$autoylab <- F
					shinyjs::enable("edit3")
					
					if (input$edit1_radio == "no" & input$edit2_radio == "no" & input$edit4_radio == "no") {
						shinyjs::enable("apply_button")
					}
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$edit3_radio == "no") {
						o_parameter$autoylab <- T
						o_parameter$ylab <- "y"
						
						if (length(input$edit3) > 0) {
							updateTextInput(session, "edit3", value = character(0))
							shinyjs::delay(100, disable("edit3"))
						}
						else {
							shinyjs::disable("edit3")
						}
					}
					else {
						o_parameter$autoylab <- F
						shinyjs::enable("edit3")
					}
				}
			}
		}
	})
	
	# Z:
	
	observeEvent(input$edit4_radio, {
		if ("all" %in% ls(e_data)) {
			if (isolate(o_click_button$display) == 1) {
				if (input$edit4_radio == "no") {
					o_parameter$autozlab <- T
					
					if (length(input$edit4) > 0) {
						updateTextInput(session, "edit4", value = character(0))
						shinyjs::delay(100, disable("edit4"))
					}
					else {
						shinyjs::disable("edit4")
					}
					
					if (input$edit1_radio == "no" & input$edit2_radio == "no" & input$edit3_radio == "no") {
						shinyjs::disable("apply_button")
					}
					
					if (isolate(o_cond$label)[4] == 1) {
						if (!is.na(o_parameter$h)) {
							o_parameter$zlab <- "h(z)"
						}
						else {
							o_parameter$zlab <- "z"
						}
						
						o_cond$label[4] <- 0
						click("update_button")
						o_cond$display <- 1
						click("display_button")
					}
				}
				else {
					o_parameter$autozlab <- F
					shinyjs::enable("edit4")
					
					if (input$edit1_radio == "no" & input$edit2_radio == "no" & input$edit3_radio == "no") {
						shinyjs::enable("apply_button")
					}
				}
			}
			else {
				if (isolate(o_click_button$load1) == 1 & "all" %in% ls(e_data)) {
					if (input$edit4_radio == "no") {
						o_parameter$autozlab <- T
						o_parameter$zlab <- NA
						
						if (length(input$edit4) > 0) {
							updateTextInput(session, "edit4", value = character(0))
							shinyjs::delay(100, disable("edit4"))
						}
						else {
							shinyjs::disable("edit4")
						}
					}
					else {
						o_parameter$autozlab <- F
						shinyjs::enable("edit4")
					}
				}
			}
		}
	})
	
	# 3.13. Add events on apply button
	# ================================
	
	observeEvent(input$apply_button, {
		if ("all" %in% ls(e_data)) {
			if (input$edit1_radio == "yes") {
				o_parameter$title <- input$edit1
				o_cond$label[1] <- 1
			}
			
			if (input$edit2_radio == "yes") {
				o_parameter$xlab <- input$edit2
				o_cond$label[2] <- 1
			}
			
			if (input$edit3_radio == "yes") {
				o_parameter$ylab <- input$edit3
				o_cond$label[3] <- 1
			}
			
			if (input$edit4_radio == "yes") {
				o_parameter$zlab <- input$edit4
				o_cond$label[4] <- 1
			}
			
			click("update_button")
			o_cond$display <- 1
			click("display_button")
		}
	})
	
	# ----
	# Flag
	# ----
	
	# 3.14. Add events on "action" radio button 
	# =========================================
	
	observeEvent(input$action, {
		if (input$data_type %in% c("temporal", "ir") & isolate(o_click_button$display) == 1) {
			if (input$action == "add_flag") {
				if (input$mean_spect == F) {
					shinyjs::enable("qc")
				}
			}
			else {
				if (input$qc == "1") {
					updateRadioButtons(session, "qc", choices = c("1" = "1", "2" = "2"), selected = "2", inline = F)
					shinyjs::delay(100, disable("qc"))
				}
				else {
					shinyjs::disable("qc")
				}
			}
			
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
						o_click_graph$prev_date <- NULL
						o_click_graph$prev_var <- NULL
					}
					
					v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num  - 1)
				}
				else {
					df_elt <- isolate(o_plot$elt)
					i_tot <- sum(rowSums(df_elt[, -1]))
					v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
				}
				
				s_num_del <- paste(v_num_del, collapse = ", ")
				
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
				
				rm(list = ls(e_current_flag), envir = e_current_flag)
				shinyjs::disable("clear1_button")
				shinyjs::disable("clear2_button")
				shinyjs::disable("save_button")
			}
			
			js$resetClick()
		}
	})
	
	# 3.15. Add events on X/Y/Z variable check boxes 
	# ==============================================
	
	observeEvent(c(input$var_flag_1, input$var_flag_2), {
		if (isolate(o_click_button$display) == 1) {
			if (input$data_type == "normal" & input$plot_type == "plot") {
				if (length(e_current_flag) > 0) {
					v_num_del <- isolate(o_plot$elt):(isolate(o_plot$elt) + e_current_flag$num - 1)
					s_num_del <- paste(v_num_del, collapse = ", ")
					
					eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
					
					rm(list = ls(e_current_flag), envir = e_current_flag)
					shinyjs::disable("clear1_button")
					shinyjs::disable("clear2_button")
					shinyjs::disable("save_button")
					js$resetClick()
					
					if (input$dim_num == "2d") {
						shinyjs::enable("lreg")
						shinyjs::enable("conf_ellipsoid")
						shinyjs::enable("centroid")
					}
					else {
						shinyjs::enable("centroid")
					}
				}
				
				if (input$dim_num == "2d") {
					if (input$lreg == F & input$conf_ellipsoid == F & input$centroid == F & input$f_radio == "no" & input$g_radio == "no") {
						if (is.null(input$var_flag_1)) {
							updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))
						}
					}
				}
				else {
					if (input$centroid == F & input$f_radio == "no" & input$g_radio == "no" & input$h_radio == "no") {
						if (is.null(input$var_flag_1) & input$var_flag_2 == F) {
							updateCheckboxGroupInput(session, "var_flag_1", selected = c("flag_x", "flag_y"))
							updateCheckboxInput(session, "var_flag_2", value = T)
						}
					}
				}
			}
		}
	})
	
	# 3.16. Add events on the "draw" radio button
	# ===========================================
	
	observeEvent(input$draw, {
		if (input$draw == "pt" & "click_iter" %in% ls(e_current_flag)) {
			o_click_graph$prev_date <- NULL
			o_click_graph$prev_var <- NULL
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
				
				shinyjs::enable("save_button")
			}
			else {
				rm("num", envir = e_current_flag)
				rm("all_coord", envir = e_current_flag)
				
				i_num_del <- length(isolate(o_plot$elt))
				ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "deleteTraces", list(i_num_del))
			
				shinyjs::disable("clear1_button")
				shinyjs::disable("clear2_button")
			}
			
			js$resetClick()
		}
	})
	
	# 3.17. Add events of clear button
	# ================================
	
	observeEvent(input$clear1_button, {
		if (isolate(o_click_button$display) == 1) {
			if (input$data_type == "normal") {
				v_num_del <- isolate(o_plot$elt) + e_current_flag$num - 1
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))
				
				if (e_current_flag$num > 1) {
					e_current_flag$coord <- e_current_flag$coord[-which(e_current_flag$coord$num == e_current_flag$num),]
					e_current_flag$num <- e_current_flag$num - 1
				}
				else {
					if (input$dim_num == "2d") {
						shinyjs::enable("lreg")
						shinyjs::enable("conf_ellipsoid")
					}
					
					shinyjs::enable("centroid")
					rm(list = ls(e_current_flag), envir = e_current_flag)
					shinyjs::disable("clear1_button")
					shinyjs::disable("clear2_button")
					shinyjs::disable("save_button")
				}
				
				js$resetClick()
			}
			else if (input$data_type == "temporal") {
				if ("click_iter" %in% ls(e_current_flag)) {
					if ("coord" %in% ls(e_current_flag)) {
						df_num <- as.data.frame(addmargins(table(e_current_flag$coord$num)))
						df_num <- df_num[-dim(df_num)[1],]
						names(df_num) <- c("name", "freq")
						v_num <- as.vector(df_num[df_num$freq == 1, "name"])
						v_geom <- e_current_flag$coord[e_current_flag$coord$num %in% v_num, "geom"]
						v_num_del <- length(isolate(o_plot$elt)) + length(which(df_num$freq > 1)) * 3 + length(which(v_geom == "pt")) + length(which(v_geom == "mpt")) * 3
						
						o_click_graph$prev_date <- NULL
						o_click_graph$prev_var <- NULL
						
						eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))	
						
						e_current_flag$all_coord <- e_current_flag$all_coord[-which(e_current_flag$all_coord$num == e_current_flag$num),]
						e_current_flag$num <- e_current_flag$num - 1
						rm("click_iter", envir = e_current_flag)
						shinyjs::enable("save_button")
					}
					else {
						v_num_del <- length(isolate(o_plot$elt))
						o_click_graph$prev_date <- NULL
						o_click_graph$prev_var <- NULL
						
						eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", v_num_del, "))")))	
						
						rm(list = ls(e_current_flag), envir = e_current_flag)
						shinyjs::disable("clear1_button")
						shinyjs::disable("clear2_button")
						shinyjs::disable("save_button")
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
							shinyjs::disable("clear1_button")
							shinyjs::disable("clear2_button")
							shinyjs::disable("save_button")
						}
						
						js$resetClick()
					}
				}
			}
			else {
				df_elt <- isolate(o_plot$elt)
				i_tot <- sum(rowSums(df_elt[, -1]))
				v_num_del <- i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1
				
				if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
					v_num_del <- c(v_num_del - 1, v_num_del)
				}
				
				s_num_del <- paste(v_num_del, collapse = ", ")
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))
				
				if (dim(e_current_flag$coord)[1] > 1) {
					e_current_flag$coord <- e_current_flag$coord[-dim(e_current_flag$coord)[1],]
				}
				else {
					rm(list = ls(e_current_flag), envir = e_current_flag)
					shinyjs::disable("clear1_button")
					shinyjs::disable("clear2_button")
					shinyjs::disable("save_button")
					shinyjs::enable("mean_spect")
				}
				
				js$resetClick()
			}
		}
	})
	
	# 3.18. Add events of clear all button
	# ====================================
	
	observeEvent(input$clear2_button, {
		if (length(e_current_flag) > 0) {
			if (input$data_type == "normal") {
				if (input$dim_num == "2d") {
					shinyjs::enable("lreg")
					shinyjs::enable("conf_ellipsoid")
				}
				
				shinyjs::enable("centroid")
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
					o_click_graph$prev_date <- NULL
					o_click_graph$prev_var <- NULL
				}
				
				v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num - 1)
			}
			else {
				df_elt <- isolate(o_plot$elt)
				i_tot <- sum(rowSums(df_elt[, -1]))
				v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
				shinyjs::enable("mean_spect")
			}
			
			s_num_del <- paste(v_num_del, collapse = ", ")
			eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
			
			rm(list = ls(e_current_flag), envir = e_current_flag)
			shinyjs::disable("clear1_button")
			shinyjs::disable("clear2_button")
			shinyjs::disable("save_button")
			js$resetClick()
		}
	})
	
	# 3.19. Add events of save button
	# ===============================
	
	observeEvent(input$save_button, {
		if (length(e_current_flag) > 0) {
			v_split_path <- unlist(strsplit(input$data_path1, split = "/"))
			
			if (is.na(isolate(o_flag$name))) {
				o_flag$name <- f_create_flag_data_name(input$data_type, v_split_path, isolate(o_parameter$id))
			}
			
			s_flag_path <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/", isolate(o_flag$name))
			
			if ("flag" %in% ls(e_data)) {
				b_cond_1 <- f_file_write(e_data$flag, s_flag_path)
			}
			else {
				b_cond_2 <- isolate(o_flag$name) %in% list.files(paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/"))
				
				if (b_cond_2 == T) {
					df_flag <- fread(s_flag_path)
					b_cond_1 <- f_file_write(df_flag, s_flag_path)
				}
				else {
					b_cond_1 <- NULL
				}
			}
			
			if (is.null(b_cond_1)) {
				if (input$action == "add_flag") {
					if (!"flag" %in% ls(e_data)) {
						o_cond$save1 <- 1
					}
				}
				
				if (input$data_type == "normal") {
					if (input$dim_num == "2d") {
						shinyjs::enable("lreg")
						shinyjs::enable("conf_ellipsoid")
					}
					
					shinyjs::enable("centroid")
					eval(parse(text = paste0("e_data$flag <- f_save_current_flag_data(s_data_type = \"normal\", df_previous_flag = ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", df_current_flag = e_current_flag$coord, s_id_var = isolate(o_parameter$id))")))
					fwrite(e_data$flag, file = s_flag_path, sep = ",")
					v_num_del <- isolate(o_plot$elt):(isolate(o_plot$elt) + e_current_flag$num - 1)
				}
				else if (input$data_type == "temporal") {
					if ("data" %in% ls(e_previous_flag)) {
						rm("data", envir = e_previous_flag)
					}
					
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
						o_click_graph$prev_date <- NULL
						o_click_graph$prev_var <- NULL
					}
					
					v_num_del <- length(isolate(o_plot$elt)):(length(isolate(o_plot$elt)) + i_object_num - 1)
					
					o_plot$elt_pt_pos <- NA
					o_plot$add_pt <- NA
					o_plot$var_pt <- NA
					o_plot$var_pt_color <- NA
				}
				else {
					shinyjs::enable("mean_spect")
					
					eval(parse(text = paste0("e_data$flag <- f_save_current_flag_data(\"ir\", input$action, ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", e_current_flag$coord, NULL, isolate(o_parameter$id), NA, as.numeric(input$qc), input$comment)")))
					fwrite(e_data$flag, file = s_flag_path, sep = ",")
					
					df_elt <- isolate(o_plot$elt)
					i_tot <- sum(rowSums(df_elt[, -1]))
					v_num_del <- i_tot:(i_tot + sum(as.vector(e_current_flag$coord$pt) + 1) - 1)
				}
				
				s_num_del <- paste(v_num_del, collapse = ", ")
				eval(parse(text = paste0("ply_1 <- plotlyProxyInvoke(p = plotlyProxy(\"graphic\", session), \"deleteTraces\", list(", s_num_del, "))")))	
				
				rm(list = ls(e_current_flag), envir = e_current_flag)
				shinyjs::disable("clear1_button")
				shinyjs::disable("clear2_button")
				shinyjs::disable("save_button")
				js$resetClick()
				
				o_plot$elt <- NA
				
				click("update_button")
				
				if (isolate(o_cond$save1) == 1) {
					if (input$data_type %in% c("normal", "ir")) {
						shinyjs::disable("id")
						
						if (!is.na(isolate(o_parameter$id))) {
							shinyjs::disable("var_id")
						}
					}
					
					updateCheckboxInput(session, "flag", value = T)
				}
				
				o_cond$display <- 1
				o_cond$save2 <- 1
				click("display_button")
			}
			else {
				showNotification(paste0("Please close the flag file (", s_flag_path, ")"), duration = 15, type = "error")
			}
		}
	})
	
	# ----------
	# Statistics
	# ----------
	
	# 3.20. Add events on linear regression check box
	# ===============================================
	
	observeEvent(input$lreg, {
		if ("all" %in% ls(e_data)) {
			if (input$lreg == T) {
				o_parameter$lreg <- T
				
				if (isolate(o_cond$flag_msg) == 0) {
					showNotification("The flag tab is disabled because a function (f, g, h) is enabled or one of these options is selected in the Statistics tab: linear regression, confidence ellipsoid, centroid", duration = 15, type = "warning")
					o_cond$flag_msg <- 1
				}
				
				if (isolate(o_click_button$display) == 1) {
					if (input$model == "valid") {
						v_choice <- c("lreg parameters", "R2", "t-test on lreg parameters") 
					}
					else {
						v_choice <- c("lreg parameters", "R2", "RMSE")
					}
					
					updateSelectizeInput(session, "lreg_info_elt", choices = v_choice)
					shinyjs::show(id = "sh_popup2")
					
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$lreg <- F
				
				if (isolate(o_click_button$display) == 1) {
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
					
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- grep("[(]lreg[)]", names(input$traces))
							
							if ((length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								js$resetClick_leg()
							}
							else {
								v_pos_1 <- grep("[(]lreg[)]", names(input$traces)[v_pos_1])
								
								if (length(v_pos_1) > 0) {
									o_cond$legend <- 1
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				js$resetClick()
				click("update_button")
				o_cond$stat <- 1
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.21. Add events on confidence ellipsoid check box
	# ==================================================
	
	observeEvent(input$conf_ellipsoid, {
		if ("all" %in% ls(e_data)) {
			if (input$conf_ellipsoid == T) {
				o_parameter$conf_ellipsoid <- T
				
				if (isolate(o_cond$flag_msg) == 0) {
					showNotification("The flag tab is disabled because a function (f, g, h) is enabled or one of these options is selected in the Statistics tab: linear regression, confidence ellipsoid, centroid", duration = 15, type = "warning")
					o_cond$flag_msg <- 1
				}
				
				if (isolate(o_click_button$display) == 1) {
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$conf_ellipsoid <- F
				
				if (isolate(o_click_button$display) == 1) {
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- grep("[(]ellipsoid[)]", names(input$traces))
							
							if ((length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								js$resetClick_leg()
							}
							else {
								v_pos_1 <- grep("[(]ellipsoid[)]", names(input$traces)[v_pos_1])
								
								if (length(v_pos_1) > 0) {
									o_cond$legend <- 1
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				js$resetClick()
				click("update_button")
				o_cond$stat <- 2
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.22. Add events on centroid check box
	# ======================================
	
	observeEvent(input$centroid, {
		if ("all" %in% ls(e_data)) {
			if (input$centroid == T) {
				o_parameter$centroid <- T
				
				if (isolate(o_cond$flag_msg) == 0) {
					showNotification("The flag tab is disabled because a function (f, g, h) is enabled or one of these options is selected in the Statistics tab: linear regression, confidence ellipsoid, centroid", duration = 15, type = "warning")
					o_cond$flag_msg <- 1
				}
				
				if (isolate(o_click_button$display) == 1) {
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$centroid <- F
				
				if (isolate(o_click_button$display) == 1) {
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- grep("[(]centroid[)]", names(input$traces))
							
							if ((length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								js$resetClick_leg()
							}
							else {
								v_pos_1 <- grep("[(]centroid[)]", names(input$traces)[v_pos_1])
								
								if (length(v_pos_1) > 0) {
									o_cond$legend <- 1
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				js$resetClick()
				click("update_button")
				o_cond$stat <- 3
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.23. Add events on mean/sd check box
	# =====================================
	
	observeEvent(input$box_mean_sd, {
		if ("all" %in% ls(e_data)) {
			if (input$box_mean_sd == T) {
				o_parameter$boxmean <- "\"sd\""
			}
			else {
				o_parameter$boxmean <- "NULL"
			}
			
			if (isolate(o_click_button$display) == 1) {
				click("update_button")
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.24. Add events on density curve check box
	# ===========================================
	
	observeEvent(input$dens_curve, {
		if ("all" %in% ls(e_data)) {
			if (input$dens_curve == T) {
				o_parameter$dens_curve <- T
				
				if (isolate(o_click_button$display) == 1) {
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$dens_curve <- F
				
				if (isolate(o_click_button$display) == 1) {
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- c(grep("[(]curve[)]", names(input$traces)), grep("[(]normal curve[)]", names(input$traces)))
							
							if (input$norm_dens_curve == F & (length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								if (isolate(o_cond$legend) == 1) {
									o_cond$legend <- 0
								}
								
								js$resetClick_leg()
							}
							else {
								if (isolate(o_cond$legend) == 0) {
									v_pos_1 <- grep("[(]curve[)]", names(input$traces)[v_pos_1])
									
									if (length(v_pos_1) > 0) {
										o_cond$legend <- 1
									}
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				click("update_button")
				o_cond$stat <- 1
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.25. Add events on normal density curve check box 
	# ==================================================
	
	observeEvent(input$norm_dens_curve, {
		if ("all" %in% ls(e_data)) {
			if (input$norm_dens_curve == T) {
				o_parameter$norm_dens_curve <- T
				
				if (isolate(o_click_button$display) == 1) {
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$norm_dens_curve <- F
				
				if (isolate(o_click_button$display) == 1) {
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- c(grep("[(]curve[)]", names(input$traces)), grep("[(]normal curve[)]", names(input$traces)))
							
							if (input$dens_curve == F & (length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								if (isolate(o_cond$legend) == 1) {
									o_cond$legend <- 0
								}
								
								js$resetClick_leg()
							}
							else {
								if (isolate(o_cond$legend) == 0) {
									v_pos_1 <- grep("[(]normal curve[)]", names(input$traces)[v_pos_1])
									
									if (length(v_pos_1) > 0) {
										o_cond$legend <- 1
									}
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				click("update_button")
				o_cond$stat <- 2
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 3.26. Add events on mean spectrum check box
	# ===========================================
	
	observeEvent(input$mean_spect, {
		if (length(ls(e_data)) > 0) {
			if (input$mean_spect == T) {
				o_parameter$mean_spect <- T
				showNotification("The flag tab is disabled when a mean spectrum is added on IR spectra plot", duration = 15, type = "warning")
				
				if (isolate(o_click_button$display) == 1) {
					if (isolate(o_cond$legend) == 1) {
						js$reloadClick_leg()
						o_cond$legend <- 0
					}
				}
			}
			else {
				o_parameter$mean_spect <- F
				
				if (isolate(o_click_button$display) == 1) {
					if (is.list(input$traces)) {
						v_pos_1 <- which(as.vector(unlist(input$traces)) == "legendonly")
						
						if (length(v_pos_1) > 0) {
							v_pos_2 <- grep("[(]mean[)]", names(input$traces))
							
							if ((length(names(input$traces)) - length(v_pos_2)) == 1 & 1 %in% v_pos_1) {
								js$resetClick_leg()
							}
							else {
								v_pos_1 <- grep("[(]mean[)]", names(input$traces)[v_pos_1])
								
								if (length(v_pos_1) > 0) {
									o_cond$legend <- 1
								}
							}
						}
					}
				}
			}
			
			if (isolate(o_click_button$display) == 1) {
				click("update_button")
				o_cond$stat <- 1
				o_cond$display <- 1
				click("display_button")
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
			
			if (is.na(input$picture_height)) {
				o_picture_info$height <- 800 
			}
			else {
				o_picture_info$height <- input$picture_height
			}
			
			if (is.na(input$picture_width)) {
				o_picture_info$width <- 1000 
			}
			else {
				o_picture_info$width <- input$picture_width
			}
			
			click("update_button")
			o_cond$display <- 1
			click("display_button")
			toggleModal(session, "picture_info", toggle = "close")
		}
	})
	
	# 4.2. Add conditions on picture height/width inputs 
	# ==================================================
	
	observe({
		if (!is.numeric(input$picture_height) | input$picture_height < 100 | is.na(input$picture_height)) {
			if (!is.numeric(input$picture_height) | is.na(input$picture_height)) {
				updateNumericInput(session, "picture_height", value = 800)
			}
			else {
				updateNumericInput(session, "picture_height", value = 100)
			}
		}
		
		if (!is.numeric(input$picture_width) | input$picture_width < 300 | is.na(input$picture_width)) {
			if (!is.numeric(input$picture_width) | is.na(input$picture_width)) {
				updateNumericInput(session, "picture_width", value = 1000)
			}
			else {
				updateNumericInput(session, "picture_width", value = 300)
			}
		}
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
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = isolate(o_parameter$xlab), range = NULL), yaxis = list(title = isolate(o_parameter$ylab), range = NULL)))
					}
					
					if (isolate(o_parameter$plot_type) == "histplot") {
						ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = isolate(o_parameter$xlab), range = NULL), yaxis = list(title = "Density", range = NULL)))
					} 
				}
				else {
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(xaxis = list(title = "Frequency (cm-1)", rangeslider = list(thickness = 0.1, borderwidth = 1), range = rev(range(as.vector(isolate(e_data$code_freq$Frequency)))))))
				}
			}
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
				
				click("update_button")
				o_cond$display <- 1
				click("display_button")
				toggleModal(session, "lreg_info", toggle = "close")
			}
		}
	})
	
	# 4.5. Add events on linear regression informations "reset" button
	# ================================================================
	
	observeEvent(input$reset3_button, {
		if (isolate(o_click_button$display) == 1) {
			o_lreg_info$xpos <- 0
			o_lreg_info$ypos <- 1
			o_lreg_info$elt <- NA
			shinyjs::disable("reset3_button")
			
			click("update_button")
			o_cond$display <- 1
			click("display_button")
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
				if (!is.na(isolate(o_parameter$group)) & paste(isolate(o_parameter$model), input$select_graph, sep = "_") == "calib_QQplot") {
					o_cond$select_graph2 <- 1
				}
				
				o_parameter$select_graph <- input$select_graph
				click("update_button")
				o_cond$display <- 1
				click("display_button")
			}
		}
	})
	
	# 4.7. Add events on mouse click  
	# ==============================
	
	observeEvent(event_data("plotly_click", source = "graphic"), {	
		if (isolate(o_click_button$display) == 1) {
			o_click_ev <- event_data("plotly_click", source = "graphic")
			
			shinyjs::delay(100,
				if (input$data_type == "normal") {
					if (input$plot_type == "plot" & input$model == "none") {
						if ((o_click_ev[["curveNumber"]] + 1) <= isolate(o_plot$elt) & isolate(o_parameter$lreg) == F & isolate(o_parameter$conf_ellipsoid) == F & isolate(o_parameter$centroid) == F & input$f_radio == "no" & input$g_radio == "no" & input$h_radio == "no") {
							l_click_info <- f_create_click_info(s_data_type = "normal", s_dim_num = input$dim_num, v_xy_flag = input$var_flag_1, b_z_flag = input$var_flag_2, s_flag_name = isolate(o_flag$name), e_data = e_data, o_click_ev = o_click_ev, o_parameter = o_parameter, o_plot = NULL, o_cond = NULL)
							i_click <- 0
							
							if ("coord" %in% ls(e_current_flag)) {
								if (length(which(l_click_info[[3]] %in% e_current_flag$coord$id)) > 0) {
									i_click <- 1
								}
							}
							
							if (i_click == 0) {
								eval(parse(text = paste0("l_results <- f_create_current_flag_data(s_data_type = \"normal\", l_click_info = l_click_info, df_previous_flag = ", ifelse("flag" %in% ls(e_data), "e_data$flag", "NULL"), ", e_current_flag = e_current_flag)"))) # create current flag data
								
								if (!"message" %in% names(l_results)) {
									eval(parse(text = paste(paste0("e_current_flag$", names(l_results), " <- l_results[[", 1:length(l_results), "]]"), collapse = "; ")))
									
									ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "addTraces", f_add_current_flag("normal", input$dim_num, NULL, o_parameter, NULL, o_click_ev, e_current_flag$num)) # add current flag on the graph
									ply_1
									
									shinyjs::enable("clear1_button")
									shinyjs::enable("clear2_button")
									shinyjs::enable("save_button")
									
									if (e_current_flag$num == 1) {
										if (input$dim_num == "2d") {
											showNotification("The statistic options (linear regression, confidence ellipsoid, centroid) are disabled when new flags is added on the 2D plot", duration = 15, type = "warning")
											shinyjs::disable("lreg")
											shinyjs::disable("conf_ellipsoid")
											shinyjs::disable("centroid")
										}
										else {
											showNotification("The centroid checkbox is disabled when new flags is added on the 3D plot", duration = 15, type = "warning")
											shinyjs::disable("centroid")
										}
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
							df_all <- f_create_modal_data(isolate(o_plot$data), o_parameter, o_click_ev[["x"]], o_click_ev[["y"]])
							
							if (dim(df_all)[1] > 0 & o_click_ev[["x"]] != o_click_ev[["y"]]) {
								o_modal_plot$num <- 0
								o_modal_plot$x <- o_click_ev[["x"]]
								o_modal_plot$y <- o_click_ev[["y"]]
								
								showModal(modalDialog(
									title = paste0("cor = ", round(cor(df_all[, 1], df_all[, 2]), digits = 2), ", size = ", dim(df_all)[1]),
									easyClose = T,
									size = "m",
									renderPlotly({
										ply_1 <- f_build_modal_graph(df_all,  0, o_click_ev[["x"]], o_click_ev[["y"]])
										ply_1
									}),
									footer = tagList(actionButton("prev_button", icon("angle-left")), actionButton("next_button", icon("angle-right")), modalButton("Close"))
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
										
										if (length(v_pos) > 0) {
											i_qc <- as.vector(e_previous_flag$data$qc[v_x_pos[v_y_pos[v_pos]]]) 
										}
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
									if (length(which(e_current_flag$all_coord$x == s_x_click & e_current_flag$all_coord$var_name == isolate(o_plot$elt[o_click_ev[["curveNumber"]] + 1]))) > 0) {
										i_click <- 1
									}
								}
								
								if (i_click == 0) {
									l_click_info <- list(s_x_click, o_click_ev[["y"]], isolate(o_plot$elt[o_click_ev[["curveNumber"]] + 1]))
									eval(parse(text = paste0("l_results <- f_create_current_flag_data(\"temporal\", input$action, input$draw, isolate(o_parameter$x), l_click_info, ", ifelse("data" %in% ls(e_previous_flag), "e_previous_flag$data", "NULL"), ", e_current_flag, o_click_graph, o_plot)"))) # create current flag data
									
									if (length(l_results$message) == 0) {
										l_pos <- list(which(names(l_results) %in% c("num", "coord", "all_coord", "click_iter")), which(names(l_results) %in% c("prev_date", "prev_var")), which(names(l_results) %in% c("marker", "line")))
										
										if (sum(lengths(l_pos[1:2])) > 0) {
											eval(parse(text = paste(paste0(c(rep("e_current_flag", length(l_pos[[1]])), rep("o_click_graph", length(l_pos[[2]]))), "$", names(l_results)[c(l_pos[[1]], l_pos[[2]])], " <- l_results$", names(l_results)[c(l_pos[[1]], l_pos[[2]])]), collapse = "; ")))
										}
										
										l_temp_coord <- NULL
										
										if (length(l_pos[[3]]) > 0) {
											rm("click_iter", envir = e_current_flag)
											l_temp_coord <- list(l_results$marker, l_results$line)
										}
										
										ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "addTraces", f_add_current_flag("temporal", NULL, l_temp_coord, o_parameter, o_plot, o_click_ev, e_current_flag$num)) # add current flag on the graph
										ply_1
										
										eval(parse(text = paste(paste0("shinyjs::enable(\"", l_results$button_name, "\")"), collapse = "; ")))
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
						l_results <- f_create_click_info(s_data_type = "ir", s_action = input$action, o_click_ev = o_click_ev, o_parameter, o_plot = o_plot, o_cond = o_cond)
						
						if (l_results[[3]]) {
							i_click <- 0
							
							if ("coord" %in% ls(e_current_flag)) {
								if (length(which(l_results[[1]] %in% e_current_flag$coord$id)) > 0) {
									i_click <- 1
								}
							}
							else {
								showNotification("The mean spectrum checkbox in the Statistics tab is disabled when new flags is added on IR spectra plot", duration = 15, type = "warning")
								shinyjs::enable("clear1_button")
								shinyjs::enable("clear2_button")
								shinyjs::enable("save_button")
								shinyjs::disable("mean_spect")
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
			)
		}
	})
	
	# 4.8. Add events on zoom 
	# =======================
	
	observeEvent(event_data("plotly_relayout", source = "graphic"), {
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
									o_zoom$coord <- rev(range(as.vector(isolate(e_data$code_freq$Frequency))))
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
										o_zoom$coord <- rev(range(as.vector(isolate(e_data$code_freq$Frequency))))
									}
								}
							}
							else {
								if (input$data_type == "temporal") {
									o_zoom$coord <- NULL
								}
								else {
									o_zoom$coord <- rev(range(as.vector(isolate(e_data$code_freq$Frequency))))
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
							eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data$all, o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", df_click_legend, input$fraction, NULL)")))
							o_plot$y_coord <- l_results[[2]]
							ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = isolate(o_parameter$ylab), range = l_results[[1]])))
							
							if (length(l_results[[3]]) > 0) {
								showNotification(l_results[[3]], duration = 15, type = "warning")
							}
							
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
				if (isolate(o_cond$legend) == 1) {
					o_cond$legend <- 0
				}
			}
			else {
				if (input$y_scale == "auto") {
					df_click_legend <- isolate(o_click_legend$item)
					
					if (is.list(input$traces)) {
						v_name <- names(input$traces)[which(as.vector(unlist(input$traces)) == "legendonly")]
						df_click_legend$statut <- ifelse(df_click_legend$name %in% v_name, "\"legendonly\"", "T")
					}
					
					o_plot$y_coord <- NULL
					eval(parse(text = paste0("l_results <- f_calcul_y_axis_range(input$data_type, \"auto\", e_data$all, o_parameter, o_zoom, o_plot, ", ifelse(input$data_type == "temporal", "o_cond", "NULL"), ", df_click_legend, input$fraction, NULL)")))
					o_plot$y_coord <- l_results[[2]]
					ply_1 <- plotlyProxyInvoke(p = plotlyProxy("graphic", session), "relayout", list(yaxis = list(title = isolate(o_parameter$ylab), range = l_results[[1]])))
					
					if (length(l_results[[3]]) > 0) {
						showNotification(l_results[[3]], duration = 15, type = "warning")
					}
					
					rm(list = "l_results")
				}
			}
		}
	})
	
	# 4.10. Add events on previous/next buttons (modal dialog: plotly_click)
	# =========================================
	
	observeEvent(input$prev_button, {
		o_modal_plot$num <- isolate(o_modal_plot$num) - 1 
		
		if (isolate(o_modal_plot$num) == (-1)) { 
			o_modal_plot$num <- 3
		}
		
		df_all <- f_create_modal_data(isolate(o_plot$data), o_parameter, isolate(o_modal_plot$x), isolate(o_modal_plot$y))
		
		showModal(modalDialog(
			title = paste0("cor = ", round(cor(df_all[, 1], df_all[, 2]), digits = 2), ", size = ", dim(df_all)[1]),
			easyClose = T,
			size = "m",
			renderPlotly({
				ply_1 <- f_build_modal_graph(df_all,  isolate(o_modal_plot$num), isolate(o_modal_plot$x), isolate(o_modal_plot$y))
				ply_1
			}),
			footer = tagList(actionButton("prev_button", icon("angle-left")), actionButton("next_button", icon("angle-right")), modalButton("Close"))
		))
	})
	
	observeEvent(input$next_button, {
		o_modal_plot$num <- isolate(o_modal_plot$num) + 1 
		
		if (isolate(o_modal_plot$num) == 4) { 
			o_modal_plot$num <- 0
		}
		
		df_all <- f_create_modal_data(isolate(o_plot$data), o_parameter, isolate(o_modal_plot$x), isolate(o_modal_plot$y))
		
		showModal(modalDialog(
			title = paste0("cor = ", round(cor(df_all[, 1], df_all[, 2]), digits = 2), ", size = ", dim(df_all)[1]),
			easyClose = T,
			size = "m",
			renderPlotly({
				ply_1 <- f_build_modal_graph(df_all,  isolate(o_modal_plot$num), isolate(o_modal_plot$x), isolate(o_modal_plot$y))
				ply_1
			}),
			footer = tagList(actionButton("prev_button", icon("angle-left")), actionButton("next_button", icon("angle-right")), modalButton("Close"))
		))
	})
	
	# ===
	# End
	# ===
	
	# Close session
	# =============
	
	session$onSessionEnded(function() {stopApp()})
}


# III) Run the shiny application
# ==============================

o_app <- shinyApp(o_ui, f_server)
runApp(o_app, launch.browser = T)
