#' @importFrom shiny fluidPage fluidRow column wellPanel tabsetPanel tabPanel textInput numericInput selectizeInput actionButton radioButtons checkboxInput checkboxGroupInput icon textAreaInput
#' @importFrom shinythemes shinytheme
#' @importFrom shinyjs useShinyjs extendShinyjs disabled hidden
#' @importFrom htmltools tags HTML br div img h4 h5 strong
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyBS bsModal bsButton bsTooltip 
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom plotly plotlyOutput
NULL

#' Creating the User's Interface (UI)

#' @description
#' `f_ui` returns the WIDEa UI

#' @encoding UTF-8

f_ui <- function() {
	# Creating Operating System parameter data
	s_OS_name <- Sys.info()[[1]]
	df_UI_param <- data.frame(
		"Parameters" = c(
			"lp_text_input", "lp_multi_rb_top_margin", "lp_button_gap",
			"multi_rb_margin_after", "tp_multi_rb_top_margin_1",
			"tp_multi_rb_top_margin_2", "tp_multi_rb_top_margin_3", "tp_multi_rb_right_margin_1", "tp_multi_rb_right_margin_2", "tp_multi_rb_right_margin_3", "tp_gap_1", "tp_gap_2"
		), 
		"Windows" = c(244, 0, 0, 0, 0, 10, 0, -22, -3, -42, 0, 0.7),
		"Darwin" = c(260, 6, 1.05, 3, 3, 12, 10, 0, 0, 0, 1.5, 0.2),
		"Linux" = c(260, 6, 1.05, 3, 3, 12, 10, 0, 0, 0, 1.5, 0.2)
	)
	
	# Build WIDEa User's Interface
	o_ui <- fluidPage(theme = shinytheme("flatly"),
		useShinyjs(),
		
		extendShinyjs(
			text = 'shinyjs.backgroundCol = function(params) {
				var defaultParams = {id : null, col : "white"};
				params = shinyjs.getParams(params, defaultParams);
				var el = $("#" + params.id);
				el.css("background-color", params.col);
			}', 
			functions = c("backgroundCol")
		), # change the background color of an element
		
		extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-graphic', 'null'); }", functions = c("resetClick")), # reset click on graph
		tags$style("select ~ .selectize-control .selectize-input {max-height: 36px; overflow-y: auto;}"), # selectize input parameters (UI)
		
		# Multi-column checkbox/radiobutton class (UI) 
		tags$head(tags$style(HTML('.multicol1 .shiny-options-group{-webkit-column-count: 1; -moz-column-count: 1; column-count: 1; -moz-column-fill: balanced; -column-fill: balanced;}'))),
		tags$head(tags$style(HTML('.multicol2 .shiny-options-group{-webkit-column-count: 2; -moz-column-count: 2; column-count: 2; -moz-column-fill: balanced; -column-fill: balanced;}'))),
		tags$head(tags$style(HTML('.multicol3 .shiny-options-group{-webkit-column-count: 3; -moz-column-count: 3; column-count: 3; -moz-column-fill: balanced; -column-fill: balanced;}'))),
		
		tags$head(tags$style(HTML('.rangeslider-grabber-min {display: none; !important} .rangeslider-grabber-max {display: none; !important}'))), # remove rangeslider grabber for X axis (temporal/IR) 
		tags$head(tags$style(HTML('#shiny-notification-panel{width: 500px;}'))), # notification panel width
		
		tags$head(tags$script(
			'var dimension = [0, 0];
			$(document).on("shiny:connected", function(e) {
				dimension[0] = window.innerWidth;
				dimension[1] = window.innerHeight;
				Shiny.onInputChange("dimension", dimension);
			});
			$(window).resize(function(e) {
				dimension[0] = window.innerWidth;
				dimension[1] = window.innerHeight;
				Shiny.onInputChange("dimension", dimension);
			});'
		)), # resize graph window
		
		# Add a loading spinner
		add_busy_spinner(spin = "double-bounce", color = "#FFFFFF", position = "top-left", timeout = 100, margins = c(0, 70), height = "38px", width = "55px"),
		
		# Style class of top-sidebar and main panel 
		shinyjs::inlineCSS(list(.tpanel_class1 = "overflow: auto; position: fixed; margin-top: 40px; margin-left: 300px; height: 220px; top: 0; left: 0; bottom: 0; right: 0;")),
		shinyjs::inlineCSS(list(.tpanel_class2 = "overflow: auto; position: fixed; margin-top: 40px; height: 220px; top: 0; left: 0; bottom: 0; right: 0;")),
		shinyjs::inlineCSS(list(.mainpanel_class1 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 260px; margin-left: 300px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 1 (left panel = show, top panel = show, top main panel: hide)
		shinyjs::inlineCSS(list(.mainpanel_class2 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 260px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 2 (left panel = hide, top panel = show, top main panel: hide)
		shinyjs::inlineCSS(list(.mainpanel_class3 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 40px; margin-left: 300px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 3 (left panel = show, top panel = hide, top main panel: hide)
		shinyjs::inlineCSS(list(.mainpanel_class4 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 40px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 4 (left panel = hide, top panel = hide, top main panel: hide)
		
		shinyjs::inlineCSS(list(.mainpanel_class5 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 315px; margin-left: 300px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 5 (left panel = show, top panel = show, top main panel: show)
		shinyjs::inlineCSS(list(.mainpanel_class6 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 315px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 6 (left panel = hide, top panel = show, top main panel: show)
		shinyjs::inlineCSS(list(.mainpanel_class7 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 95px; margin-left: 300px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 7 (left panel = show, top panel = hide, top main panel: show)
		shinyjs::inlineCSS(list(.mainpanel_class8 = "background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 95px; margin-bottom: 0px; top: 0; left: 0; bottom: 0; right: 0;")), # main panel class 8 (left panel = hide, top panel = hide, top main panel: show)
		
		shinyjs::inlineCSS(list(.tmainpanel_class1 = "z-index:100; background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 260px; margin-left: 300px; height: 55px; top: 0; left: 0; bottom: 0; right: 0;")), # top main panel class 1 (left panel = show, top panel = show)
		shinyjs::inlineCSS(list(.tmainpanel_class2 = "z-index:100; background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 260px; height: 55px; top: 0; left: 0; bottom: 0; right: 0;")), # top main panel class 2 (left panel = hide, top panel = show)
		shinyjs::inlineCSS(list(.tmainpanel_class3 = "z-index:100; background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 40px; margin-left: 300px; height: 55px; top: 0; left: 0; bottom: 0; right: 0;")), # top main panel class 3 (left panel = show, top panel = hide)
		shinyjs::inlineCSS(list(.tmainpanel_class4 = "z-index:100; background: #FFFFFF; position: fixed; border: none; border-radius: 0px; margin-top: 40px; height: 55px; top: 0; left: 0; bottom: 0; right: 0;")), # top main panel class 4 (left panel = hide, top panel = hide)
		
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
				column(12, selectizeInput('picture_format', 'Format:', c('png', 'jpeg', 'svg'), 'png'))
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
		
		tags$head(tags$style(HTML('table.dataTable tbody tr.selected td {color: white !important; box-shadow: inset 0 0 0 9999px #B0BEC5 !important;}'))), # change datatable selected row color
		tags$head(tags$style(HTML('table.dataTable tbody tr:hover {color: white !important; background-color: #EFF2F3 !important;}'))), # change datatable hover row color
		
		div(
			style = "display:flex; align-items:flex-start",
			
			# Hide/show panel
			# ---------------
			
			wellPanel( 
				style = "background: #367BB4FF; overflow: hidden; position: fixed; border: none; border-radius: 0px; height: 40px; top: 0; left: 0; bottom: 0; right: 0;",
				div(style = "position: absolute; top: 0.36em; left: 0.3em;", img(src = "www/WIDEa_header_img.png", width = "25%")),
				div(style = "color: white; position: absolute; top: 0em; right: 8.9em;", h5("Hide/show panel:")),
				div(style = "position: absolute; top: 0.4em; right: 6.4em;", actionButton("hs_lpanel_button", "Left", style='padding: 0px; font-size: 100%')),
				div(style = "position: absolute; top: 0.4em; right: 3.95em;", actionButton("hs_tpanel_button", "Top", style='padding: 0px; font-size: 100%')),
				div(style = "position: absolute; top: 0.4em; right: 1em;", actionButton("hs_bpanel_button", "Both", style='padding: 0px; font-size: 100%'))
			),
			
			# Left panel
			# ----------
			
			tags$head(tags$style(HTML('#data_path1{background-color: white;}'))),
			tags$head(tags$style(HTML('#vvalue2 {height: 35px}'))),
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
				
				# Sub-data creation
				
				div(style = "position: absolute; top: 28.5em; left: 1.25em; color: #367BB4FF;", h4("Sub-data creation")),
				
				div(style = "position: absolute; top: 30.8em; left: 1.25em;", disabled(checkboxInput('subdata_option', 'Add conditions', F))),
				div(style = "position: absolute; top: 33.8em; left: 1.25em;", disabled(selectizeInput("vname", "Variable name:", choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
				div(style = "position: absolute; top: 38.8em; left: 1.25em;", disabled(radioButtons("vtype", "Variable type:", choices = c(Quantitative = "quant", Qualitative = "qualit"), selected = "qualit", inline = T))),
				div(style = "position: absolute; top: 43.3em; left: 1.25em;", strong("Relation symbol:")),
				div(style = "position: absolute; top: 42.9em; left: 9.55em;", disabled(selectizeInput("rel_symbol", NULL, choices = " ", width = "120px"))),
				div(style = "position: absolute; top: 46em; left: 1.25em;", strong("Value:")),
				div(style = paste0("position: absolute; top: 46.26em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del1_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 47.85em; left: 1.25em;", disabled(selectizeInput("vvalue1", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				div(style = "position: absolute; top: 47.85em; left: 1.25em;", hidden(disabled(textInput("vvalue2", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"))))),
				div(style = paste0("position: absolute; top: 50.9em; left: ", 6 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton("c_info_clear_button", "Info/Clear"))),
				div(style = paste0("position: absolute; top: 50.9em; left: ", 13.4 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton("c_add_button", "Add"))),
				div(style = "position: absolute; top: 55.05em; left: 1.25em;", strong("Condition formula:")),
				div(style = paste0("position: absolute; top: 55.6em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand1_button', NULL, icon = icon("expand-alt"), class = "expand"))),
				div(style = "position: absolute; top: 57.2em; left: 1.25em;", disabled(textInput("c_formula", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				div(style = paste0("position: absolute; top: 60.9em; left: ", 12.2 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(bsButton("create_button", "Create"))),
				
				# Normal plot selection
				div(style = "position: absolute; top: 64.96em; left: 1.25em; color: #367BB4FF;", h4("Normal plot selection")),
				
				div(style = "position: absolute; top: 67.96em; left: 1.25em;", strong("Type:")),
				div(style = paste0("position: absolute; top: ", 69.96 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_2"), s_OS_name]), "em; left: 1.25em;"), align = 'left', class = 'multicol3', disabled(radioButtons("plot_type", NULL, choices = c("Plot" = "plot", "Boxplot" = "boxplot", "Histplot" = "histplot", "Barplot" = "barplot", "Corplot" = "corplot"), selected = "plot", inline = F))),
				
				div(style = "position: absolute; top: 74.96em; left: 1.25em;", disabled(radioButtons("dim_num", "Dimension number:", choices = c("2" = "2d", "3" = "3d"), selected = "2d", inline = T))),
				
				div(style = "position: absolute; top: 79.46em; left: 1.25em;", strong("Model:")),
				div(style = paste0("position: absolute; top: ", 81.46 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_2"), s_OS_name]), "em; left: 1.25em;"), align = 'left', class = 'multicol2', disabled(radioButtons("model", NULL, choices = c(None = "none", Calibration = "calib", Validation = "valid"), selected = "none", inline = F))),
				
				# Model parameter loading
				div(style = "position: absolute; top: 86.46em; left: 1.25em; color: #367BB4FF;", h4("Model parameter loading")),
				
				div(style = "position: absolute; top: 89.46em; left: 1.25em;", strong("Select model parameter:")),
				div(style = "position: absolute; top: 91.96em; left: 1.25em;", disabled(textInput('data_path3', NULL, '', width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				div(style = paste0("position: absolute; top: 94.96em; left: ", 6.9 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(shinyFilesButton("browse3_button", "Browse", "Please select a file", F))),
				div(style = paste0("position: absolute; top: 94.96em; left: ", 13.1 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('load3_button', 'Load'))),
				
				# Variable selection
				div(style = "position: absolute; top: 99.1em; left: 1.25em; color: #367BB4FF;", h4("Variable selection")),
				
				div(style = "position: absolute; top: 102.1em; left: 1.25em;", disabled(radioButtons("id", "ID:", choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = "position: absolute; top: 106.1em; left: 1.25em;", disabled(selectizeInput("var_id", label = NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
				
				div(style = "position: absolute; top: 109.5em; left: 1.25em;", strong("Random:")),
				div(style = "position: absolute; top: 109.41em; left: 5.7em;", disabled(radioButtons("ref_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = paste0("position: absolute; top: 109.9em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del2_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 111.51em; left: 1.25em;", disabled(selectizeInput("ref", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				
				div(style = "position: absolute; top: 115em; left: 1.25em;", strong("X:")),
				div(style = "position: absolute; top: 116em; left: 1.25em;", disabled(checkboxInput('concat1', 'Concatenation', F))),
				div(style = paste0("position: absolute; top: 117.01em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del3_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 118.6em; left: 1.25em;", disabled(selectizeInput("var_x", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				div(style = "position: absolute; top: 121.39em; left: 1.25em;", strong("f(x):")),
				div(style = "position: absolute; top: 121.3em; left: 3.5em;", disabled(radioButtons("f_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = paste0("position: absolute; top: 121.6em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand2_button', NULL, icon = icon("expand-alt"), class = "expand"))),
				div(style = "position: absolute; top: 123.2em; left: 1.25em;", disabled(textInput("f_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				div(style = "position: absolute; top: 127.15em; left: 1.25em;", strong("format:")),
				div(style = "position: absolute; top: 126.8em; left: 5.45em;", disabled(selectizeInput("date_format", label = NULL, choices = " ", width = '180px'))),
				
				div(style = "position: absolute; top: 130.4em; left: 1.25em;", strong("Y:")),
				div(style = paste0("position: absolute; top: 130.66em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del4_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 132.25em; left: 1.25em;", disabled(selectizeInput("var_y", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				div(style = "position: absolute; top: 135.04em; left: 1.25em;", strong("g(y):")),
				div(style = "position: absolute; top: 134.95em; left: 3.5em;", disabled(radioButtons("g_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = paste0("position: absolute; top: 135.45em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand3_button', NULL, icon = icon("expand-alt"), class = "expand"))),
				div(style = "position: absolute; top: 137.05em; left: 1.25em;", disabled(textInput("g_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				
				div(style = "position: absolute; top: 141.15em; left: 1.25em;", strong("Z:")),
				div(style = paste0("position: absolute; top: 141.41em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del5_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 143em; left: 1.25em;", disabled(selectizeInput("var_z", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				div(style = "position: absolute; top: 145.79em; left: 1.25em;", strong("h(z):")),
				div(style = "position: absolute; top: 145.7em; left: 3.5em;", disabled(radioButtons("h_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = paste0("position: absolute; top: 146.2em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand4_button', NULL, icon = icon("expand-alt"), class = "expand"))),
				div(style = "position: absolute; top: 147.8em; left: 1.25em;", disabled(textInput("h_text", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				
				div(style = "position: absolute; top: 152.14em; left: 1.25em;", strong("Weighted residuals:")),
				div(style = "position: absolute; top: 152.05em; left: 10.6em;", disabled(radioButtons("wres_radio", NULL, choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = "position: absolute; top: 153.35em; left: 1.25em;", disabled(checkboxInput('wres_cbox', 'with groups:', F))),
				div(style = paste0("position: absolute; top: 154.34em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del6_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 155.95em; left: 1.25em;", disabled(selectizeInput("wres_group", NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999, maxItems = 9999)))),
				div(style = "position: absolute; top: 158.51em; left: 1.25em;", strong("variance function:")),
				div(style = paste0("position: absolute; top: 158.96em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('expand5_button', NULL, icon = icon("expand-alt"), class = "expand"))),
				div(style = "position: absolute; top: 160.56em; left: 1.25em;", disabled(textInput("wres_vfun", NULL, character(0), width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px")))),
				
				div(style = "position: absolute; top: 164.7em; left: 1.25em;", disabled(radioButtons("group", "Group:", choices = c(Yes = "yes", No = "no"), selected = "no", inline = T))),
				div(style = "position: absolute; top: 167.7em; left: 1.25em;", disabled(checkboxInput('concat2', 'Concatenation', F))),
				div(style = paste0("position: absolute; top: 168.71em; left: ", 14.85 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('del7_button', NULL, icon = icon("trash-alt"), class = "del"))),
				div(style = "position: absolute; top: 170.3em; left: 1.25em;", disabled(selectizeInput("var_group", label = NULL, choices = " ", width = paste0(as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_text_input"), s_OS_name]), "px"), options = list(maxOptions = 9999)))),
				
				div(style = paste0("position: absolute; top:174.45em; left: ", 6.6 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('graph_clear_button', 'Clear'))),
				div(style = paste0("position: absolute; top:174.45em; left: ", 11.95 + as.vector(df_UI_param[which(df_UI_param$Parameters == "lp_button_gap"), s_OS_name]), "em;"), disabled(actionButton('display_button', 'Display')))
			),
			
			# Top panel
			# ---------
			
			tags$head(tags$style(HTML(paste0("#mode .radio{margin-left: 0px; margin-right: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_top_margin_1"), s_OS_name]), "px !important; -webkit-margin-after: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "multi_rb_margin_after"), s_OS_name]), "px !important;} #mode>*{margin-bottom: 15px;}")))),
			tags$head(tags$style(HTML(paste0("#bw_radio .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_1"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #bw_radio>*{margin-bottom: 15px;}")))),
			tags$head(tags$style(HTML(paste0("#y_scale .radio{margin-left: 0px; margin-top: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_top_margin_2"), s_OS_name]), "px; margin-bottom: 25px;} #y_scale>*{margin-bottom: 6px;}")))),
			tags$head(tags$style(HTML(paste0("#dec_num_radio .radio{margin-left: 0px; margin-right: ", as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_multi_rb_right_margin_1"), s_OS_name]), "px; margin-top: 0px !important; -webkit-margin-after: 0px !important;} #dec_num_radio>*{margin-bottom: 15px;}")))),
			tags$head(tags$style(HTML('#fraction {height: 42px;}'))),
			tags$head(tags$style(HTML('#ymin {height: 35px}'))),
			tags$head(tags$style(HTML('#ymax {height: 35px;}'))),
			tags$head(tags$style(HTML('#edit_option_button {margin-right: 10px;}'))),
			
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
						div(style = "position: absolute; top: 7.4em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("mode", "Mode:", choices = c("Line+point" = "line_marker", "Line" = "line", "Point" = "marker"), selected = "marker", inline = F))),
						div(style = "position: absolute; top: 13.4em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("dec_num_radio", "Decimal number:", choices = c("Auto" = "auto", "Manual:" = "manual"), selected = "auto", inline = F))),
						div(style = paste0("position: absolute; top: 13em; left: ", 12 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(numericInput("dec_num", "", numeric(0), min = 0, step = 1, width = "100px"))),
						div(style = paste0("position: absolute; top: 14.65em; left: ", 19.4 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("dec_num_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
						div(style = "position: absolute; top: 17.9em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("bw_radio", "Bin width:", choices = c("Auto" = "auto", "Manual:" = "manual"), selected = "auto", inline = F))),
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
						div(style = paste0("position: absolute; top: 12.4em; left: ", 41 + as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("y_scale_button", NULL, icon = icon("angle-double-right"), style='padding:5px; font-size:100%'))),
						
						div(style = paste0("position: absolute; top: 3.5em; left: ", 46.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(selectizeInput("edit_option", label = "Option:", choices = " ", width = '180px'))),
						div(style = paste0("position: absolute; top: 4.85em; left: ", 59.5 + 2 * as.vector(df_UI_param[which(df_UI_param$Parameters == "tp_gap_1"), s_OS_name]), "em;"), disabled(actionButton("edit_option_button", "Edit")))
					),
					
					# Flag parameters
					
					tabPanel(title = "Flag",
						div(style = "position: absolute; top: 3.5em; left: 2em;", align = 'left', class = 'multicol2', disabled(radioButtons("action", "Action:", choices = c("Add new flags" = "add_flag", "Replace qc = 1 with 2" = "replace_qc"), selected = "add_flag", inline = F))),
						div(style = "position: absolute; top: 7.5em; left: 2em;", align = 'left', class = 'multicol2', disabled(checkboxGroupInput("var_flag_1", "Variable:", choices = c("X" = "flag_x", "Y" = "flag_y"), inline = F))),
						div(style = "position: absolute; top: 8.57em; left: 7.85em;", disabled(checkboxInput("var_flag_2", "Z", F))),
						div(style = "position: absolute; top: 7.5em; left: 12em;", align = 'left', class = 'multicol2', disabled(radioButtons("draw", "Draw:", choices = c(Interval = "mpt", Point = "pt"), selected = "pt", inline = F))),
						div(style = "position: absolute; top: 7.5em; left: 23.5em;", align = 'left', class = 'multicol2', disabled(radioButtons("qc", "Quality", choices = c("1" = "1", "2" = "2"), selected = "2", inline = F))),
						div(style = "position: absolute; top: 7.5em; left: 27.2em;", strong("code:")),
						div(style = "position: absolute;top: 7.5em; left: 32.5em;", disabled(textAreaInput('comment', 'Commentary:', '', height = '60px', width = '388px', resize = "none"))),
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
			
			
			# Top main panel
			# --------------
			
			hidden(wellPanel(id = "tmainpanel",
				class = "tmainpanel_class1",
				div(style = "position: absolute; top: 0.5em; left: 0.5em;", selectizeInput("select_graph", label = NULL, choices = " ", width = "300px"))
			)),
			
			# Main panel
			# ----------
			
			tags$head(tags$style(HTML(".select_leg {background-color: #01796F; border: none; color: #FFFFFF; width: 15px; padding: 3px; font-size: 60%;}
				.select_leg.disabled:hover, .select_leg.disabled:focus {background-color: #01796F;}
				.select_leg:hover, .select_leg:focus, .select_leg.focus, .select_leg:active:focus, .select_leg.active:focus, .select_leg:active.focus, .select_leg.active.focus {background-color: #004953;}"
			))),
			tags$head(tags$style(HTML(".deselect_leg {background-color: #F8412C; border: none; color: #FFFFFF; width: 15px; padding: 3px; font-size: 60%;}
				.deselect_leg.disabled:hover, .deselect_leg.disabled:focus {background-color: #F8412C;}
				.deselect_leg:hover, .deselect_leg:focus, .deselect_leg.focus, .deselect_leg:active:focus, .deselect_leg.active:focus, .deselect_leg:active.focus, .deselect_leg.active.focus {background-color: #FF0000;}"
			))),
			
			tags$head(tags$style(HTML('#select_leg_button {margin-left: 10px;}'))),
			tags$head(tags$style(HTML('#deselect_leg_button {margin-left: 10px;}'))),
			tags$head(tags$style(HTML('#popup1_button {margin-left: 10px;}'))),
			tags$head(tags$style(HTML('#reset2_button {margin-left: 10px;}'))),
			tags$style(HTML(".tooltip > .tooltip-inner {color: white; background-color: #5F6D7A;}")),
			
			wellPanel(id = "mainpanel",
				class = "mainpanel_class1",
				div(style = "position: absolute; top: 0em; left: 0em; right: 0em; bottom: 0em;", plotlyOutput("graphic")),
				div(style = "position: absolute; top: 2em; left: 0em;", 
					hidden(div(id = "sh_select_leg", actionButton('select_leg_button', NULL, icon = icon("list-ul"), class = "select_leg"))),
					bsTooltip("select_leg_button", "Select all legend items", placement = "right", options = list(container = "body"))
				),
				div(style = "position: absolute; top: 3.5em; left: 0em;", 
					hidden(div(id = "sh_deselect_leg", actionButton('deselect_leg_button', NULL, icon = icon("list-ul"), class = "deselect_leg"))),
					bsTooltip("deselect_leg_button", "Deselect all legend items", placement = "right", options = list(container = "body"))
				),
				div(style = "position: absolute; top: 0em; left: 0em;",
					hidden(div(id = "sh_popup1", style = "display: inline;", actionButton('popup1_button', NULL, icon = icon("plus"), style='padding:0px; font-size:60%'))),
					bsTooltip("popup1_button", "Change picture details", placement = "bottom", options = list(container = "body")),
					hidden(div(id = "sh_reset", style = "display: inline;", actionButton('reset2_button', NULL, icon = icon("home"), style='padding:0px; font-size:60%'))),
					bsTooltip("reset2_button", "Reset axes", placement = "bottom", options = list(container = "body")),
					hidden(div(id = "sh_popup2", style = "display: inline;", style = "display: inline;", actionButton('popup2_button', NULL, icon = icon("edit"), style='padding:0px; font-size:60%'))),
					bsTooltip("popup2_button", "Add linear regression information in the graph", placement = "bottom", options = list(container = "body"))
				)
			)
		)
	)
	
	return(o_ui)
}
