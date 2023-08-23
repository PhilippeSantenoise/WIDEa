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
# Description: launching WIDEa
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
library(DT)

library(bindrcpp)

library(colourpicker) # add shiny colorInput
library(scales) # color brewer

library(data.table)
library(arrangements)

library(car) # levene test


#===================================================================================
# Loading R scripts
#===================================================================================

v_rscript_name <- c("WIDEa_ui", "WIDEa_server", "utils", paste0(c("on_off", "update"), "_ui_inputs"), "input_inventories", "stat_method_inventory", "create_stat_method_message", "update_reactive_values", "build_modal_dialog", "load_data", "create_sub_data")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/", v_rscript_name, ".r\")"), collapse = "; ")))

# Display button (lp)
# --------------

# Checking process:
v_rscript_name <- c("fields", "variables", "trsf_variables", "model")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/data/checking_process/check_", v_rscript_name, ".r\")"), collapse = "; ")))

# Data:
v_rscript_name <- c(paste0("create_", c("date", "concat"), "_variable"), "prepare_data", "create_model_data")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/data/", v_rscript_name, ".r\")"), collapse = "; ")))

# Graph:
v_rscript_name <- c("check_tp1_inputs", "create_graph_option", "create_element_data", "build_legend_items", "edit_axis_layout", "calculate_y_axis_range", "build_graph", "add_flag")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/graph/", v_rscript_name, ".r\")"), collapse = "; ")))

# Statistics:
v_rscript_name <- c("lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve", "mean_spect")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/display_button/graph/statistics/add_", v_rscript_name, ".r\")"), collapse = "; ")))

# Save button (tp2)
# -----------

v_rscript_name <- c("create_flag_data_name", "save_current_flag_data")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/save_button/", v_rscript_name, ".r\")"), collapse = "; ")))

# Plotly click (main panel)
# ------------

v_rscript_name <- c("create_click_info", "create_current_flag_data", "add_current_flag")
eval(parse(text = paste(paste0("source(\"", s_WIDEa_path, "R_script/plotly_click/", v_rscript_name, ".r\")"), collapse = "; ")))


#===================================================================================
# Run the shiny application
#===================================================================================

f_widea <- function () {
	o_app <- shinyApp(f_ui, f_server)
	return(runApp(o_app, launch.browser = T))
}

f_widea()
