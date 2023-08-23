#########################################################################################################
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
# Description : Inventory of statistical methods (Statistics tab) available for all data types
#
# Creation date : January 2023
#########################################################################################################

# Dataframe with 4 columns
# ------------------------
# data_type: data type
# name: method name
# check_process: value used to define which methods have (= 1) or not (= 0) preliminary checks and which methods have returned an error after preliminary checks (= -1)
# message: method name returned by a warning/error message (corresponding to preliminary checks)
# code: code associated to preliminary checks applied on statistical methods. Statistical methods with a same code have same preliminary checks.
# click: binary value updated when a method is added/removed (check boxes in Statistics tab) in the current graph

df_stat_method_inv_ini <- data.frame(
	"data_type" = c(
		rep("normal", 5),
		"ir"
	),
	"name" = c(
		"lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve", 
		"mean_spect"
	),
	"leg_name" = c(
		"lreg", "ellipsoid", "centroid", "curve", "normal curve", 
		"mean"
	),
	"check_process" = c(
		rep(1, 2), 0, rep(1, 2),
		0
	),
	"message" = c(
		"linear regression", "confidence ellipsoid", NA, "density curve", "normal density curve", 
		NA
	),
	"code" = c(
		rep("cp1", 2), NA, rep("cp2", 2),
		NA
	)
)

df_stat_method_inv_ini$click <- 0

# List of methods with preliminary checks
# ---------------------------------------

# information about (Group) levels added in the graph legend after preliminary checks have been applied 
eval(parse(text = paste0("l_stat_method_level_ini <- list(", paste(paste0(as.vector(sort(unique(df_stat_method_inv_ini$code[!is.na(df_stat_method_inv_ini$code)]))), " = c()"), collapse = ", "), ")")))

# information (code: 1, 0 or -1) associated to a warning/error message. The warning/error message is used to inform users about (Group) levels which preliminary checks failed.
# 0 (initial value) means no preliminary checks have been executed for selected statistical methods. The code is equal to 1 (-1 resp.) if a (no resp.) message must be displayed
# after preliminary checks.  
eval(parse(text = paste0("l_stat_method_message_ini <- list(", paste(paste0(df_stat_method_inv_ini[which(df_stat_method_inv_ini$check_process == 1), "name"], " = 0"), collapse = ", "), ")")))
