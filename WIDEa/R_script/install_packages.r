###########################################################################
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
# Description: Downloading/installing of R packages used by WIDEa
#
# Creation date: August 2019
###########################################################################


if (length(list.files(Sys.getenv("R_LIBS_USER")))==0) {
   dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
}

if (!"shiny" %in% rownames(installed.packages())) {
   install.packages("shiny", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"shinyBS" %in% rownames(installed.packages())) {
   install.packages("shinyBS", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"shinyjs" %in% rownames(installed.packages())) {
   install.packages("shinyjs", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"shinyFiles" %in% rownames(installed.packages())) {
   install.packages("shinyFiles", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"shinythemes" %in% rownames(installed.packages())) {
   install.packages("shinythemes", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"shinybusy" %in% rownames(installed.packages())) {
   install.packages("shinybusy", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"V8" %in% rownames(installed.packages())) {
   Sys.setenv(DOWNLOAD_STATIC_LIBV8=1)
   install.packages("V8", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"plotly" %in% rownames(installed.packages())) {
   install.packages("plotly", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"htmltools" %in% rownames(installed.packages())) {
   install.packages("htmltools", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"htmlwidgets" %in% rownames(installed.packages())) {
   install.packages("htmlwidgets", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"bindrcpp" %in% rownames(installed.packages())) {
   install.packages("bindrcpp", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"scales" %in% rownames(installed.packages())) {
   install.packages("scales", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"data.table" %in% rownames(installed.packages())) {
   install.packages("data.table", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"arrangements" %in% rownames(installed.packages())) {
   install.packages("arrangements", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"car" %in% rownames(installed.packages())) {
   install.packages("car", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"DT" %in% rownames(installed.packages())) {
   install.packages("DT", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}

if (!"colourpicker" %in% rownames(installed.packages())) {
   install.packages("colourpicker", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")
}
