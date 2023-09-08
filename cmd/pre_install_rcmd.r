rm(list = ls(all = TRUE))
options(warn = -1)

# Creating the R library directory (if it doesn't exist)
if(length(list.files(Sys.getenv("R_LIBS_USER")))==0) {dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)}

# Installing the devtools package (if it doesn't exist)
# The devtools package is used to install WIDEa from the github repository
if(!require(devtools)){install.packages("devtools", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu")}