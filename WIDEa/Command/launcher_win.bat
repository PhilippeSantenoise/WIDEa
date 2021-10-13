@echo off

set "current_path=%~dp0"
set WIDEa_path=%current_path:~0,-8%
set WIDEa_path_new=%WIDEa_path:\=/%
set "WIDEa_files_path=C:\Users\%USERNAME%\Documents\WIDEa files"
set "R_software_txt_file_path=%WIDEa_files_path%\Rexe_path.txt"

setlocal enableDelayedExpansion

set /p R_software_path=<"%R_software_txt_file_path%"

set "WIDEa_launch_Rscript=R_script/WIDEa_launcher.r"
set "WIDEa_launch_Rscript_path=%WIDEa_path_new%%WIDEa_launch_Rscript%"

set "Temp_file_path=%WIDEa_files_path%\Temporary_file.r"
set "Temp_file_rout_path=%WIDEa_files_path%\Log_file.r.rout"

echo rm(list = ls(all = TRUE)); s_WIDEa_path = "%WIDEa_path_new%"; source("%WIDEa_launch_Rscript_path%"); > "%Temp_file_path%" 
attrib +s +h "%Temp_file_path%"

echo Launching WIDEa...

"%R_software_path%" CMD BATCH -q --slave --no-restore "%Temp_file_path%" "%Temp_file_rout_path%"

del /A:S /A:H "%Temp_file_path%"
del "%Temp_file_rout_path%"


