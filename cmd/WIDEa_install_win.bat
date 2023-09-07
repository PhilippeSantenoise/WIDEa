@echo off

set "WIDEa_files_path=C:\Users\%USERNAME%\Documents\WIDEa files"
set "R_software_txt_file_path=%WIDEa_files_path%\Rexe_path.txt"
set "Command=rm(list = ls(all = TRUE)); if(!require(devtools)){install.packages("devtools")}; devtools::install_github("PhilippeSantenoise/WIDEa");"

setlocal enableDelayedExpansion

IF EXIST "%R_software_txt_file_path%" (
  set /p R_software_path=<"%R_software_txt_file_path%" 
) ELSE (
  echo Creating WIDEa files folder in user's Documents folder and searching R software on your computer...
  IF %PROCESSOR_ARCHITECTURE% == x86 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "i386"') do set R_software_path=%%a
  ) 
  IF %PROCESSOR_ARCHITECTURE% == AMD64 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "x64"') do set R_software_path=%%a
  ) 
  md "%WIDEa_files_path%"
  echo !R_software_path! > "%R_software_txt_file_path%"
  echo Done
)

set "Temp_file_path=%WIDEa_files_path%\Temporary_file.r"
set "Temp_file_rout_path=%WIDEa_files_path%\Log_file.r.rout"

echo !Command! > "%Temp_file_path%" 
attrib +s +h "%Temp_file_path%"

echo Installing the WIDEa package (few minutes)...

"%R_software_path%" CMD BATCH -q --slave --no-restore "%Temp_file_path%" "%Temp_file_rout_path%"

echo Done

del "%Temp_file_path%"
del "%Temp_file_rout_path%"

endlocal