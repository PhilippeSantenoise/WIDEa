@echo off

set "current_path=%~dp0"
set "WIDEa_files_path=C:\Users\%USERNAME%\Documents\WIDEa files"
set "R_software_txt_file_path=%WIDEa_files_path%\Rexe_path.txt"

setlocal enableDelayedExpansion

IF EXIST "%R_software_txt_file_path%" (
  set /p R_software_path=<"%R_software_txt_file_path%" 
) ELSE (
  echo **Creating the "WIDEa files" folder in the user's Documents folder
  md "%WIDEa_files_path%"
  echo Done
  echo;
  echo **Searching R software in your computer and saving the path in the "WIDEa files" folder...
  IF %PROCESSOR_ARCHITECTURE% == x86 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "i386"') do set R_software_path=%%a
  ) 
  IF %PROCESSOR_ARCHITECTURE% == AMD64 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "x64"') do set R_software_path=%%a
  ) 
  echo !R_software_path! > "%R_software_txt_file_path%"
  echo Done
  echo;
)

echo Important: The following steps can take several minutes. A log file (see the "WIDEa files"
echo folder) is created during installation of all R packages required. The log file and the
echo terminal will be automatically deleted/closed when all steps have been completed. Please,
echo don't close the terminal before that.
echo;

set "Rcmd_path1=%current_path%\pre_install_rcmd.r"
set "Rcmd_path2=%current_path%\WIDEa_install_rcmd.r"
set "Rcmd_log_path=%WIDEa_files_path%\Log_file.r.rout"

echo **Installing the devtools package...

"%R_software_path%" CMD BATCH -q --slave --no-restore "%Rcmd_path1%" "%Rcmd_log_path%"

echo Done
echo;

echo **Installing the WIDEa package...

"%R_software_path%" CMD BATCH -q --slave --no-restore "%Rcmd_path2%" "%Rcmd_log_path%"

echo Done

del "%Rcmd_log_path%"

endlocal