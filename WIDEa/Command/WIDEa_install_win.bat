@echo off

set "current_path=%~dp0"
set WIDEa_path=%current_path:~0,-8%
set WIDEa_path_new=%WIDEa_path:\=/%
set "WIDEa_files_path=C:\Users\%USERNAME%\Documents\WIDEa files"
set "R_software_txt_file_path=%WIDEa_files_path%\Rexe_path.txt"

setlocal enableDelayedExpansion

echo WIDEa software installation :
echo -----------------------------

IF EXIST "%R_software_txt_file_path%" (
  set /p R_software_path=<"%R_software_txt_file_path%" 
  set "value=0"
) ELSE (
  echo 1. Creating WIDEa files folder in user's Documents folder and searching R software on your computer...
  IF %PROCESSOR_ARCHITECTURE% == x86 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "i386"') do set R_software_path=%%a
  ) 
  IF %PROCESSOR_ARCHITECTURE% == AMD64 (
     for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\R.exe" ^| find "x64"') do set R_software_path=%%a
  ) 
  md "%WIDEa_files_path%"
  echo !R_software_path! > "%R_software_txt_file_path%"
  echo Done
  set "value=1"
)

set "Install_pkg_Rscript=R_script/install_packages.r"
set "Install_pkg_Rscript_path=%WIDEa_path_new%%Install_pkg_Rscript%"

set "Temp_file_path=%WIDEa_path%R_script\Temporary_file.r"
set "Temp_file_rout_path=%WIDEa_path%R_script\Log_file.r.rout"

echo source("%Install_pkg_Rscript_path%"); > "%Temp_file_path%" 

if %value%==0 echo 1. Downloading and installing R packages (few minutes)...
if %value%==1 echo 2. Downloading and installing R packages (few minutes)...

"%R_software_path%" CMD BATCH -q --slave --no-restore "%Temp_file_path%" "%Temp_file_rout_path%"

echo Done

del "%Temp_file_path%"
del "%Temp_file_rout_path%"

if %value%==0 echo 2. Paste WIDEa logo in shiny R package folder
if %value%==1 echo 3. Paste WIDEa logo in shiny R package folder

endlocal

set "logo_src_path=%WIDEa_path%\Image\WIDEa_header_img.png"

for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\Users\%username%\" ^| find "R\" ^| find "shiny\" ^| find "NEWS.md"') do (set R_library_path=%%a)
for /f "tokens=1 delims=NEWS" %%a in ("%R_library_path%") do (set R_library_path=%%a)
set "logo_dest_path=%R_library_path%www\WIDEa_header_img.png"
echo f | xcopy /f /y "%logo_src_path%" "%logo_dest_path%"

echo Done