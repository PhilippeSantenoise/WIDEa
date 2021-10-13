@echo off

set "WIDEa_files_path=C:\Users\%USERNAME%\Documents\WIDEa files"

rmdir /S /Q "%WIDEa_files_path%"

for /f "tokens=1 delims=" %%a in ('dir /s /b "c:\Users\%username%\Documents\R\WIDEa_header_img.png" ^| find "shiny\www"') do (set logo_path=%%a)
del /Q "%logo_path%"