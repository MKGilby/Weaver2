@echo off
echo ----------------------------------------------------------------------
echo This script prepares tiles and sprites and copies them to data folder.
echo ----------------------------------------------------------------------

..\..\tools\mkconv2 convert.mc2
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png
