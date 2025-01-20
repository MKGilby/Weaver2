@echo off
echo ----------------------------------------------------------------------
echo This script prepares tiles and sprites and copies them to data folder.
echo ----------------------------------------------------------------------

..\..\tools\mkconv2 convert.mc2

..\..\tools\cut floor.gsd 4 1024 ..\..\data\floor.bin
..\..\tools\cut zapper_floor.gsd 4 1024 ..\..\data\zapper_floor.bin
..\..\tools\cut wall.gsd 4 1024 ..\..\data\wall.bin
del *.gsd

for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png
