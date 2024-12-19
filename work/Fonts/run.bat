..\..\tools\fontbuild2 data\font1.png font1_.png -charset "ABCDEFGHIJKLMNOPQRS:TUVWXYZ0123456789.!?()" -sort
..\..\tools\fontbuild2 data\font2.png font2_.png -charset "ABCDEFGHIJKLMNOPQRS:TUVWXYZ0123456789.!?()" -sort 
..\..\tools\mkconv2 convert.mc2
del font1_.png 
del font2_.png 
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png
