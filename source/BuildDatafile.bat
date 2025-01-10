@echo off
if exist ..\release\x64\Weaver2.data del ..\release\x64\Weaver2.data
..\tools\mad4 ..\release\x64\Weaver2.data ..\data * -1 -r -n
copy /y ..\release\x64\Weaver2.data ..\release\x86\Weaver2.data 
