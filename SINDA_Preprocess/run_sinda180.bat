rem del TAPE*
copy %1.sin sinda.sin
del %1.lis
del %1.for
del %1.out
del %1.tp*
rem C:\alpha_sindag\p171dp
sin180pre
ren sinda.lis %1.lis
ren sinda.for %1.for
rem ren sinda.tp2 %1.tp2
rem ren sinda.tp4 %1.tp4
DF %1.for /link sin180pc.lib
%1
rename sinda.out %1.out
del sinda.sin
rem del %1.for
del %1.exe
rem del sinda.tp*
