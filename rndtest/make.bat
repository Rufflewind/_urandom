set QTPREFIX=C:\Qt\5.7\msvc2015_64
cl /EHsc /LD rndmod.cpp /Ferndmod.dll && ^
cl /EHsc /I%QTPREFIX%\include /I%QTPREFIX%\include\QtCore rndtest.cpp /link /LIBPATH:%QTPREFIX%\lib Qt5Core.lib && ^
rndtest
