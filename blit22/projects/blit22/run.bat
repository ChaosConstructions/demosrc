@echo off

rem mode
set mode=sna
rem Params
set name=blit22

del %name%.sna

echo ---------           Compile           ---------
..\..\sjasm\sjasmplus ablit.asm --nofakes -Dmode=\"%mode%\" -DTWO_PASS
cls
..\..\sjasm\sjasmplus ablit.asm --nofakes -Dmode=\"%mode%\"

echo ---------           Running           ---------
rem Copy labels to emulator
copy "user.l" "..\..\us\"
del user.l



rem Make tap loader
..\..\utils\bas2tap -s%name% -a10 -c loader_tap.bas %name%.tap
..\..\utils\taptool +$ %name%.tap %name%.$C


..\..\utils\bas2tap -s%name% -a10 -c loader_trd.bas loader_trd.tap
..\..\utils\tapto0 -f loader_trd.tap
..\..\utils\0tohob %name%.000
rem del loader_trd.000
rem del loader_trd.tap
..\..\utils\trdtool # %name%.trd
..\..\utils\trdtool + %name%.trd "..\..\boots\maxiboot.$B"
..\..\utils\trdtool + %name%.trd %name%.$B
..\..\utils\trdtool + %name%.trd %name%.$C

if %mode% == sna (
 ..\..\us\unreal %name%.sna
)
if %mode% == tap (
..\..\us\unreal %name%.tap
)