cd %~dp0

..\..\sjasm\sjasmplus scroll.asm --nofakes

copy "user.l" "..\..\us\"

..\..\us\unreal scroll.sna
