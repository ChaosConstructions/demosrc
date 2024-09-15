set LIBS= /LIBPATH:libs opengl32.lib kernel32.lib user32.lib gdi32.lib
set SHADER_MINIFIER=deps\shader_minifier.exe
set NASM=deps\nasm.exe
set CRINKLER=deps\crinkler.exe

set OPTS= ^
	/ENTRY:start ^
	/CRINKLER ^
	/SUBSYSTEM:WINDOWS ^
	/UNSAFEIMPORT ^
	/NOINITIALIZERS ^
	/RANGE:opengl32 ^
	/PRINT:IMPORTS ^
	/PRINT:LABELS ^
	/TRANSFORM:CALLS ^
	/TINYIMPORT


%SHADER_MINIFIER% --format nasm -o shader.inc shader.frag || exit /b 1
%NASM% -fwin32 -o intro.obj intro.asm || exit /b 2

%CRINKLER% ^
	%OPTS% ^
	/COMPMODE:SLOW /ORDERTRIES:16000 ^
	%LIBS% ^
	intro.obj /OUT:intro.exe ^
	|| exit /b 2
