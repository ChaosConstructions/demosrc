;  Teonon 1k generated image by dart/fenomen for CC`2024
;
;  GLSL shader player for 1k generated images 
; forked version of the 4k intro engine "BEPCT4K" by ProvodGL
;          https://github.com/w23/bepct4k
;             

BITS 32

WIDTH equ 1920
HEIGHT equ 1080

%define FULLSCREEN
%define GLCHECK


GL_FRAGMENT_SHADER EQU 0x8b30

%macro WINAPI_FUNC 2
%if 1
	extern __imp__ %+ %1 %+ @ %+ %2
	%define %1 [__imp__ %+ %1 %+ @ %+ %2]
%else
	extern _ %+ %1 %+ @ %+ %2
	%define %1 _ %+ %1 %+ @ %+ %2
%endif
%endmacro

%ifdef FULLSCREEN
WINAPI_FUNC ChangeDisplaySettingsA, 8
%endif
WINAPI_FUNC ChoosePixelFormat, 8
WINAPI_FUNC CreateWindowExA, 48
WINAPI_FUNC ExitProcess, 4
WINAPI_FUNC GetAsyncKeyState, 4
WINAPI_FUNC GetDC, 4
WINAPI_FUNC PeekMessageA, 20
WINAPI_FUNC SetPixelFormat, 12
WINAPI_FUNC SwapBuffers, 4
WINAPI_FUNC wglCreateContext, 4
WINAPI_FUNC wglGetProcAddress, 4
WINAPI_FUNC wglMakeCurrent, 8
WINAPI_FUNC glRects, 16
%ifdef DEBUG
WINAPI_FUNC glGetError, 0
%endif

%macro FNCALL 1-*
	%rep %0-1
		%rotate -1
		push %1
	%endrep
	%rotate -1
	call %1
%endmacro

%macro GL_FUNC 1
section _ %+ %1 data align=1
%1:
%defstr %[%1 %+ __str] %1
	db %1 %+ __str, 0
%endmacro

GL_FUNC glCreateShaderProgramv
GL_FUNC glUseProgram
GL_FUNC glGetUniformLocation
GL_FUNC glUniform1f

%ifdef DEBUG
GL_FUNC glGetProgramInfoLog
%endif

%ifdef DEBUG
	WNDCLASS EQU static_
%else
	%define WNDCLASS 0xc018
%endif

%ifdef FULLSCREEN
section _devmode data align=1
devmode:
	times 9 dd 0
	db 0x9c, 0, 0, 0
	db 0, 0, 0x1c, 0
	times 15 dd 0
	DD	020H, WIDTH, HEIGHT
	times 10 dd 0
%endif

section _pfd data align=1
pfd:
	DW	00H, 00H
	DD	21H ;025H
	DB	00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H
	DB	00H, 00H, 00H, 00H, 00H, 00H
	DD	00H, 00H, 00H

section _shader data align=1
	%include "shader.inc"  ; Attach minified fragment shader .asm file to the intro

section _shdrptr data align=1
src_main:
	dd _shader_frag

section _strings data align=1
%ifdef DEBUG
static_: db "static", 0
%endif

section _text text align=1
_start:
%if 1
	%define ZERO 0
%else
	%define ZERO ecx
	xor ZERO, ZERO
%endif

%ifdef FULLSCREEN
	FNCALL ChangeDisplaySettingsA, devmode, 4
%endif

	;FNCALL ShowCursor, ZERO
	FNCALL CreateWindowExA, ZERO, WNDCLASS, ZERO, 0x90000000, ZERO, ZERO, WIDTH, HEIGHT, ZERO, ZERO, ZERO, ZERO
	FNCALL GetDC, eax
	mov ebp, eax ; ebp = HDC
	FNCALL ChoosePixelFormat, ebp, pfd
	FNCALL SetPixelFormat, ebp, eax, pfd
	FNCALL wglCreateContext, ebp
	FNCALL wglMakeCurrent, ebp, eax
	GLCHECK

	FNCALL wglGetProcAddress, glCreateShaderProgramv
	FNCALL eax, GL_FRAGMENT_SHADER, 1, src_main

	; Shows alarm window if the shader errors was there
%ifdef DEBUG
	push eax
	push infolog
	push 0
	push 1023
	push eax
	FNCALL wglGetProcAddress, glGetProgramInfoLog
	call eax
	push 0
	push infolog
	push infolog
	push 0
	call MessageBoxA
	pop eax
%endif

	mov esi, eax
	FNCALL wglGetProcAddress, glUseProgram
	FNCALL eax, esi
	GLCHECK

	push 01bH ;ESC key code for GetAsyncKeyState
	call GetAsyncKeyState ; RESET KEYS STATES
	
mainloop:

	push 01bH ;ESC key code for GetAsyncKeyState

	; PeekMessageA
	push 1
	push 0
	push 0
	push 0
	push 0

	; SwapBuffers
	push ebp

	; glRects
	push 1
	push 1
	push byte -1
	push byte -1

	call glRects

	call SwapBuffers
	call PeekMessageA
	call GetAsyncKeyState
	jz mainloop
exit:
	call ExitProcess
