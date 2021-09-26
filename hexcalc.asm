;   HexCalc is a hexadecimal calculator, which allows you to see 
;   the results of executing most of the x86 arithmetic instructions.
;
;   Copyright (C) 2003 Lukasz Czajka
;
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 2 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to the Free Software
;   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

    .486
    .model flat, stdcall
    option casemap :none   ; case sensitive

    WinMain proto :DWORD,:DWORD,:DWORD,:DWORD
    AboutProc proto :DWORD,:DWORD,:DWORD,:DWORD
    NumberProc proto :DWORD,:DWORD,:DWORD,:DWORD
    EditProc proto :DWORD,:DWORD,:DWORD,:DWORD
    strbin proto :DWORD,:DWORD
    stroct proto :DWORD,:DWORD
    SetFlags proto :DWORD
    EnableButtons proto :DWORD

    include \masm32\include\windows.inc
    include \masm32\include\user32.inc
    include \masm32\include\kernel32.inc
    include \masm32\include\gdi32.inc

    includelib \masm32\lib\user32.lib
    includelib \masm32\lib\kernel32.lib
    includelib \masm32\lib\gdi32.lib
.const
    IDE_NUMBER  equ 101
    IDE_ANSI    equ 144
    IDE_ASCII   EQU 139
    IDE_UNICODE EQU 132
    
    ID_MUL      equ 42
    ID_SUB      equ 45
    ID_ADD      equ 43
    ID_DIV      equ 47
    ID_MOD      equ 37
    ID_SIGN     equ 102
    ID_OR       equ 92
    ID_XOR      equ 94
    ID_NOT      equ 96
    ID_AND      equ 38
    ID_SHL      equ 44
    ID_SHR      equ 46
    ID_ROL      equ 114
    ID_ROR      equ 111
    ID_RCL      equ 126
    ID_RCR      equ 127
    ID_SAL      equ 128
    ID_SAR      equ 129

    ID_TEST     equ 130
    ID_BSWAP    equ 131

    ID_EQUALS   equ 104

    ID_BACK     equ 107
    ID_CE       equ 106

    ID_ABOUT    equ 137

    IDB_SIGNED      equ 141
    IDB_UNSIGNED    equ 142

    IDB_DWORD   equ 152
    IDB_WORD    equ 153
    IDB_BYTE    equ 154

    IDB_DEC     equ 301
    IDB_HEX     equ 302
    IDB_OCT     equ 303
    IDB_BIN     equ 304

    CF_CLEAR  EQU 305
    CF_SET    EQU 306
    
    ZF_CLEAR  EQU 307
    ZF_SET    EQU 308

    OF_CLEAR  EQU 309
    OF_SET    EQU 310

    PF_CLEAR  EQU 311
    PF_SET    EQU 312

    SF_CLEAR  EQU 313
    SF_SET    EQU 314

    DLG_ABOUT   equ 200

; -------------------------------------------------------------------------

.data
    FirstNum        dd 0
    SecondNum       dd 0
    Num             dd 0

    operation       dd 0
    newnum          dd 0
    sign            dd -1    ; nonzero - signed
    system          dd IDB_DEC
    xsize           dd IDB_DWORD

    hwndMain        dd 0
    hwndNumber      dd 0
    hwndANSI        dd 0
    hwndASCII       dd 0
    hwndUnicode     dd 0

    OldEditProc     dd 0

    hInstance       dd 0
    CommandLine     dd 0

    szAppName       db 'HexCalc',0
    szDivByZero     db 'Division by zero!',0
    szHex           db '%X',0
    szDecU          db '%u',0
    szDecS          db '%d',0

    buffer          db 33 dup(0)
.code

; -------------------------------------------------------------------------

start:
    invoke GetModuleHandle, NULL 
    mov    hInstance,eax 
    invoke GetCommandLine
    mov CommandLine,eax 
    invoke WinMain, hInstance,NULL,CommandLine, SW_SHOWDEFAULT 
    invoke ExitProcess,eax 

; -------------------------------------------------------------------------

WinMain proc hInst:HINSTANCE,hPrevInst:HINSTANCE,CmdLine:LPSTR,CmdShow:DWORD 
    LOCAL wc:WNDCLASS
    LOCAL msg:MSG
    
    mov wc.style, CS_HREDRAW or CS_VREDRAW
    mov wc.lpfnWndProc, offset WndProc
    mov wc.cbClsExtra,0
    mov wc.cbWndExtra,DLGWINDOWEXTRA
    mov wc.lpszMenuName,NULL
    mov wc.lpszClassName,OFFSET szAppName 
    mov wc.hbrBackground,COLOR_BTNFACE+1 
    push hInst
    pop wc.hInstance
    invoke LoadIcon,hInst,offset szAppName
    mov wc.hIcon,eax
    invoke LoadCursor,NULL,IDC_ARROW
    mov wc.hCursor,eax

    invoke RegisterClass, addr wc 

    invoke CreateDialogParam,hInst,offset szAppName,0,NULL,0
    mov hwndMain,eax

; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««

    invoke GetDlgItem,eax,IDE_NUMBER
    mov hwndNumber,eax

    invoke SetWindowLong,eax,GWL_WNDPROC,offset NumberProc
    mov OldEditProc, eax

    invoke GetDlgItem,hwndMain,IDE_ANSI
    mov hwndANSI,eax
    invoke SetWindowLong,eax,GWL_WNDPROC,offset EditProc

    invoke GetDlgItem,hwndMain,IDE_ASCII
    mov hwndASCII,eax
    invoke SetWindowLong,eax,GWL_WNDPROC,offset EditProc
    invoke GetStockObject,OEM_FIXED_FONT
    invoke SendMessage,hwndASCII,WM_SETFONT,eax,FALSE

    invoke GetDlgItem,hwndMain,IDE_UNICODE
    mov hwndUnicode,eax
    invoke SetWindowLong,eax,GWL_WNDPROC,offset EditProc

    push 0
    call SetFlags

    invoke SendMessage,hwndMain,WM_COMMAND,IDB_DEC,0
    invoke SendMessage,hwndMain,WM_COMMAND,IDB_SIGNED,0
    invoke SendMessage,hwndMain,WM_COMMAND,IDB_DWORD,0  
  
; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««

    .WHILE TRUE 
        invoke GetMessage, ADDR msg,NULL,0,0 
        .BREAK .IF (!eax) 
        invoke TranslateMessage, ADDR msg 
        invoke DispatchMessage, ADDR msg 
    .ENDW 
    mov eax,msg.wParam 
    ret 
WinMain endp

; -------------------------------------------------------------------------

WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM 
    LOCAL hButton:DWORD
    cf equ hButton
   ; LOCAL cf:DWORD

    mov edx,uMsg
    .IF edx==WM_KEYDOWN
        .if wParam==VK_BACK || wParam==VK_DELETE
            mov wParam,ID_BACK
            jmp flash    
        .elseif wParam==VK_ADD
            mov wParam,ID_ADD
            jmp flash
        .elseif wParam==VK_SUBSTRACT
            mov wParam,ID_SUB
            jmp flash
        .elseif wParam==VK_RETURN
            mov wParam,ID_EQUALS
            jmp flash
        .elseif wParam==VK_MULTIPLY
            mov wParam,ID_MUL
            jmp flash
        .elseif wParam==VK_DIVIDE
            mov wParam,ID_DIV
            jmp flash
        .endif
    .ELSEIF edx==WM_CHAR
        .if wParam > 60h && wParam < 7Bh
            sub wParam,27h
        .endif
flash:
        invoke GetDlgItem,hWnd,wParam
        test eax,eax
        jz end_proc
        mov hButton,eax
        invoke IsWindowEnabled,eax
        test eax,eax
        jz end_proc
            invoke SendMessage,hButton,BM_SETSTATE,1,0
            invoke Sleep,100
            invoke SendMessage,hButton,BM_SETSTATE,0,0
        jmp command
; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««      
    .ELSEIF edx==WM_COMMAND
command:
        mov eax,wParam
        ror eax,16
        test ax,ax
        jnz end_proc
        shr eax,16
        .if eax >= 30h && eax < 40h   ; digit
            .if newnum==0
                .if system==IDB_BIN
                    mov edx,2
                .elseif system==IDB_HEX
                    mov edx,16
                .elseif system==IDB_OCT
                    mov edx,8
                .else   ; decimal
                    mov edx,10
                .endif

                mov ecx,eax
                mov eax,Num
                sub ecx,30h
                push ebx
                xor ebx,ebx
                .if xsize==IDB_DWORD
                    cmp edx,10
                    jne @endif1
                    cmp sign,0
                    je @endif1
                    cmp eax,0
                    jge @endif1
                        neg eax
                        jo err_1
                        mov ebx,1
                   @endif1:
                    mul edx
                    jo err_1
                    .if ebx==1
                        test eax,eax
                        js err_1
                        add eax,ecx                    
                        neg eax
                    .else
                        add eax,ecx
                    .endif
                .elseif xsize==IDB_WORD
                    cmp edx,10
                    jne @endif2
                    cmp sign,0
                    je @endif2
                    cmp ax,0
                    jge @endif2
                        neg ax
                        jo err_1
                        mov ebx,1
                   @endif2:
                    mul dx
                    jo err_1
                    .if ebx==1
                        test ax,ax
                        js err_1
                        add eax,ecx
                        neg ax
                    .else
                        add eax,ecx
                    .endif
                .else       ; byte
                    cmp edx,10
                    jne @endif3
                    cmp sign,0
                    je @endif3
                    cmp al,0
                    jge @endif3
                        neg al
                        jo err_1
                        mov ebx,1
                   @endif3:
                    mul dl
                    jo err_1
                    .if ebx==1
                        test al,al
                        js err_1
                        add eax,ecx
                        neg al
                    .else
                        add eax,ecx
                    .endif
                .endif
                mov Num,eax
                mov SecondNum,eax
                jmp @end1
            err_1:
                invoke MessageBeep,0
            .else
                .if newnum==2
                    xor edx,edx
                    mov operation,edx
                .else
                    mov edx,Num
                .endif
                sub eax,30h
                mov FirstNum,edx
                mov Num,eax
                mov SecondNum,eax
                xor eax,eax
                mov newnum,eax
            .endif
        .elseif eax==ID_BSWAP
            mov eax,Num
            bswap eax
            pushfd
            mov Num,eax
            mov SecondNum,eax
            call SetFlags           
        .elseif eax==ID_NOT
            .if xsize==IDB_DWORD
                not Num
                pushfd
            .elseif xsize==IDB_WORD
                not word ptr [Num]
                pushfd
            .else   ; byte
                not byte ptr [Num]
                pushfd
            .endif
            mov eax,Num
            mov SecondNum,eax
            call SetFlags           
        .elseif eax==ID_SIGN
            .if xsize==IDB_DWORD
                neg Num            
                pushfd
            .elseif xsize==IDB_WORD
                neg word ptr Num
                pushfd
            .else   ; byte
                neg byte ptr Num
                pushfd
            .endif
            mov eax,Num
            mov SecondNum,eax
            call SetFlags           
; -------------------------------------------------------------------------
        .elseif eax==ID_EQUALS
            mov edx,operation
            test edx,edx
            jz end_proc
            mov eax,FirstNum
            mov ecx,SecondNum
            .if edx==ID_ADD
                .if xsize==IDB_DWORD
                    add eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    add ax,cx
                    pushfd
                .else   ; byte
                    add al,cl
                    pushfd
                .endif
            .elseif edx==ID_SUB
                .if xsize==IDB_DWORD
                    sub eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    sub ax,cx
                    pushfd
                .else   ; byte
                    sub al,cl
                    pushfd
                .endif
            .elseif edx==ID_MUL
                .if xsize==IDB_DWORD
                    mov edx,sign
                    test edx,edx
                    jz @s1  ; no sign
                        imul ecx    
                    jmp @s2
                   @s1:
                        mul ecx
                   @s2:
                    pushfd    
                .elseif xsize==IDB_WORD
                    mov edx,sign
                    test edx,edx
                    jz @s3  ; no sign
                        imul cx    
                    jmp @s4
                   @s3:
                        mul cx
                   @s4:
                    pushfd    
                .else   ; byte
                    mov edx,sign
                    test edx,edx
                    jz @s5  ; no sign
                        imul cl    
                    jmp @s6
                   @s5:
                        mul cl
                   @s6:
                    pushfd    
                    xor ah,ah
                .endif
            .elseif edx==ID_TEST          
                .if xsize==IDB_DWORD
                    test eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    test ax,cx
                    pushfd
                .else   ; byte
                    test al,cl
                    pushfd
                .endif
            .elseif edx==ID_XOR
                .if xsize==IDB_DWORD
                    xor eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    xor ax,cx
                    pushfd
                .else   ; byte
                    xor al,cl
                    pushfd
                .endif
            .elseif edx==ID_OR
                .if xsize==IDB_DWORD
                    or eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    or ax,cx
                    pushfd
                .else   ; byte
                    or al,cl
                    pushfd
                .endif
            .elseif edx==ID_AND
                .if xsize==IDB_DWORD
                    and eax,ecx
                    pushfd    
                .elseif xsize==IDB_WORD
                    and ax,cx
                    pushfd
                .else   ; byte
                    and al,cl
                    pushfd
                .endif
            .elseif edx==ID_SHL
                .if xsize==IDB_DWORD
                    shl eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    shl ax,cl
                    pushfd
                .else   ; byte
                    shl al,cl
                    pushfd
                .endif
            .elseif edx==ID_SHR
                .if xsize==IDB_DWORD
                    shr eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    shr ax,cl
                    pushfd
                .else   ; byte
                    shr al,cl
                    pushfd
                .endif
            .elseif edx==ID_SAL         
                .if xsize==IDB_DWORD
                    sal eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    sal ax,cl
                    pushfd
                .else   ; byte
                    sal al,cl
                    pushfd
                .endif
            .elseif edx==ID_SAR
                .if xsize==IDB_DWORD
                    sar eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    sar ax,cl
                    pushfd
                .else   ; byte
                    sar al,cl
                    pushfd
                .endif
            .elseif edx==ID_ROL
                .if xsize==IDB_DWORD
                    rol eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    rol ax,cl
                    pushfd
                .else   ; byte
                    rol al,cl
                    pushfd
                .endif
            .elseif edx==ID_ROR
                .if xsize==IDB_DWORD
                    ror eax,cl
                    pushfd    
                .elseif xsize==IDB_WORD
                    ror ax,cl
                    pushfd
                .else   ; byte
                    ror al,cl
                    pushfd
                .endif
            .elseif edx>=ID_RCL
                pushad    ; save
                invoke GetDlgItem, hWnd, CF_SET            
                invoke SendMessage, eax, BM_GETCHECK, 0, 0
                mov cf, eax
                popad
                .if edx==ID_RCL
                    mov edx,eax
                    mov eax,cf
                    lahf
                    and ah,0feh
                    or ah,al
                    .if xsize==IDB_DWORD
                        sahf
                        rcl edx,cl
                        pushfd    
                    .elseif xsize==IDB_WORD
                        sahf
                        rcl dx,cl
                        pushfd
                    .else   ; byte
                        sahf
                        rcl dl,cl
                        pushfd
                    .endif                    
                .else   ; rcr
                    mov edx,eax
                    mov eax,cf
                    lahf
                    and ah,0feh
                    or ah,al
                    .if xsize==IDB_DWORD
                        sahf
                        rcr edx,cl
                        pushfd    
                    .elseif xsize==IDB_WORD
                        sahf
                        rcr dx,cl
                        pushfd
                    .else   ; byte
                        sahf
                        rcr dl,cl
                        pushfd
                    .endif                    
                .endif
                mov eax,edx
            .else   ; div or mod
                test ecx,ecx
                jz div_by_zero
                .if xsize==IDB_DWORD
                    mov edx,sign
                    test edx,edx
                    jz @s7  ; no sign
                        xor edx,edx
                        test eax,eax
                        jns @ss1
                            dec edx     ; make sign
                      @ss1:
                        idiv ecx    
                    jmp @s8
                   @s7:
                        div ecx
                   @s8:
                    pushfd  
                    .if operation==ID_MOD
                        mov eax,edx
                    .endif
                .elseif xsize==IDB_WORD
                    mov edx,sign
                    test edx,edx
                    jz @s9  ; no sign
                        xor edx,edx
                        test ax,ax
                        jns @ss2
                            dec dx     ; make sign
                      @ss2:
                        idiv cx    
                    jmp @s10
                   @s9:
                        div ecx
                   @s10:
                    pushfd    
                    .if operation==ID_MOD
                        mov eax,edx
                    .endif
                .else   ; byte
                    mov edx,sign
                    xor ah,ah
                    test edx,edx
                    jz @s11  ; no sign
                        test al,al
                        jns @ss3
                            dec ah     ; make sign
                      @ss3:
                        idiv cl    
                    jmp @s12
                   @s11:
                        div cl
                   @s12:
                    pushfd
                    .if operation==ID_MOD
                        mov al,ah
                    .endif
                    xor ah,ah
                .endif
            .endif
            mov SecondNum,ecx
            mov FirstNum,eax
            mov Num,eax
            call SetFlags
            mov newnum,2
            
; -------------------------------------------------------------------------
            
        .elseif eax==ID_BACK
            mov eax,Num
            
            .if system==IDB_HEX
                mov ecx,16
            .elseif system==IDB_OCT
                mov ecx,8
            .elseif system==IDB_BIN
                mov ecx,2
            .else ; dec
                mov ecx,10
            .endif

            xor edx,edx
            .if sign==0
                div ecx                
            .else
                .if eax<0
                    dec edx     ; make sign
                .endif
                idiv ecx
            .endif

            mov Num,eax
            mov SecondNum,eax
        .elseif eax==ID_CE
            xor eax,eax
            mov Num,eax
            mov FirstNum,eax
            mov SecondNum,eax
            mov operation,eax
            mov newnum,eax
            push eax
            call SetFlags
        .elseif eax==ID_ABOUT
            invoke DialogBoxParam, hInstance, DLG_ABOUT, hWnd, addr AboutProc,0
        .else
            .if eax>=CF_CLEAR
                mov edx,eax
                test edx,1    ; make odd (see constant values)
                jnz @@@1
                    dec edx
              @@@1:
                mov ecx,edx
                inc ecx
            .elseif eax>=IDB_DEC
                mov system,eax
                mov edx,IDB_DEC
                mov ecx,IDB_BIN
                invoke EnableButtons,eax
            .elseif eax>=IDB_DWORD
                mov xsize,eax
                mov edx,IDB_DWORD
                mov ecx,IDB_BYTE
                pushad
                .if eax==IDB_WORD
                    and Num,0ffffh
                    mov ebx,FALSE
                .elseif eax==IDB_BYTE
                    and Num,0ffh 
                    mov ebx,FALSE
                .else
                    mov ebx,TRUE
                .endif
                invoke GetDlgItem,hwndMain,ID_BSWAP
                invoke EnableWindow,eax,ebx
                popad
            .elseif eax>=IDB_SIGNED
                mov sign,eax
                mov ecx,IDB_UNSIGNED
                mov edx,IDB_SIGNED
                sub sign,ecx
            .else   ; operation
                .if newnum==0 && operation!=0
                    push eax
                    invoke SendMessage, hWnd, WM_COMMAND, ID_EQUALS, 0
                    pop eax
                .endif
                mov operation,eax
                mov newnum,1
                jmp focus
            .endif
            invoke CheckRadioButton, hWnd, edx, ecx, eax
        .endif
        
    @end1:
        mov ecx,Num

        cmp system,IDB_HEX
        jnz @e1
        mov edx,offset szHex
        jmp @e3
    @e1:
        cmp system,IDB_DEC
        jnz @e2
        .if sign==0
            mov edx,offset szDecU
        .else
            .if xsize==IDB_WORD
                test cx,cx
                jns @@e1
                    or ecx,0ffff0000h
                @@e1:
            .elseif xsize==IDB_BYTE
                test cl,cl
                jns @@e2
                    or ecx,0ffffff00h
                @@e2:
            .endif
            mov edx,offset szDecS
        .endif
    @e3:
        invoke wsprintf, addr buffer, edx, ecx
        jmp @e4
    @e2:
        cmp system,IDB_OCT
        jnz @e5
        invoke stroct, addr buffer, ecx
        jmp @e4
    @e5:
        invoke strbin, addr buffer, ecx
    @e4:
        invoke SetWindowText,hwndNumber,addr buffer
        
        mov ecx,Num
        mov edx,offset buffer
        .if xsize==IDB_WORD
            .if ch==0h
                mov ch,2eh
            .endif
            mov [edx],ch
            .if cl==0h
                mov cl,2eh
            .endif
            mov [edx+1],cl
            xor ecx,ecx
            mov [edx+2],cl
        .elseif xsize==IDB_DWORD
            .if ch==0h
                mov ch,2eh
            .endif
            mov [edx+2],ch
            .if cl==0h
                mov cl,2eh
            .endif
            mov [edx+3],cl
            
            shr ecx,16
            
            .if ch==0h
                mov ch,2eh
            .endif
            mov [edx],ch
            .if cl==0h
                mov cl,2eh
            .endif
            mov [edx+1],cl
            
            xor ecx,ecx
            mov [edx+4],cl
        .else
            .if cl==0h
                mov cl,2eh
            .endif
            mov [edx],cl
            xor ecx,ecx
            mov [edx+1],cl
        .endif
        invoke SetWindowTextA,hwndANSI,edx
        invoke SetWindowTextA,hwndASCII,addr buffer
        
    ; unicode

        mov ecx,Num
        mov edx,offset buffer
        .if xsize==IDB_WORD
            .if cx==0
                dec cx
            .endif
            mov [edx],cx
            xor ecx,ecx
            mov [edx+2],cx
        .elseif xsize==IDB_DWORD
            .if cx==0
                dec cx
            .endif
            mov [edx+2],cx         
            shr ecx,16
            
            .if cx==0
                dec cx
            .endif
            mov [edx],cx
            xor ecx,ecx
            mov [edx+4],cx
        .else
            mov [edx],cl
            xor ecx,ecx
            mov [edx+1],ecx
        .endif
        invoke SetWindowTextW,hwndUnicode,addr buffer
focus:
        invoke SetFocus, hWnd
    .ELSEIF uMsg==WM_DESTROY
        invoke PostQuitMessage,0
    .ELSE
        invoke DefWindowProc,hWnd,uMsg,wParam,lParam 
        ret
    .ENDIF
    
end_proc:
    xor eax,eax
    ret

div_by_zero:
    invoke SetWindowText,hwndNumber,offset szDivByZero
    jmp focus

WndProc endp

EditProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM 
    .if uMsg==WM_CHAR || uMsg==WM_KEYDOWN
        invoke SendMessage, hwndMain, uMsg, wParam, lParam
    .endif
    invoke CallWindowProc,OldEditProc,hWnd,uMsg,wParam,lParam
    ret
EditProc endp

NumberProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM 
    .if uMsg==WM_SETFOCUS
        invoke SetFocus,hwndMain
    .else
        invoke CallWindowProc,OldEditProc,hWnd,uMsg,wParam,lParam
        ret
    .endif
    xor eax,eax
    ret
NumberProc endp

AboutProc proc hDlg:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM 
    .if uMsg==WM_COMMAND
        .if wParam==IDOK
            invoke EndDialog, hDlg, 0
        .else
            xor eax,eax
            ret
        .endif
    .elseif uMsg!=WM_INITDIALOG
        xor eax,eax
        ret
    .endif

    mov eax,TRUE
    ret
AboutProc endp

strbin proc buff:DWORD, num:DWORD
    mov edx,buff
    mov eax,num
    bsr ecx,eax
    jz @zero
    add edx,ecx
    mov byte ptr [edx+1],0
    mov ecx,3031h   
 @lp:
    shr eax,1
    jz @endlp
    jc @@1
        mov byte ptr [edx], ch  ; '0'
    jmp @@2
   @@1:
        mov byte ptr [edx], cl  ; '1'
   @@2:
    dec edx
    jmp @lp
 @endlp:
    jnc @@3
        mov [edx],cl
   @@3:
    ret
 @zero:
    add eax,30h
    mov [edx],eax
    ret
strbin endp

stroct proc buff:DWORD, num:DWORD
    mov ecx,num
    bsr eax,ecx
    jz @zero
    inc eax
    xor ah,ah
    mov edx,3
    div dl
    test ah,ah
    jz @@1
        inc al
  @@1:
    and eax,0ffh
    mov edx,buff
    add edx,eax
    mov byte ptr [edx],0
    dec edx
 @lp:
    mov eax,ecx
    and eax,07h
    add eax,30h
    mov [edx],al

    dec edx
    shr ecx,3
    jnz @lp

    ret
 @zero:
    mov edx,buff
    add ecx,30h
    mov [edx],ecx
    ret

stroct endp

SetFlags proc flags:DWORD
    push ebx

    mov ebx,flags
    mov ecx,CF_CLEAR
    test ebx,1h     ; cf
    jz no_cf
        inc ecx
 no_cf:
        invoke CheckRadioButton, hwndMain, CF_CLEAR, CF_SET, ecx 

    mov ecx,PF_CLEAR
    test ebx,4h     ; pf
    jz no_pf
        inc ecx
 no_pf:
        invoke CheckRadioButton, hwndMain, PF_CLEAR, PF_SET, ecx 

    mov ecx,ZF_CLEAR
    test ebx,40h     ; zf
    jz no_zf
        inc ecx
 no_zf:
        invoke CheckRadioButton, hwndMain, ZF_CLEAR, ZF_SET, ecx 

    mov ecx,SF_CLEAR
    test ebx,80h     ; sf
    jz no_sf
        inc ecx
 no_sf:
        invoke CheckRadioButton, hwndMain, SF_CLEAR, SF_SET, ecx 

    mov ecx,OF_CLEAR
    test ebx,800h     ; of
    jz no_of
        inc ecx
 no_of:
        invoke CheckRadioButton, hwndMain, OF_CLEAR, OF_SET, ecx 

    pop ebx
    ret
SetFlags endp

EnableButtons proc sys:DWORD
 LOCAL en2:DWORD
    
    pushad

    .if sys==IDB_BIN
        mov ebx,FALSE   ; 2 - 7
        mov en2,FALSE   ; 8 - 9
        mov edi,FALSE   ; a - f
    .elseif sys==IDB_OCT
        mov ebx,TRUE   ; 2 - 7
        mov en2,FALSE   ; 8 - 9
        mov edi,FALSE   ; a - f
    .elseif sys==IDB_DEC
        mov ebx,TRUE   ; 2 - 7
        mov en2,TRUE   ; 8 - 9
        mov edi,FALSE   ; a - f
    .else
        mov ebx,TRUE
        mov en2,TRUE
        mov edi,TRUE
    .endif

    mov esi,32h
 lp1:
    invoke GetDlgItem,hwndMain,esi
    invoke EnableWindow,eax,ebx
    inc esi
    cmp esi,38h
    jnz lp1

 lp2:
    invoke GetDlgItem,hwndMain,esi
    invoke EnableWindow,eax,en2
    inc esi
    cmp esi,3Ah
    jnz lp2

 lp3:
    invoke GetDlgItem,hwndMain,esi
    invoke EnableWindow,eax,edi
    inc esi
    cmp esi,40h
    jnz lp3

    popad
    ret
EnableButtons endp

end start
