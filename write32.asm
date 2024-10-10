BITS 32
    org 0x08048000

ehdr:
    db 0x7f
    db 0x45
    db 0x4c
    db 0x46
    db 0x01
    db 0x01
    db 0x01
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    db 0x00
    dw 0x0002
    dw 0x0003
    dd 0x00000001
    dd _start
    dd e_phoff
    dd 0x00000000
    dd 0x00000000
    dw ehdrsize
    dw phdrsize
    dw 0x0001
    dw 0x0000
    dw 0x0000
    dw 0x0000
phdr:
    dd 0x00000001
    dd 0x00000000
    dd 0x08048000
    dd 0x08048000
    dd filesize
    dd filesize
    dd 0x00000005
    dd 0x00001000
phdr_end:
    ehdrsize equ phdr - ehdr
    phdrsize equ phdr_end - phdr
    e_phoff equ phdr - ehdr
msg:
    db 0x48
    db 0x65
    db 0x6c
    db 0x6c
    db 0x6f
    db 0x20
    db 0x57
    db 0x6f
    db 0x72
    db 0x6c
    db 0x64
    db 0x21
    db 0x0a
_start:
    mov eax, 0x00000004
    mov ebx, 0x00000001
    mov ecx, 0x08048054
    mov edx, 0x0000000d
    int 0x80
    call exit
exit:
    mov eax, 0x00000001
    mov ebx, 0x00000000
    int 0x80
footer:
    filesize equ footer - ehdr
