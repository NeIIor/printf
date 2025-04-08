section .note.GNU-stack noalloc noexec nowrite progbits
section .data
; Constants for syscalls and buffer management
WRITE_FUNC      equ 0x1
PRINT_NUMBER    equ 0x1
STDOUT          equ 0x1

BUFFER_LEN equ 0xFF

NUMBER_ARGS_IN_REGS equ 6

END_SYMBOL           equ 0x0
ARG_SYMBOL           equ '%'
JUMP_TABLE_FIRST_SYM equ 'b'
%define JUMP_TABLE_LEN_FROM_F_TO_N  'n'-'f' - 1
%define JUMP_TABLE_LEN_FROM_N_TO_O  'o'-'n' - 1
%define JUMP_TABLE_LEN_FROM_O_TO_S  's'-'o' - 1
%define JUMP_TABLE_LEN_FROM_S_TO_X  'x'-'s' - 1

DONE_RESULT       equ 0x0
INVALID_SPECIFIER equ 0x1
SYSCALL_ERROR     equ 0x2

STACK_ELEM_SIZE   equ 0x8
QWORD_LEN         equ 0x3

REGISTER_SIZE equ 64
INT_SIZE      equ 32
FLOAT_SIZE    equ 32
DOUBLE_SIZE   equ 64

%define CURRENT_ARGUMENT        qword [r8]
%define INCREASE_ARGUMENT_INDEX add r8, STACK_ELEM_SIZE
%define ARGUMENT_INDEX          r8

%define CURRENT_FLOAT_ARGUMENT         [r9]
%define INCREASE_FLOAT_ARGUMENT_INDEX  add r9, STACK_ELEM_SIZE
%define FLOAT_ARGUMENT_INDEX           r9

%macro START_INDEXING_FLOAT_ARGUMENTS 0
    mov r9, (NUMBER_ARGS_IN_REGS + 2) * STACK_ELEM_SIZE
    add r9, rbp
%endmacro

FLAG_START_NUMBER equ 0x1
FLAG_END_NUMBER   equ 0x0

THIRD_DEGREE  equ 0x3
FOURTH_DEGREE equ 0x4

EAX_PATTERN equ 0x00000000FFFFFFFF

MANTISSA_LEN equ 52
MANTISSA_ONE dq 0x0010000000000000

EXPONENT equ 0b10000000000

FLAG_DEF_RCX equ 0
FLAG_NEG_RCX equ 1

FLAG_PLUS  equ 0
FLAG_MINUS equ 1

FLAG_DEF_VAL  equ 0
FLAG_SPEC_VAL equ 1  ; Flags for special or default value of float number

%define LOC_VAR_NUM_PRINTED qword [rbp - STACK_ELEM_SIZE]
%define LOC_VAR_FLOAT_GOT   qword [rbp - STACK_ELEM_SIZE * 2]
%define INCREASE_FLOAT_COUNTER add qword [rbp - STACK_ELEM_SIZE * 2], 8  ; 8 = SizeOf (label)

%macro ARG_F_GOT_FLOAT_FROM_REG 1
    INCREASE_FLOAT_COUNTER
    movq rax, %1
    call PrintArgF
    jmp .Conditional
%endmacro

%macro GOT_ARGUMENT_TO_RAX 0
    mov rax, CURRENT_ARGUMENT
    INCREASE_ARGUMENT_INDEX
%endmacro

INF db 'inf'
NAN db 'nan'

NAN_EXPONENTA dq 0x7FF0000000000000

%macro SYNCHRONIZATION_ARG_INDEXES 0
    mov rax, ARGUMENT_INDEX
    sub rax, (NUMBER_ARGS_IN_REGS + 2) * STACK_ELEM_SIZE
    cmp rax, rbp
    je .SynchronizationFromAvgToFloat
    ja .SynchronizationFromFloatToAvg
%endmacro

Alphabet:
    db '0123456789ABCDEF'

Buffer:
    db BUFFER_LEN dup (0)

SaveZoneAfterBuffer:
    db 0xFF dup (0)

section .text

global MyPrintf

MyPrintf:
    pop rax  ; Save return address

    push r9  ; Push parameters
    push r8
    push rcx
    push rdx
    push rsi
    push rdi

    push rax

    push rbp
    mov rbp, rsp  ; Create stack frame
    sub rsp, STACK_ELEM_SIZE
    mov LOC_VAR_NUM_PRINTED, 0x0  ; Local variable for number of printed symbols

    sub rsp, STACK_ELEM_SIZE
    mov LOC_VAR_FLOAT_GOT, 0x0

    mov r8, rbp
    add r8, STACK_ELEM_SIZE * 2  ; R8 - pointer to the argument
    mov rsi, qword [r8]  ; Move pointer to string to RSI
    add r8, STACK_ELEM_SIZE

    jmp MyPrintfReal  ; Start real Printf

ExitFunction:
    add rsp, STACK_ELEM_SIZE * 3
    mov rsp, rbp

    pop rbp
    pop rdi

    add rsp, STACK_ELEM_SIZE * 6
    push rdi

    ret

MyPrintfReal:
    xor rcx, rcx

    START_INDEXING_FLOAT_ARGUMENTS

.Conditional:
    cmp byte [rsi], END_SYMBOL
    je .Done  ; Check if end of string is reached

.While:
    cmp byte [rsi], ARG_SYMBOL
    je .PrintArgument  ; Check if an argument is needed

.PrintChar:
    cmp rcx, BUFFER_LEN
    je .BufferEnd

.Continue:
    mov al, byte [rsi]
    mov byte [Buffer + rcx], al
    inc rcx

    inc rsi  ; RSI points to the next symbol in the string
    jmp .Conditional

.Done:
    cmp rcx, 0x0
    jne .LastPrintBuffer

.MovDoneResult:
    mov rax, DONE_RESULT

.Exit:
    jmp ExitFunction

.LastPrintBuffer:
    push rsi
    call PrintBuffer
    pop rsi
    jmp .MovDoneResult

.BufferEnd:
    call PrintBuffer
    jmp .Continue

.PrintArgument:
    inc rsi

    cmp byte [rsi], ARG_SYMBOL
    je .PrintChar
    
    movzx r14, byte [rsi]      ; Use r14 for temp storage
    inc rsi
    cmp r14, 'b'
    jb .InvalidArgument
    cmp r14, 'x'
    ja .InvalidArgument

    ; Optimize with LEA
    lea r15, [.JumpTable]      ; r15 = table base
    movzx rax, r14b
    sub rax, JUMP_TABLE_FIRST_SYM
    jmp [r15 + rax*8]

.InvalidArgument:
    mov rax, INVALID_SPECIFIER
    jmp ExitFunction

.ArgB:

    GOT_ARGUMENT_TO_RAX

    push rsi
    mov rsi, 0x1  ; RSI - power of 2 in the counting system
    call ValToStrPowTwo
    pop rsi

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgC:

    GOT_ARGUMENT_TO_RAX

    call PrintArgC

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgD:

    GOT_ARGUMENT_TO_RAX

    call PrintArgD

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgF:

    mov rax, LOC_VAR_FLOAT_GOT
    add rax, .JumpTableFloat
    mov rax, [rax]
    jmp rax

;--------

.FirstFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm0

;--------

.SecondFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm1

;--------

.ThirdFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm2

;--------

.FourthFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm3

;--------

.FifthFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm4

;--------

.SixthFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm5

;--------

.SeventhFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm6

;--------

.EighthFloat:
    ARG_F_GOT_FLOAT_FROM_REG xmm7

;--------

.FloatInStack:

    movsd xmm4, CURRENT_FLOAT_ARGUMENT
    movq rax, xmm4  ; XMM4 - could be changed according to calling convention
    INCREASE_FLOAT_ARGUMENT_INDEX

    call PrintArgF

    mov rax, ARGUMENT_INDEX
    sub rax, (NUMBER_ARGS_IN_REGS + 2) * STACK_ELEM_SIZE
    cmp rax, rbp

    jae .SynchronizationFromAvgToFloat
    jmp .Conditional

;--------

.JumpTableFloat:
    dq .FirstFloat
    dq .SecondFloat
    dq .ThirdFloat
    dq .FourthFloat
    dq .FifthFloat
    dq .SixthFloat
    dq .SeventhFloat
    dq .EighthFloat
    dq .FloatInStack

;-----------------

.ArgN:

    GOT_ARGUMENT_TO_RAX

    mov rdx, LOC_VAR_NUM_PRINTED
    add rdx, rcx
    mov qword [rax], rdx

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgO:

    GOT_ARGUMENT_TO_RAX

    call ValToStrOct

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgS:

    push rsi
    mov rsi, CURRENT_ARGUMENT
    INCREASE_ARGUMENT_INDEX
    call PrintArgString
    pop rsi

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;-----------------

.ArgX:

    GOT_ARGUMENT_TO_RAX

    push rsi
    mov rsi, FOURTH_DEGREE  ; RSI - power of 2 in the counting system
    call ValToStrPowTwo
    pop rsi

    SYNCHRONIZATION_ARG_INDEXES

    jmp .Conditional

;---------------------------------

.JumpTable:
    dq .ArgB
    dq .ArgC
    dq .ArgD
    dq .InvalidArgument  ; %e
    dq .ArgF
    dq JUMP_TABLE_LEN_FROM_F_TO_N dup (.InvalidArgument)
    dq .ArgN
    dq JUMP_TABLE_LEN_FROM_N_TO_O dup (.InvalidArgument)
    dq .ArgO
    dq JUMP_TABLE_LEN_FROM_O_TO_S dup (.InvalidArgument)
    dq .ArgS
    dq JUMP_TABLE_LEN_FROM_S_TO_X dup (.InvalidArgument)
    dq .ArgX

;---------------------------------

.SynchronizationFromAvgToFloat:
    mov ARGUMENT_INDEX, FLOAT_ARGUMENT_INDEX
    jmp .Conditional

;---------------------------------

.SynchronizationFromFloatToAvg:
    mov FLOAT_ARGUMENT_INDEX,  ARGUMENT_INDEX
    jmp .Conditional

;---------------------------------

;---------------------------------
; Prints buffer to stdout and
; resets RCX to 0
;
; Entry:  Buffer, RCX
; Exit:   Stdout, RCX
; Destrs: RAX, RDI, RSI, RDX, R11
;---------------------------------

PrintBuffer:
    mov rax, WRITE_FUNC
    mov rdi, STDOUT  ; Set syscall parameters
    mov rsi, Buffer
    mov rdx, rcx
    add LOC_VAR_NUM_PRINTED, rcx
    syscall

    cmp rax, rdx
    jne .Error

    xor rcx, rcx

    ret

.Error:
    mov rax, SYSCALL_ERROR
    jmp ExitFunction

;---------------------------------

;---------------------------------
; Moves argument character to buffer
;
; Entry:  RAX, RCX
; Exit:   Stdout, RCX, Buffer
; Destrs: RAX, RBX, RCX, RDX
;---------------------------------

PrintArgC:

    cmp rcx, BUFFER_LEN
    je .BufferEnd

.Continue:
    mov byte [Buffer + rcx], al
    inc rcx
    ret

.BufferEnd:
    call PrintBuffer
    jmp .Continue

;---------------------------------

;---------------------------------
; Translates RAX value to string
;
; Entry:  RAX
; Exit:   Buffer
; Destrs: RAX, RDX
;---------------------------------

PrintArgD:

    push rax
    push rbx
    push rdi
    push rsi

    push rcx
    mov rcx, REGISTER_SIZE
    shr rcx, 1  ; CL = REGISTER_SIZE / 2
    shl rax, cl
    shr rax, cl  ; Prepare DX and AX for division
    pop rcx

    mov rbx, INT_SIZE
    call CheckSign
    mov ebx, 0xA

    xor rdx, rdx

    xor rsi, rsi  ; RSI - Counter of digits in the number

    mov rdi, FLAG_START_NUMBER

.Conditional_1:
    test rax, rax
    je .StopWhile_1

.While_1:
    div ebx
    push rdx
    xor rdx, rdx
    inc rsi
    jmp .Conditional_1

.StopWhile_1:

.Conditional_2:
    test rsi, rsi
    je .StopWhile_2

.While_2:
    pop rax
    call DigitToStr
    dec rsi
    jmp .Conditional_2

.StopWhile_2:

    pop rsi
    pop rdi
    pop rbx
    pop rax

    ret

;---------------------------------

;---------------------------------
; Translates RAX value to string
;
; Entry:  RAX
; Exit:   Buffer
; Destrs: RAX
;---------------------------------

PrintArgF:

    call CheckSpecialFloatMeaning
    cmp r11, FLAG_SPEC_VAL
    je .Skip

    push rax
    push rbx
    push rdx

    mov rbx, DOUBLE_SIZE
    call CheckSign
    push rcx

    cmp rdx, FLAG_MINUS
    je .NegativeNum

    mov rdx, rax
    shr rdx, MANTISSA_LEN
    mov rbx, EXPONENT
    inc rdx
    sub rdx, rbx  ; DX - Exponent

.ContinueNegativeNum:
    mov rbx, rax
    shl rbx, DOUBLE_SIZE - MANTISSA_LEN
    shr rbx, DOUBLE_SIZE - MANTISSA_LEN
    add rbx, qword [MANTISSA_ONE]  ; RBX - Mantissa

    tzcnt rcx, rbx  ; RCX = Number of trailing zeroes in RBX
    shr rbx, cl

    push rbx
    mov rbx, rcx
    mov rcx, MANTISSA_LEN
    sub rcx, rbx
    sub rcx, rdx

    pop rbx

    ; Printed number = RBX * 2 ^ (-RCX)
    ; Printed number = RBX * 5 ^ (RCX) * 10 ^ (-RCX)
    cmp rcx, 0x0
    jl .NegRCX
    je .ZeroRCX

    push rax
    push rdx

    lzcnt rdx, rbx  ; RDX = Number of leading zeroes in RBX
    push rcx  ; Push old RCX
    shl rcx, 1  ; RCX = old RCX * 2
    pop rax  ; RAX = old RCX
    add rcx, rax  ; RCX = old RCX * 3

.ConditionalRoundResult:
    cmp rdx, rcx  ; What is greater?
                  ; Number of leading zeroes in RBX or 3 * RCX
    jae .StopRounding

    sub rcx, rdx
    shr rcx, 2  ; RCX - RDX = (old RCX - new RCX) / 3 + (new RDX - old RDX)
    inc rcx
    shr rbx, cl
    sub rax, rcx

.StopRounding:
    mov rcx, rax

    push rcx
.For:
    imul rbx, 0x5  ; 5 = 10 / 2
    loop .For
    pop rcx

    pop rdx
    pop rax

    mov rax, rbx
    mov rbx, rcx
    pop rcx
    call PrintNumber

.Done:
    pop rdx
    pop rbx
    pop rax

.Skip:
    ret

.NegRCX:
    neg rcx
    shl rbx, cl
    mov rax, rbx
    pop rcx
    call PrintArgD
    jmp .Done

.ZeroRCX:
    mov rax, rbx
    pop rcx
    call PrintArgD
    jmp .Done

.NegativeNum:
    neg rax  ; In function of checking sign RAX was negative
    shl rax, 1
    shr rax, 1
    mov rdx, rax
    shr rdx, MANTISSA_LEN
    mov rbx, EXPONENT
    inc rdx
    sub rdx, rbx  ; DX - Exponent
    jmp .ContinueNegativeNum

;---------------------------------

;---------------------------------
; Checks if RAX is INF or NAN
; If it is NAN or INF, this func will
; set a flag in R11
;
; Entry:  RAX
; Exit:   Buffer
; Destrs: RAX, R11
;---------------------------------

CheckSpecialFloatMeaning:

    push rax

    mov r11, FLAG_DEF_VAL
    and rax, qword [NAN_EXPONENTA]
    cmp rax, qword [NAN_EXPONENTA]
    je .FullExp

.Done:
    pop rax
    ret

.FullExp:
    pop rax
    push rax
    shl rax, REGISTER_SIZE - MANTISSA_LEN
    cmp rax, 0x0
    je .Infinite

    pop rax
    push rax
    shr rax, REGISTER_SIZE - 1
    cmp rax, 1
    je .MinusNAN

    cmp rcx, BUFFER_LEN - 3
    jae .BufferEndPlusNAN

.ContinuePlusNAN:
    mov ax, word [NAN]
    mov word [Buffer + rcx], ax  ; [Buffer + RCX] = 'na'
    mov al, byte [NAN + 2]
    mov byte [Buffer + 2 + rcx], al  ; [Buffer + RCX] = 'nan'
    add rcx, 3

    mov r11, FLAG_SPEC_VAL
    jmp .Done

.BufferEndPlusNAN:
    call PrintBuffer
    jmp .ContinuePlusNAN

.MinusNAN:
    cmp rcx, BUFFER_LEN - 4
    jae .BufferEndMinusNAN

.ContinueMinusNAN:
    mov byte [Buffer + rcx], '-'  
    mov ax, word [NAN]
    mov word [Buffer + 1 + rcx], ax  ; [Buffer + RCX] = '-na'
    mov al, byte [NAN + 2]
    mov byte [Buffer + 3 + rcx], al  ; [Buffer + RCX] = '-nan'
    add rcx, 4

    mov r11, FLAG_SPEC_VAL
    jmp .Done

.BufferEndMinusNAN:
    call PrintBuffer
    jmp .ContinueMinusNAN

.Infinite:
    pop rax
    push rax
    shr rax, REGISTER_SIZE - 1
    cmp rax, 1
    je .MinusINF

    cmp rcx, BUFFER_LEN - 3
    jae .BufferEndPlusINF

.ContinuePlusINF:
    mov ax, word [INF]
    mov word [Buffer + rcx], ax  ; [Buffer + RCX] = 'in'
    mov al, byte [INF + 2]
    mov byte [Buffer + 2 + rcx], al  ; [Buffer + RCX] = 'inf'
    add rcx, 3

    mov r11, FLAG_SPEC_VAL
    jmp .Done

.BufferEndPlusINF:
    call PrintBuffer
    jmp .ContinuePlusINF

.MinusINF:
    cmp rcx, BUFFER_LEN - 4
    jae .BufferEndMinusINF

.ContinueMinusINF:
    mov byte [Buffer + rcx], '-' ; [Buffer + RCX] = '-'
    mov ax, word [INF]
    mov word [Buffer + 1 + rcx], ax  ; [Buffer + RCX] = '-in'
    mov al, byte [INF + 2]
    mov byte [Buffer + 3 + rcx], al  ; [Buffer + RCX] = '-inf'
    add rcx, 4

    mov r11, FLAG_SPEC_VAL
    jmp .Done

.BufferEndMinusINF:
    call PrintBuffer
    jmp .ContinueMinusINF

;---------------------------------

;---------------------------------
; Prints number RAX * 10 ^ (-RBX)
;
; Entry:  RAX, RBX, RCX
; Exit:   Buffer
; Destrs: RAX
;---------------------------------

PrintNumber:

    push rax
    push rdx
    push rbx
    push rdi
    push rsi

    mov rdi, rbx

    mov rbx, 0xA

    xor rdx, rdx

    xor rsi, rsi  ; RSI - Counter of digits in the number

.Conditional_1:
    test rax, rax
    je .StopWhile_1

.While_1:
    div rbx
    push rdx
    xor rdx, rdx
    inc rsi
    jmp .Conditional_1

.StopWhile_1:

    mov rbx, rdi
    cmp rbx, rsi
    jae .ZeroStarted  ; RBX more than length of RAX so number will look like '0.etc'
    jmp .RSImoreRBX

.Conditional_2:
    test rsi, rsi
    je .StopWhile_2

.While_2:
    pop rax
    call DigitToStr
    dec rsi
    jmp .Conditional_2

.StopWhile_2:

    pop rsi
    pop rdi
    pop rbx
    pop rdx
    pop rax

    ret

.ZeroStarted:
; Prints '0.' and then prints zeroes until RBX = RSI. Then it prints the whole number

    cmp rcx, BUFFER_LEN - 1
    jae .BufferEndZeroStarted

.ContinueZeroStarted:
    mov word [Buffer + rcx], '0.'  ; The number starts with '0.'
    add rcx, 2

.Conditional_ZeroStarted:
    cmp rsi, rbx
    je .StopWhile_ZeroStarted

.While_ZeroStarted:
    xor rax, rax
    call DigitToStr
    dec rbx
    jmp .Conditional_ZeroStarted

.StopWhile_ZeroStarted:

    jmp .Conditional_2

.BufferEndZeroStarted:
    push rsi
    call PrintBuffer
    pop rsi
    jmp .ContinueZeroStarted

.RSImoreRBX:
; Prints digits of RAX until RBX = RSI. Then puts '.' and prints the remaining digits

.Conditional_RBX:
    cmp rsi, rbx
    je .StopWhile_RBX

.While_RBX:
    pop rax
    call DigitToStr
    dec rsi
    jmp .Conditional_RBX

.StopWhile_RBX:

    cmp rcx, BUFFER_LEN
    je .BufferEndRSImoreRBX

.ContinueRSImoreRBX:
    mov byte [Buffer + rcx], '.'
    inc rcx
    jmp .Conditional_2

.BufferEndRSImoreRBX:
    push rsi
    call PrintBuffer
    pop rsi
    jmp .ContinueRSImoreRBX

;---------------------------------

;---------------------------------
; Moves sign to the buffer
;
; Entry:  RAX, RBX
; Exit:   Buffer
; Destrs: RBX, RDX
;---------------------------------

CheckSign:

    push rax

    push rcx
    mov rcx, rbx
    sub rcx, 1
    shr rax, cl
    pop rcx

    cmp rax, 1
    je .Minus
    pop rax
    mov rdx, FLAG_PLUS

.Done:
    ret

.Minus:
    cmp rcx, BUFFER_LEN
    je .BufferEnd

.Continue:
    pop rax
    neg rax
    mov byte [Buffer + rcx],  '-'
    inc rcx
    mov rdx, FLAG_MINUS
    jmp .Done

.BufferEnd:
    push rcx
    call PrintBuffer
    pop rcx
    jmp .Continue

;---------------------------------

;---------------------------------
; Translates RAX value to string
; in a number system of 2^RSI
;
; Entry:  RAX, RSI
; Exit:   Buffer
; Destrs: RAX
;---------------------------------

ValToStrPowTwo:

    push rdi
    push rbx
    push rdx
    push rax

    xor rdx, rdx
    mov rbx, REGISTER_SIZE
    sub rbx, rsi
    mov rdi, FLAG_START_NUMBER

.Conditional:
    cmp rdx, REGISTER_SIZE
    jae .StopWhile

.While:
    push rcx
    mov rcx, rdx ; shr/shl rax, r15/r14/... is not working
    shl rax, cl
    mov rcx, rbx
    add rcx, rdx
    shr rax, cl
    pop rcx
    call DigitToStr
    pop rax
    push rax
    sub rbx, rsi
    add rdx, rsi
    jmp .Conditional

.StopWhile:

    pop rax
    pop rdx
    pop rbx
    pop rdi
    ret

;---------------------------------

;---------------------------------
; Translates RAX value to string
; in octal number system
;
; Entry:  RAX
; Exit:   Buffer
; Destrs: RAX, RDI
;---------------------------------

ValToStrOct:

    push rdi
    push rbx
    push rdx
    push rax

    xor rdx, rdx
    mov rbx, REGISTER_SIZE
    sub rbx, 1  ; 64 mod 3 = 1, where 64 bit - size of register, 3 - power of 2
    mov rdi, FLAG_START_NUMBER

.Conditional:
    cmp rdx, REGISTER_SIZE
    jae .StopWhile

.While:
    push rcx
    mov rcx, rdx
    shl rax, cl
    mov rcx, rbx
    add rcx, rdx
    shr rax, cl
    pop rcx
    call DigitToStr
    pop rax
    push rax
    sub rbx, THIRD_DEGREE
    test rdx, rdx
    je .FirstDigit
    add rdx, THIRD_DEGREE
    jmp .Conditional

.StopWhile:
    pop rax
    pop rdx
    pop rbx
    pop rdi
    ret

.FirstDigit:
    add rdx, 1  ; 64 mod 3 = 1, where 64 bit - size of register, 3 - power of 2
    jmp .Conditional

;---------------------------------

;---------------------------------
; Translates number from AL to
; string
;
; Entry:  RAX, RDI, RCX
; Exit:   Buffer
; Destrs: RAX, RDI
;---------------------------------

DigitToStr:

    cmp rcx, BUFFER_LEN
    je .BufferEnd

.Continue:
    cmp rdi, FLAG_START_NUMBER
    je .SkipZeroes

.DontSkip:
    mov al, byte [Alphabet + rax]; 2 more strs for lodsb stosb
    mov byte [Buffer + rcx], al
    inc rcx

.Skip:
    ret

.SkipZeroes:
    test rax, rax
    je .Skip
    mov rdi, FLAG_END_NUMBER
    jmp .DontSkip

.BufferEnd:
    push rax
    push rbx
    push rdx
    push rsi
    call PrintBuffer
    pop rsi
    pop rdx
    pop rbx
    pop rax
    jmp .Continue

;---------------------------------

;---------------------------------
; Prints string pointed by RSI
;
; Entry:  RSI
; Exit:   Stdout
; Destrs: RAX, RDX, RDI
;---------------------------------

PrintArgString:

    push rsi

    call PrintBuffer

    pop rsi

.Conditional:
    cmp byte [rsi], END_SYMBOL
    je .ExitWhile

.While:
    inc rsi
    inc rcx
    jmp .Conditional

.ExitWhile:
    mov rax, WRITE_FUNC
    mov rdi, STDOUT  ; Set syscall parameters
    sub rsi, rcx
    mov rdx, rcx
    add LOC_VAR_NUM_PRINTED, rcx
    syscall

    cmp rax, rdx
    jne .Error

    xor rcx, rcx

    ret

.Error:
    mov rax, SYSCALL_ERROR
    jmp ExitFunction

;---------------------------------

;--------------------------------------------