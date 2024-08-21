.section .text

.globl _exit
_exit:
    li a7, 0
    ecall
    ret

.globl _open
_open:
    li a7, 1
    ecall
    ret

.globl _read
_read:
    li a7, 2
    ecall
    ret

.globl _write
_write:
    li a7, 3
    ecall
    ret

.globl _close
_close:
    li a7, 4
    ecall
    ret

.globl _stat
_stat:
    li a7, 5
    ecall
    ret

.globl epoch
epoch:
    li a7, 6
    ecall
    ret

.globl access
access:
    li a7, 7
    ecall
    ret

.globl usleep
usleep:
    li a7, 8
    ecall
    ret

.globl _lseek
_lseek:
    li a7, 9
    ecall
    ret

.globl setGraphicsMode
setGraphicsMode:
    li a7, 10
    ecall
    ret

.globl updatePalette
updatePalette:
    li a7, 11
    ecall
    ret

.globl updateScreen
updateScreen:
    li a7, 12
    ecall
    ret

.globl getEvent
getEvent:
    li a7, 13
    ecall
    ret

.globl I_GetTime
I_GetTime:
    li a7, 14
    ecall
    ret

/*.globl FixedMul
FixedMul:
    mul t0, a0, a1
    mulh t1, a0, a1
    srli t0, t0, 16
    srai a0, t1, 16
    li t1, 0xFFFF
    and t0, t0, t1
    or a0, a0, t0
    ret*/
