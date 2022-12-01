.intel_syntax noprefix
.global _calorie_counting

.text
/* ------------------------ MAIN ------------------------ */
// rdi = pointer to data file
// r8 = maximum number of calories
// r9 = second maximum
// r10 = third maximum
// r11 = temporal block calories count

_calorie_counting:
    xor r8, r8
    xor r9, r9
    xor r10, r10

.L_calorie_counting_loop:
    // stop
    cmp byte ptr [rdi], 0
    je .L_calorie_counting_end
    // loop per block
    call .L_calorie_counting_block
    inc rdi
    // compare maximum 
    cmp r8, rax
    jg .L_calorie_counting_second
    xchg r8, rax

.L_calorie_counting_second:
    cmp r9, rax
    jg .L_calorie_counting_third
    xchg r9, rax

.L_calorie_counting_third:
    cmp r10, rax
    jg .L_calorie_counting_loop
    mov r10, rax
    jmp .L_calorie_counting_loop

.L_calorie_counting_end:
    // max in higher 32 bits, sum of three maxes in lower 32 bits
    mov rax, r8
    shl rax, 32
    add rax, r8
    add rax, r9
    add rax, r10
    ret

// Helper: count calories in a block
.L_calorie_counting_block:
    xor r11, r11
.L_calorie_counting_block_loop:
    cmp byte ptr [rdi], 0xa
    je .L_calorie_counting_block_end
    // get int
    call .L_get_int
    // add to total
    add r11, rax
    inc rdi
    jmp .L_calorie_counting_block_loop
.L_calorie_counting_block_end:
    mov rax, r11
    ret

// Helper: get int
.L_get_int:
    xor rax, rax
.L_get_int_loop:
    // stop
    cmp byte ptr [rdi], 0xa
    jz .L_get_int_end
    // get int
    xor rdx, rdx
    mov dl, byte ptr [rdi]
    sub dl, 48
    cmp dl, 9
    ja .L_get_int_end
    // add to rax
    imul rax, 10
    add rax, rdx
    // next
    inc rdi
    jmp .L_get_int_loop

.L_get_int_end:
    ret