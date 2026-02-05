# i have written this a while ago for testing jumps, and memory
# jumps were never implemented, i dont remember is this algorithm implementation finished

@bubbleSort:
    mov r4, @dataToSort
    lod4 r30, @dataToSortLen
    add r5, r4, r30 << 2 # r5 now holds pointer to the end of dataToSortVal
    jmp @bubbleSort_afterSwap
@bubbleSort_comparission:
    lod4 r6, r4[0]
    lod4 r7, r4[4]
    jmp.les @bubbleSort_afterSwap, r6, r7
    str4 r7, r4[0]
    str4 r6, r4[4]
@bubbleSort_afterSwap:
    jmp.leq @bubbleSort_end, r5, r4
    add r4, 4
    jmp @bubbleSort_comparission
@bubbleSort_end:
    ret

# u4 is a 4 bytes (32b) long unsigned integer
@dataToSortLen:
    .binary u4 { 4 }
@dataToSortVal:
    .binary u4 { 25, 16, 32, 56 }
