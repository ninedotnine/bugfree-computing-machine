stuff equ 072 ; 58

main:   call prompt
        read
        call prompt
        read
        add
        print
        pushv 10
        printc
        halt

prompt: pushv '>
        printc
        return
