stuff equ 0xa2 ; 162

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
