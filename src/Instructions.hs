module Instructions (Instruction(..)) where 
-- this is all this module is for.

data Instruction = 
      BKPT
    | PUSH
    | PUSHV
    | PUSHS
    | PUSHX
    | POP
    | POPS
    | POPX
    | DUPL
    | SWAP
    | OVER
    | DROP
    | ROT
    | TSTLT
    | TSTLE
    | TSTGT
    | TSTGE
    | TSTEQ
    | TSTNE
    | BNE
    | BEQ
    | BR
    | CALL
    | CALLS
    | RETURN
    | RETN
    | HALT
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | OR
    | AND
    | XOR
    | NOT
    | NEG
    | ADDX
    | ADDSP
    | READ
    | PRINT
    | READC
    | PRINTC
    | TRON
    | TROFF
    | DUMP
    | ERROR
    deriving (Show, Read, Enum)
