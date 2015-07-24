module Instructions (Instruction(..)) where 
-- this is all this module is for.

data Instruction = BKPT
--                  | PUSH Int
--                  | PUSHV Int
                 | PUSH
                 | PUSHV
                 | PUSHS
--                  | PUSHX Int
--                  | POP Int
                 | PUSHX
                 | POP
                 | POPS
--                  | POPX Int
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
--                  | BNE Int
--                  | BF Int
--                  | BR Int
                 | BNE
                 | BEQ
                 | BR
--                  | CALL Int Comment -- name of the call maybe?
                 | CALL
                 | CALLS
                 | RETURN
--                  | RETN Int
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
--                  | ADDX Int
--                  | ADDSP Int
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
