# bugfree-computing-machine
bugfree-computing-machine
=========================

bugfree computing machine is a suite for the eXtended Stack Machine. 

The eXtended Stack Machine is a simplified CPU architecture created by Timothy 
V. Fossum for educational purposes. Documentation is stored in `doc/`. 

I hope to combine bcm into one standalone binary and build it with cabal soon.
In the meantime, bcm is divided into four separate programs: 

hsxx
----

hsxx is an emulator for the eXtended Stack Machine. It is documented in 
`doc/assembler-doc.html`. Build it with `ghc Hsxx.hs` and run it with 
`./Hsxx <file>`. eXtended Stack Machine executables typically end with `.sxx`.

hsxxl
=====

hsxxl is a linker for eXtended Stack Machine object modules. Build it with 
`ghc Hsxxl.hs` and run it with `./Hsxxl <file> [<files>...]`. eXtended Stack 
Machine object modules typically end with `.out`.

hsax
====

hsax is an assembler for eXtended Stack Machine assembly. Build it with 
`ghc Hsax.hs` and run it with `./Hsax <file>`. eXtended Stack Machine assembly
files typically end with `.sax`.

bcmcc
=====

bcmcc is a compiler for the C language targeting eXtended Stack Machine 
assembly. Build it with `ghc Compiler.hs` and run it with `./Compiler <file>`.
C source files typically end with `.c`. 

Credit
======

Credit goes to Dr. Timothy V. Fossum for giving birth to the eXtended Stack
Machine. If not for him, such a creation would not exist. Credit also goes to
Edward Banner for his own implementation of the eXtended Stack Machine: 
https://github.com/ebanner/extended_stack_machine/blob/master/README.md

