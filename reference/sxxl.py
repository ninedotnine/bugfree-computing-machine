#!/usr/bin/python

# Stack Machine object module linker

import sys
import os
import tempfile

# getopts sanity check

argv = sys.argv[1:]

PASS = 0
file = None
TEXT_LENGTH = None
SYMTAB = dict() # public symbols, with addresses and external reference points
symtab = dict() # public symbols, with addresses and module names
ENTRY = None
ENTRY_FILE = None
i = None
tempf = None

symbol = None 
symaddr = None 
addrs = None 
atinvlist = []
hashinvlist = dict()
reldict = []


def death(msg=None): 
    if tempf is not None:
        os.remove(tempf)
    if msg is not None:
        print(msg)
    sys.exit(0)

def passe(args): # 'pass' is a python reserved word
    global atinvlist
    print(args)
    LC = 0
    # args is a list
    fileno = 0
    global PASS 
    PASS += 1

    # $pref = shift @invlist
    pref = None
    global TEXT_LENGTH
    TEXT_LENGTH = 0

    global file
    for file in args: 
        file_id = "#MAIN" + str(fileno)
        fileno += 1
#         print("file: " + file)
#         print("file_id: " + file_id)

        fn = file
        print("fn is: " + fn)
        if file == "-":
            if tempf is None:
                copySTDIN()
            fn = tempf
            file = "STDIN"

        FILE = open(fn);
        if PASS == 1: 
            SYMTAB[file_id] = [ TEXT_LENGTH ]
        else: 
            print("# File " + file)
            print("# Offset " + str(TEXT_LENGTH))

        # header
        if not (FILE.readline().startswith("%SXX+O")):
            badformat()

        # text length
        dan_nextLine = FILE.readline()
        while dan_nextLine.lstrip().startswith("#"):
            dan_nextLine = FILE.readline()
#         text_length = int(dan_nextLine.split(maxsplit=1)[0])
        text_length = getint(dan_nextLine) 
        print("# found text length: " + str(text_length))

        if not (FILE.readline().startswith("%")):
            badformat()

        # text (code)
        lc = 0
        while True: 
            dw = False
            dan_nextLine = FILE.readline().lstrip()
#             print("'next: " + dan_nextLine)
            if dan_nextLine.startswith("#"):
                continue
            if dan_nextLine.startswith("%"):
                break
            if dan_nextLine.startswith(":"):
                dw = True # colon lines are DWs ### ACTUALLY DSes THOUGH?? ###
#                 print(dan_val)
            if dw: 
                dan_val = getint(dan_nextLine[1:])
                if not dan_val >= 0:
                    badformat
                if (PASS == 2 and dan_val > 0):
                    print(":" + dan_val)
                lc += dan_val
                LC += dan_val
            else:
                dan_val = getint(dan_nextLine)
                if (PASS == 2):
#                     print("LC is: " + str(LC))
                    if atinvlist: # empty lists are false
                        pref = atinvlist[0]
                    if pref is not None:
                        print("pref is: " + str(pref))
                        (addr, ref, sym) = pref
                        if addr < LC:
                            death("external fixup address $addr for symbol $sym\ndoes not match object module location\n")
                        if addr == LC:
                            dan_val += ref
                            atinvlist = atinvlist[1:]
                        pref = None
                    print(dan_val)
                    lc += 1
                    LC += 1

        if not dan_nextLine.startswith("%"):
            badformat
        if not (lc == text_length): 
            badformat

        # relocation dictionary
        while True: 
            dan_nextLine = FILE.readline().strip()
            if dan_nextLine.startswith("#"):
                continue
            if dan_nextLine.startswith("%"):
                break
            dan_val = getint(dan_nextLine)
            if dan_val < 0 or dan_val >= text_length:
                death("illegal relocation entry")
            reloc = dan_val + TEXT_LENGTH
            if PASS == 1:
                SYMTAB[file_id].append(reloc)

        # public/extern/entry definitions
        if not dan_nextLine.startswith("%"):
            badformat

        while True: 
            dan_nextLine = FILE.readline().strip()
            if dan_nextLine.startswith("#"):
                continue
            if dan_nextLine.startswith("%"):
                break
            if PASS == 2:
                continue
            if dan_nextLine.startswith("ENTRY"):
                eep = dan_nextLine.split()[1:]; # skip 'ENTRY'
                global ENTRY
                if ENTRY is not None:
                    death("ENTRY already defined in " + ENTRY_FILE);
                symbol = eep[0]
                addr = int(eep[1])
                if addr < 0 or addr > text_length:
                    death("ENTRY address must be positive")
                if len(eep) > 2:
                    print("len(eep) > 2")
                    badformat
                addr += TEXT_LENGTH
                ENTRY = addr
                ENTRY_FILE = file
                continue
            elif dan_nextLine.startswith("EXTERN"):
                eep = dan_nextLine.split()[1:] # skip 'EXTERN'
                symbol = eep[0]
#                 eep = list(map(int, eep[1:]))
                eep = [int(x) for x in eep[1:]]
                for i in range(0, len(eep)):
                    eep[i] += TEXT_LENGTH
#                 symaddr = SYMTAB{symbol}
                if symbol not in SYMTAB.keys():
                    SYMTAB[symbol] = [ -1 ]
                SYMTAB[symbol].extend(eep)
                continue
            elif dan_nextLine.startswith("PUBLIC"):
                eep = dan_nextLine.split()[1:] # skip 'PUBLIC'
                symbol = eep[0]
                addr = int(eep[1])
                addr += TEXT_LENGTH
#                 symtab[symbol] = [ addr, file ]
                symtab[symbol] = ( addr, file )
                if symbol not in SYMTAB.keys():
                    SYMTAB[symbol] = [ addr ]
                elif SYMTAB[symbol][0] != -1: 
                    death("PUBLIC symbol already defined")
                else:
                    SYMTAB[symbol][0] = addr
                continue
            print("not one of ENTRY, EXTERN, PUBLIC")
            badformat()
                    
        # this ends it all
        if not dan_nextLine.startswith("%"):
            badformat()
        TEXT_LENGTH += text_length
        FILE.close()


    # check for any additional fixup addresses
#     if PASS == 2: # && defined $pref # FIXME
    if PASS == 2 and atinvlist: # atinvlist is true if nonempty
        print("")
        death("fixup addresses exceed module size")

def badformat():
#     death("File $file line $. is not in proper object module format\n")
    death("File " + str(file) + 
            " line $. is not in proper object module format\n")

def getint(str):
#     return int(readline(FILE))
#     print("STR IS: " + str)
    return int(str.split(maxsplit=1)[0])

def copySTDIN():
    global tempf 
    (handle, tempf) = tempfile.mkstemp(prefix="sxxl.temp")
    print("tempf is: " + tempf)
    linecount = 0
    handle = os.fdopen(handle, 'w')
    for line in sys.stdin:
        handle.write(line)
        linecount += 1
    if not linecount > 0:
        death("empty input")

if len(argv) == 0:
    argv = [ '-' ] # '-' represents standard input
    
print(argv)

passe(argv)

print("%SXX+Executable")
print("date goes here")

print(str(TEXT_LENGTH) + " text length")

if ENTRY is not None:
#     print(str(ENTRY) + " ENTRY file=" + str(ENTRY_FILE))
    print(str(ENTRY) + " ENTRY file=" + str(ENTRY_FILE))
else:
    print("0 ENTRY (default)")

print(symtab)
public_count = 0
for i in sorted(symtab.keys()):
    if public_count == 0:
        print("#PUBLIC symbols and addresses")
    public_count += 1
#     [ addr , file ] = symtab[i]
    ( addr , file ) = symtab[i]
    print("# PUBLIC " + str(i) + " " + str(addr) + "file=" + file)

print("% text")

# wtf? this program seems to be nondeterministic without this sorted() call
print("SYMTAB is: -----------------------------")
print(SYMTAB)
for symbol in SYMTAB.keys():
# for symbol in sorted(SYMTAB.keys()):
    addrs = SYMTAB[symbol]
    print("addrs = " + str(addrs))
    symaddr = addrs.pop(0)
    if symaddr < 0 and len(addrs) > 0:
        death("EXTERN " + symbol + " not found in PUBLIC declarations")
    for i in addrs:
        print("i is : " + str(i))
        # is this the array or dict called 'invlist' ??
        # it must be the hash, since it will be used sooner
        hashinvlist[i] = [symaddr, symbol]

lastaddr = -1
lastsymbol = None

print("%INVLIST:")
print(hashinvlist)

for i in sorted(hashinvlist.keys()):
    (symaddr, symbol) = hashinvlist[i]
    if lastaddr == i:
        death("external fixup address " + i + " for symbol " + symbol + 
                " appears more than once\n(previous entry for symbol was " 
                + lastsymbol)
    lastaddr = i
    lastsymbol = symbol
    atinvlist.append( (i, symaddr, symbol) )
    reldict.append(i)

print("INVLIST:")
print(atinvlist)

print("BEGIN PASS 2")
passe(argv)

print("% relocation dictionary")
for rel in reldict:
    print(rel)

print("% end of Executable module")
