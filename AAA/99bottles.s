# Main
setup:
  add 100
  store n
  load zero
  store ones
  add 10
  store tens

loop:
  load n
  add -1
  store n
  jz exit

  load ones
  add -1
  store ones
  jltz adjustNum

  adjustDone:
    load n
    add -99
    jz onTheWall

  takeOneDown:
    call printTake
    call printNum
    call printSpace
    call printBottleOrBottles
    call printSpace
    call printWall
    call printDot
    call printNewline

  onTheWall:
    call printNum
    call printSpace
    call printBottleOrBottles
    call printSpace
    call printWall
    call printComma
    call printNum
    call printSpace
    call printBottleOrBottles
    call printDot

  j loop

exit:
  call printTake
  call printNomore
  call printBottles
  call printSpace
  call printWall
  call printDot
  call printNewline
  call printNomore1
  call printBottles
  call printSpace
  call printWall
  call printComma
  call printNomore
  call printBottles
  call printDot
  call printFinal
  halt



# Helpers
adjustNum:
  load zero
  add 9
  store ones
  load tens
  add -1
  store tens
  j adjustDone



# Procedures
printDot:
  load dot
  write
  load newline
  write
  ret

printSpace:
  load space
  write
  ret

printComma:
  load comma
  write
  load space
  write
  ret

printNewline:
  load newline
  write
  ret

# Put length in t1
# Put offset in t2
print:
  load t1
  mul -1

  printLoop:
    jz printDone
    store t1

    load t2
    loadM
    write
    load t2
    add 16
    store t2

    load t1
    add 1
    j printLoop

  printDone:
    ret

printBottle:
  load zero
  add 14
  store t1
  load zero
  add bottle
  store t2
  j print

printBottles:
  load zero
  add 15
  store t1
  load zero
  add bottles
  store t2
  j print

printBottleOrBottles:
  load n
  add -1
  jz printBottle
  j printBottles

printWall:
  load zero
  add 11
  store t1
  load zero
  add wall
  store t2
  j print

printTake:
  load zero
  add 34
  store t1
  load zero
  add take
  store t2
  j print

printNomore:
  load zero
  add 8
  store t1
  load zero
  add nomore
  store t2
  j print

printNomore1:
  load zero
  add 8
  store t1
  load zero
  add nomore1
  store t2
  j print

printFinal:
  load zero
  add 66
  store t1
  load zero
  add final
  store t2
  j print

printNum:
  load tens
  jz printOnes
  add 48
  write
  printOnes:
    load ones
    add 48
    write
    ret



# Global storage
zero:     .byte 0
n:        .byte 0
tens:     .byte 0
ones:     .byte 0

# Temporary storage
t1:       .byte 0
t2:       .byte 0

# Data segment
dot:      .ascii "."
space:    .ascii " "
comma:    .ascii ","
newline:  .byte 10
bottle:   .ascii "bottle of beer"
bottles:  .ascii "bottles of beer"
wall:     .ascii "on the wall"
take:     .ascii "Take one down and pass it around, "
nomore:   .ascii "no more "
nomore1:  .ascii "No more "
final:    .ascii "Go to the store and buy some more, 99 bottles of beer on the wall!"
