zero: .byte 0
t1:   .byte 0
t2:   .byte 0
t3:   .byte 0

loop:
  # cap  = byte AND 32
  read
  store t1
  and   32
  store t2

  # byte = byte AND (NOT cap)
  not
  andM  t1
  store t1

  # if (byte < "A" or byte > "Z") goto next
  add   -65
  jltz next
  add   -25
  jgtz next
  add    90

  # byte - "A" + 13
  add  -52
  store t3

  # (byte - "A" + 13) % 26
  div  26
  mul -26
  addM t3

  # byte = (byte - "A" + 13) % 26 + "A"
  add   65
  store t1

next:
  # write(byte OR cap)
  load t1
  orM  t2
  write

j loop
