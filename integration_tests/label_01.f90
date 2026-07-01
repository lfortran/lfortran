program label_01
  implicit none

  character(len=1) :: c
  character(len=1) :: buf
  logical :: lsw

  buf = "a"
  lsw = .false.

  if (lsw) then
    c = "x"
  else
    read(buf(1:1), "(a1)", end=10) c
    goto 20
10 continue
    error stop
20 continue
  end if

  if (c /= "a") error stop
end program label_01
