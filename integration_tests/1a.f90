program test

integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
character(len=20, kind=ucs4) :: s

s = ucs4_"Hello"
print *, s

s = ucs4_"你好"
print *, s

s = ucs4_"Hello 你好"
print *, s

end program