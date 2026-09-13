program format_hollerith_02
implicit none

! A Hollerith edit descriptor `nH...` carries n literal characters. Those
! characters must not be scanned as edit descriptors, even when they look
! like ones (issue #12254).

character(len=64) :: str

write (str, 100)
if (str /= "Hello World!") error stop

write (str, 200) 42
if (str /= "value = 42") error stop

write (str, 300)
if (str /= "a,b(x)") error stop

100 format (12hHello World!)
200 format (8hvalue = ,i0)
300 format (6ha,b(x))
end program format_hollerith_02
