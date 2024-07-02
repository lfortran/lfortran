program do_loop_05
integer :: a, b, res
b = 5
res = 0
do 1 a = b, 1, -b
      res = res + a
1 continue

print *, res
if ( res /= 5 ) error stop
end program
