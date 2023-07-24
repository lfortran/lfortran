program implied_do_loops4
   integer i

   integer :: x(10)
   x = [(i * 7, i = 1, 10)]

   do i = 1, 10
      if (x(i) /= i * 7) error stop
   end do

   print *, "x = ", x
   print *, "direct = ", [(i * 7, i = 1, 10)]
   print *, "direct 2 = ", (i * 7, i = 1, 10)

end program

