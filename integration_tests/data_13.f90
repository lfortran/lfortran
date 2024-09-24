program data_13
   character:: c(6)*3
   integer :: i
   data c/'ab','cd','ef','gh','ij','kl'/
   do i = 1, 6
      print *, c(i), len(c(i))
      if (len(c(i)) /= 3) error stop
   end do
end program data_13

