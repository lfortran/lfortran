program save_12
   implicit real(kind=8) (a-h,o-z), integer(kind=4) (i-n)
   save notyet
   logical :: notyet = .true.
   if (.not. notyet) error stop
   print *, notyet
end program save_12
