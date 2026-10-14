subroutine data_26_count(n_saved, n_implied)
   implicit none
   integer, intent(out) :: n_saved, n_implied
   integer :: icall, jcall
   save icall
   data icall / 0 /, jcall / 0 /
   icall = icall + 1
   jcall = jcall + 1
   n_saved = icall
   n_implied = jcall
end subroutine data_26_count

program data_26
   implicit none
   integer :: n_saved, n_implied, k
   do k = 1, 3
      call data_26_count(n_saved, n_implied)
      if (n_saved /= k .or. n_implied /= k) error stop
   end do
   print *, n_saved, n_implied
end program data_26
