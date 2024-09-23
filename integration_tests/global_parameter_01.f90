program a
   use iso_fortran_env
   integer :: i
   do i = 1, size(real_kinds)
      print *, real_kinds(i)
   end do
end program a
