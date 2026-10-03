program c_f_pointer_01
   use iso_c_binding, only: c_ptr, c_f_pointer, c_loc
   implicit none
   integer :: j
   real, target :: r_data(3, 4)
   real, pointer :: r_ptr(:,:)
   type(c_ptr) :: cptr
   integer, allocatable :: shp(:)

   r_data = reshape([(real(j), j=1,12)], [3, 4])
   allocate(shp(2))
   shp = [3, 4]

   cptr = c_loc(r_data(1,1))
   call c_f_pointer(cptr, r_ptr, shp)
   if (size(r_ptr, 1) /= 3 .or. size(r_ptr, 2) /= 4) error stop
   if (r_ptr(1,1) /= 1.0 .or. r_ptr(3,4) /= 12.0) error stop
   print *, "Ok"
end program c_f_pointer_01
