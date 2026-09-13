program functions_63
   implicit none
   integer :: a
   integer :: b0(2,2)
   a=10
   b0=-huge(0)
   call change_values ( a, flatten(b0) )
   if (a /= 14) error stop 1
   if (.not. all(b0 == reshape([10, 11, 12, 13], [2, 2]))) error stop 2

contains
   subroutine change_values( a, b)
      integer, intent(inout) :: a
      integer, intent(out)   :: b(:)
      integer                :: i
      do i=1,size(b)
         b(i) = a
         a = a + 1
      end do
   end subroutine change_values

   function flatten(arr) result(p_arr)
      use, intrinsic :: iso_c_binding
      integer,target, contiguous :: arr(..)
      integer,pointer            :: p_arr(:)
      integer                    :: n
      n=size(arr)
      select rank (arr)
      rank (0);
      call c_f_pointer(c_loc(arr), p_arr, [1])
      rank (1);     p_arr(1:n)=>arr
      rank (2);     p_arr(1:n)=>arr
      rank (*)
      print *, 'assumed size is unsupported'
      rank default
      print *, 'unsupported rank'
   end select
end function flatten

end program functions_63