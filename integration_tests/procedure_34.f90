module procedure_34_mod
   implicit none

   abstract interface
      subroutine pintfc(i, a, b, c, d)
         integer(4),                  intent(in)  :: i
         real(8), dimension(i:,:),    intent(in)  :: a
         real(8), dimension(i:,:),    intent(in)  :: b, c
         real(8), dimension(i:,:,:,:),intent(out) :: d
      end subroutine pintfc
   end interface
   
  contains      
   
   subroutine client(n, arr, pptr)
      integer(4),        intent(in) :: n
      real(8),           intent(in) :: arr(n:,n:,:)
      procedure(pintfc), pointer, optional :: pptr

      real(8) :: a(2:5, 4)
      real(8) :: b(2:5, 3)
      real(8) :: c(2:5, 3)
      real(8) :: d(2:5, 4, 4, 4)

      a = 5.5
      b = 6.6
      c = 7.7

      if (present(pptr)) then
            call pptr(2, a, b, c, d)
            if (any((d-1.1) >1e-8)) error stop
      else 
        error stop
      end if
   end subroutine client

   ! This routine implements pintfc and performs the safety checks
   subroutine verify_pintfc_args(i, a, b, c, d)
      integer(4),                  intent(in)  :: i
      real(8), dimension(i:,:),    intent(in)  :: a
      real(8), dimension(i:,:),    intent(in)  :: b, c
      real(8), dimension(i:,:,:,:),intent(out) :: d
      ! 1. Check if 'i' matches the hardcoded call (0)
      if (i /= 2) error stop

      ! 2. Check Lower Bounds (should be i, which is 0)
      if (lbound(a, 1) /= 2) error stop
      
      print *, size(a, 1), size(a, 2)
      if (size(a, 1) /= 4 .or. size(a, 2) /= 4) error stop
      if (any(abs(a-5.5) >1e-8)) error stop

      print *, size(b, 1), size(b, 2)
      if (size(b, 1) /= 4 .or. size(b, 2) /= 3) error stop
      if (any(abs(b - 6.6) > 1e-8)) error stop

      print *, size(c, 1), size(c, 2)
      if (size(c, 1) /= 4 .or. size(c, 2) /= 3) error stop
      if (any(abs(c - 7.7) > 1e-8)) error stop

      print *, size(d, 1), size(d, 2), size(d, 3), size(d, 4)
      if (size(d, 1) /= 4 .or. size(d, 2) /= 4 .or. &
          size(d, 3) /= 4 .or. size(d, 4) /= 4) then
          error stop 
      end if
      
      d = 1.1
   end subroutine verify_pintfc_args

end module procedure_34_mod

program procedure_34
   use procedure_34_mod
   implicit none

   integer(4) :: n = 1
   real(8)    :: my_arr(1:5, 1:5, 1:3)
   
   procedure(pintfc), pointer :: my_ptr => null()

   my_ptr => verify_pintfc_args
   call client(n, my_arr, my_ptr)

   print *, "Done"
end program procedure_34