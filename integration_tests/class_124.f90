module abstypemod_124
   implicit none

   type :: AbsType
      procedure(pintfc), pointer, nopass :: ptr => null()
   end type AbsType

   interface
      function pintfc(n) result(nres)
         integer, intent(in) :: n(:)
         integer :: nres(size(n))
      end function pintfc
   end interface

end module abstypemod_124

module clientmod_124
   use abstypemod_124, only: AbsType
   implicit none

contains

   subroutine client(obj)
      class(AbsType), intent(in) :: obj
      integer, allocatable :: nres(:)
      allocate(nres(3))
      nres = obj%ptr(nres)
   end subroutine client

end module clientmod_124

program class_124
   use abstypemod_124, only: AbsType
   use clientmod_124, only: client
   implicit none

   type(AbsType) :: obj
   obj%ptr => double_it
   call client(obj)
   print *, "ok"

contains

   function double_it(n) result(nres)
      integer, intent(in) :: n(:)
      integer :: nres(size(n))
      integer :: i
      do i = 1, size(n)
         nres(i) = n(i) * 2
      end do
   end function double_it

end program class_124
