! An elemental trait message applied to an array through an owner, as
! reported: p(:) = obj%pfloor(p(:)) calls the slot once per element with the
! receiver fixed. The program is the reporter's, with its module renamed and
! checks added. Floors() is the default structure constructor of a type whose
! only component is private and default-initialized.
! traits_runtime_elemental_02_oracle.f90 is the standard-Fortran counterpart.
module traits_runtime_elemental_02_m

   abstract interface :: IFloors
      elemental function pfloor(self,p) result(res)
         type(itself), pass, intent(in) :: self
         real(8),            intent(in) :: p
         real(8)                        :: res
      end function pfloor
   end interface
   
   type, implements(IFloors) :: Floors
      private
      real(8) :: smallp = 1.d-50
   contains
      procedure :: pfloor
   end type Floors

contains

   elemental function pfloor(self,p) result(res)
      class(Floors), intent(in) :: self
      real(8),       intent(in) :: p
      real(8)                   :: res
      res = max(p,self%smallp)
   end function pfloor
   
end module traits_runtime_elemental_02_m

program main

   use traits_runtime_elemental_02_m

   integer :: i
   real(8) :: p(5)
   class(IFloors), allocatable :: obj

   obj = Floors()

   p = 1.d2
   
   write(*,*) 'before floor:'
   do i = 1, 5
      write(*,*) p(i)
   end do
   write(*,*)

   p(:) = obj%pfloor(p(:))
   
   write(*,*) 'after floor:'
   do i = 1, 5
      write(*,*) p(i)
   end do

   if (any(p /= 1.d2)) error stop 1
   p = [1.d2, -1.d0, 0.d0, 1.d-60, 2.d-50]
   p(:) = obj%pfloor(p(:))
   if (any(p /= [1.d2, 1.d-50, 1.d-50, 1.d-50, 2.d-50])) error stop 2
   if (obj%pfloor(-3.d0) /= 1.d-50) error stop 3
   ! The private component keeps its default initialization.
   deallocate(obj)
   allocate(obj, source=Floors())
   if (obj%pfloor(1.d-60) /= 1.d-50) error stop 4
   if (any(obj%pfloor([1.d-70, 3.d-50]) /= [1.d-50, 3.d-50])) error stop 5
   
end program main
