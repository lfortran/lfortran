! Standard Fortran counterpart of traits_runtime_elemental_02: the trait is
! an abstract type whose deferred binding is elemental.
module traits_runtime_elemental_02_oracle_m

   type, abstract :: IFloors
   contains
      procedure(pfloor_interface), deferred :: pfloor
   end type IFloors

   abstract interface
      elemental function pfloor_interface(self,p) result(res)
         import :: IFloors
         class(IFloors), intent(in) :: self
         real(8),            intent(in) :: p
         real(8)                        :: res
      end function pfloor_interface
   end interface
   
   type, extends(IFloors) :: Floors
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
   
end module traits_runtime_elemental_02_oracle_m

program main

   use traits_runtime_elemental_02_oracle_m

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
