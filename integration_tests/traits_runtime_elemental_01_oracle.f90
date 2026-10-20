! Standard Fortran counterpart of traits_runtime_elemental_01: the trait is
! an abstract type whose deferred binding is elemental.
module traits_runtime_elemental_01_oracle_m

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

   interface Floors
      procedure :: init
   end interface Floors

contains

   function init() result(self)
      type(Floors) :: self
   end function init
  
   elemental function pfloor(self,p) result(res)
      class(Floors), intent(in) :: self
      real(8),       intent(in) :: p
      real(8)                   :: res
      res = max(p,self%smallp)
   end function pfloor
   
end module traits_runtime_elemental_01_oracle_m

program main

   use traits_runtime_elemental_01_oracle_m

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
   
end program main
