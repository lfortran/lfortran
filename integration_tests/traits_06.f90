module traits_06_mod
   implicit none
   private
   public :: IShape, Circle, ColoredCircle, compute_area

   abstract interface :: IShape
      real function iface_area(self)
         import :: IShape
         class(IShape), intent(in) :: self
      end function iface_area
   end interface IShape

   type, implements(IShape) :: Circle
      real :: radius
   contains
      procedure :: iface_area => circle_area
   end type Circle

   type, extends(Circle) :: ColoredCircle
      integer :: color
   end type ColoredCircle

contains

   real function circle_area(self) result(r)
      class(Circle), intent(in) :: self
      r = 3.14159265 * self%radius * self%radius
   end function circle_area

   real function compute_area(s)
      class(IShape), intent(in) :: s
      compute_area = s%iface_area()
   end function compute_area

end module traits_06_mod

program traits_06
   use traits_06_mod, only: IShape, ColoredCircle, compute_area
   implicit none
   type(ColoredCircle) :: c
   class(IShape), allocatable :: shape

   c%radius = 2.0
   c%color = 1

   if (abs(compute_area(c) - 12.5663706) > 1e-5) error stop 1
   shape = c
   if (abs(shape%iface_area() - 12.5663706) > 1e-5) error stop 2
   print *, "PASS"
end program traits_06
