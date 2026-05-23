module traits_02_mod
   implicit none
   private
   public :: IShape, Circle, Square, compute_area

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

   type, implements(IShape) :: Square
      real :: side
   contains
      procedure :: iface_area => square_area
   end type Square

contains

   real function circle_area(self) result(r)
      class(Circle), intent(in) :: self
      r = 3.14159265 * self%radius * self%radius
   end function circle_area

   real function square_area(self) result(r)
      class(Square), intent(in) :: self
      r = self%side * self%side
   end function square_area

   real function compute_area(s)
      class(IShape), intent(in) :: s
      compute_area = s%iface_area()
   end function compute_area

end module traits_02_mod

program traits_02
   use traits_02_mod, only: IShape, Circle, Square, compute_area
   implicit none
   type(Circle) :: c
   type(Square) :: s
   class(IShape), allocatable :: shape
   real :: result

   c%radius = 1.0
   s%side = 2.0

   result = compute_area(c)
   if (abs(result - 3.14159265) > 1e-5) error stop 1

   result = compute_area(s)
   if (abs(result - 4.0) > 1e-5) error stop 2

   shape = c
   if (abs(shape%iface_area() - 3.14159265) > 1e-5) error stop 3

   shape = s
   if (abs(shape%iface_area() - 4.0) > 1e-5) error stop 4

   print *, "PASS"
end program traits_02
