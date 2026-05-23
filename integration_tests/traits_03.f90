module traits_03_mod
   implicit none
   private
   public :: Point, make_point

   type :: Point
      real :: x, y
   contains
      initial :: make_point
   end type Point

contains

   type(Point) function make_point(x, y) result(p)
      real, intent(in) :: x, y
      p%x = x
      p%y = y
   end function make_point

end module traits_03_mod

program traits_03
   use traits_03_mod, only: Point, make_point
   implicit none
   type(Point) :: p
   p = Point(3.0, 4.0)
   if (abs(p%x - 3.0) > 1e-5) error stop 1
   if (abs(p%y - 4.0) > 1e-5) error stop 2
   print *, "PASS"
end program traits_03
