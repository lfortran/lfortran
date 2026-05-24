module traits_05_mod
   implicit none
   private
   public :: Point

   type :: Point
      real :: x, y
   contains
      initial :: make_point
   end type Point

contains

   type(Point) function make_point(x, y) result(p)
      real, intent(in) :: x, y
      p%x = x + 10.0
      p%y = y + 20.0
   end function make_point

end module traits_05_mod

program traits_05
   use traits_05_mod, only: Point
   implicit none
   type(Point) :: p

   p = Point(y=4.0, x=3.0)
   if (abs(p%x - 13.0) > 1e-5) error stop 1
   if (abs(p%y - 24.0) > 1e-5) error stop 2
   print *, "PASS"
end program traits_05
