module traits_invalid_initial_01_mod
   implicit none
   private
   public :: Point

   type :: Point
      integer :: x
   contains
      initial :: make_point
   end type Point

end module traits_invalid_initial_01_mod

program traits_invalid_initial_01
   use traits_invalid_initial_01_mod, only: Point
   implicit none
   type(Point) :: p

   p = Point(1)

contains

   type(Point) function make_point(x) result(p)
      integer, intent(in) :: x
      p%x = x
   end function make_point

end program traits_invalid_initial_01
