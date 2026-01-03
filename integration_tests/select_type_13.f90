module select_type_13_module
   implicit none

   type, abstract :: shape
   contains
      procedure(area), deferred :: get_area
   end type shape

   abstract interface
      real function area(this)
         import :: shape
         class(shape), intent(in) :: this
      end function
   end interface

   type, extends(shape) :: circle
      real :: r
   contains
      procedure :: get_area => circle_area
   end type circle

   type, extends(shape) :: rectangle
      real :: a, b
   contains
      procedure :: get_area => rectangle_area
   end type rectangle

contains

   real function circle_area(this)
      class(circle), intent(in) :: this
      circle_area = 3.14159 * this%r * this%r
   end function circle_area

   real function rectangle_area(this)
      class(rectangle), intent(in) :: this
      rectangle_area = this%a * this%b
   end function rectangle_area

end module select_type_13_module


program select_type_13
   use select_type_13_module
   implicit none

   class(shape), allocatable :: s1, s2

   allocate(circle :: s1)

   select type (s1)
    class is (rectangle)
      print *, "Matched as rectangle"
    class is (circle)
      print *, "Matched as circle"
      s1%r = 10.0
      print *, s1%r
      if (s1%r /= 10.0) error stop
      ! TODO: Support calling methods on derived types in select type
      ! print *, s1%get_area()
    class default
      print *, "Matched as shape"
   end select

   allocate(rectangle :: s2)

   select type (s2)
    class is (circle)
      print *, "Matched as circle"
    class is (rectangle)
      print *, "Matched as rectangle"
      s2%a = 5.0
      s2%b = 4.0
      print *, s2%a, s2%b
      if (s2%a /= 5.0) error stop
      if (s2%b /= 4.0) error stop
      ! TODO: Support calling methods on derived types in select type
      ! print *, s2%get_area()
    class default
      print *, "Matched as shape"
   end select

end program select_type_13
