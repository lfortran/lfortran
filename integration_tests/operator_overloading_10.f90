module operator_overloading_10_module
   implicit none

   type :: first_type
      integer :: x
   end type

   type :: second_type
      integer :: x
   end type

   interface operator(/=)
      module procedure ne
   end interface

   interface operator(/=)
      module procedure une
   end interface

contains
   logical function ne(x, y)
      type(first_type), intent(in) :: x, y
      print *, "first_type::ne"
      ne = .false.
   end function

   logical function une(s, z)
      type(second_type), intent(in) :: s, z
      print *, "second_type::une"
      une = .false.
   end function
end module operator_overloading_10_module

program main
   use operator_overloading_10_module
   implicit none

   type(first_type) :: a1, a2
   type(second_type) :: b1, b2

   if (a1 /= a2) error stop
   if (b1 /= b2) error stop
end program main
