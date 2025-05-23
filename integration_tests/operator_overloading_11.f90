module operator_overloading_11_module_1
   implicit none

   type :: t
   end type

   interface operator(/=)
      module procedure ne
   end interface
contains
   logical function ne(x, y)
      type(t), intent(in) :: x, y
      print *, "t::ne"
      ne = .false.
   end function
end module operator_overloading_11_module_1

module operator_overloading_11_module_2
   use operator_overloading_11_module_1, only: t, operator(/=)
   type :: u
      type(t) :: x
   end type
   interface operator(/=)
      module procedure une
   end interface
contains
   logical function une(a, b)
      type(u), intent(in) :: a, b
      print *, "u::une"
      une = .false.
   end function
end module operator_overloading_11_module_2

program main
   use operator_overloading_11_module_2

   implicit none

   type(t) :: x
   type(u) :: y

   if (x /= x) error stop
   if (y /= y) error stop
end program main
