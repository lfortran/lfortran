      module traits_initializers_03_m
      implicit none
      type :: Box
      integer :: n
      contains
      initial :: make
      end type
      contains
      function make(value) result(object)
      integer, intent(in) :: value
      type(Box) :: object
      object%n = value + 10
      end function
      end module
      program traits_initializers_03
      use traits_initializers_03_m, only: Box
      implicit none
      type(Box) :: x
      integer :: initial
      initial = 7
      x = Box(value=initial)
      if (x%n /= 17) error stop
      end program
