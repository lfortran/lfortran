module legacy_array_sections_07_module                                                           !! [order = polynomial degree + 1]
   interface db1ink
      module procedure :: db1ink_default
   end interface
   public :: db1ink
     contains
   pure subroutine db1ink_default(x)
      implicit none
      real(4), dimension(:), intent(in) :: x
      call check_inputs(x=x)
   end subroutine db1ink_default
   pure subroutine check_inputs(x, y)
      implicit none
      real(4), dimension(:), intent(in), optional :: x, y
   end subroutine check_inputs
end module legacy_array_sections_07_module
program legacy_array_sections_07
    use legacy_array_sections_07_module
    implicit none
    real :: x(5), y(2)
    x = [1.0, 2.0, 3.0, 4.0, 5.0]
    y = [6.0, 7.0]
    call db1ink_default(x)
    print *, x
    if (any(x - [1.0, 2.0, 3.0, 4.0, 5.0] > 1e-6)) error stop
    call check_inputs(x, y)
    print *, y
    if (any(y - [6.0, 7.0] > 1e-6)) error stop
end program legacy_array_sections_07