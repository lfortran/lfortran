! A module with default PRIVATE accessibility explicitly publishes a
! module entity. Users can import it with ONLY, rename it, or reach it
! through a module entity of the module.
module namespace_modules_26_a
    implicit none
    integer :: x = 1
contains
    integer function get_x()
        get_x = x
    end function
end module

module namespace_modules_26_b
    use, namespace :: a => namespace_modules_26_a
    implicit none
    private
    public :: a, y
    integer :: y = 2
end module

program namespace_modules_26
    use namespace_modules_26_b, only: a
    use namespace_modules_26_b, only: other => a
    use, namespace :: b => namespace_modules_26_b
    implicit none
    if (a%x /= 1) error stop
    other%x = 5
    if (a%get_x() /= 5) error stop
    if (b%a%x /= 5) error stop
    if (b%y /= 2) error stop
    print *, a%x, other%x, b%a%x
end program
