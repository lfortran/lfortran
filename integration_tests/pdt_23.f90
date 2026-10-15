! The `private` statement of a parameterized derived type sets the default
! accessibility of its components only, not of the module entities declared
! after the type (F2018 7.5.4.8).
module pdt_23_m
    implicit none

    type :: box_t(k)
        integer, kind :: k = 4
        private
        integer(k) :: v
    end type

    integer :: after_box = 3

contains

    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function

end module

program pdt_23
    use pdt_23_m
    implicit none
    type(box_t(4)) :: b
    if (after_box /= 3) error stop 1
    if (twice(after_box) /= 6) error stop 2
    print *, after_box, twice(after_box)
end program
