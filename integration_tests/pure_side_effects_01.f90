module pure_side_effects_01_mod
    implicit none
contains
    pure function square(x) result(res)
        integer, intent(in) :: x
        integer :: res
        res = x * x
    end function square

    function print_and_square(x) result(res)
        integer, intent(in) :: x
        integer :: res
        print *, x
        res = x * x
    end function print_and_square
end module pure_side_effects_01_mod

program pure_side_effects_01
    use pure_side_effects_01_mod
    implicit none
    integer :: r

    r = square(3)
    if (r /= 9) error stop

    r = print_and_square(4)
    if (r /= 16) error stop
end program pure_side_effects_01
