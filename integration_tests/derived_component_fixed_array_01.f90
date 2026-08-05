program derived_component_fixed_array_01
    implicit none

    type :: t
        integer :: i
    end type

    type(t) :: xs(2)

    xs%i = [11, 22]

    if (xs(1)%i /= 11 .or. xs(2)%i /= 22) then
        error stop "fixed-size derived array component access failed"
    end if
end program
