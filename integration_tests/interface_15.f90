module interface_15_mod
    implicit none
    type t
        integer :: x
    end type t
end module interface_15_mod

program interface_15
    use interface_15_mod
    implicit none

    integer :: x(2)
    x = [5, 4]
    print *, func(x)

    if (func(x) /= 9) error stop

    contains
        function func(i) result(output)
        integer, intent(in) :: i(:)
        real :: output
        type(t) :: type

        interface
            function R(j, s)
            use interface_15_mod
            implicit none
            integer, intent(in) :: j(:)
            type(t), intent(inout) :: s
            real :: R
            end function
        end interface

        output = i(1) + i(2)
        end function func
end program
