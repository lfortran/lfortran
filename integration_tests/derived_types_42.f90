module derived_types_42_mod
    implicit none
    type twoint
        integer :: m1
        integer :: m2
    end type

    interface twoint
        procedure :: reverse_constructor
    end interface

    contains
        function reverse_constructor(i, j) result(n)
            integer, intent(in) :: i, j
            type(twoint) :: n
            n%m1 = j
            n%m2 = i
        end function
 end module derived_types_42_mod

program derived_types_42
    use derived_types_42_mod

    implicit none
    type(twoint) :: ins
    ins = twoint(1, 2)

    if (ins%m1 /= 2 .or. ins%m2 /= 1) error stop
end program derived_types_42
