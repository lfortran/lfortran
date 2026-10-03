! A procedure declared `external` in the specification part of a module and
! referenced more than once from the same module procedure. Every reference
! re-derives the implicit interface, and the derived symbol must stay the one
! the earlier references already point at.
module implicit_interface_65_mod
    implicit none
    private
    integer, external :: scale_by_two
    public :: twice_scaled, thrice_scaled
contains

    integer function twice_scaled(n)
        integer, intent(in) :: n
        twice_scaled = scale_by_two(n)
        twice_scaled = twice_scaled + scale_by_two(n + 1)
    end function twice_scaled

    integer function thrice_scaled(n)
        integer, intent(in) :: n
        integer :: acc
        acc = scale_by_two(n)
        acc = acc + scale_by_two(n + 1)
        acc = acc + scale_by_two(n + 2)
        thrice_scaled = acc
    end function thrice_scaled

end module implicit_interface_65_mod

integer function scale_by_two(k)
    implicit none
    integer :: k
    scale_by_two = 2*k
end function scale_by_two

program implicit_interface_65
    use implicit_interface_65_mod, only: twice_scaled, thrice_scaled
    implicit none
    integer :: r

    r = twice_scaled(3)
    print *, r
    if (r /= 14) error stop

    r = thrice_scaled(3)
    print *, r
    if (r /= 24) error stop

    print *, "OK"
end program implicit_interface_65
