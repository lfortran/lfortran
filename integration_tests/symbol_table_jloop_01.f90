module symbol_table_jloop_01_m
    integer, parameter :: jloop = 1

    type t
        integer :: kntf(jloop:jloop)
    end type t
end module

program p
    use symbol_table_jloop_01_m
    implicit none
    type(t) :: x

    if (lbound(x%kntf, 1) /= jloop) error stop 1
    if (ubound(x%kntf, 1) /= jloop) error stop 2

    x%kntf(jloop) = 42
    if (x%kntf(jloop) /= 42) error stop 3
end program p
