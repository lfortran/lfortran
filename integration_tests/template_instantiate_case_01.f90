! Fortran names are case-insensitive: INSTANTIATE must find a template or a
! templated subprogram whatever the case of its name (#13584).
module template_instantiate_case_01_m
    implicit none
    TEMPLATE Tt {t}
        deferred type :: t
    contains
        function id(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = x
        end function
    end template
contains
    template function g{t}(x)
        deferred type :: t
        type(t) :: x, g
        g = x
    end function
end module

program template_instantiate_case_01
    use template_instantiate_case_01_m
    implicit none
    INSTANTIATE G {integer}, only: gi => g
    INSTANTIATE TT {integer}, only: id_i => id
    instantiate tT {real}, only: ID_R => ID
    INSTANTIATE Tt {real(8)}, only: ID
    if (gi(5) /= 5) error stop
    if (id_i(7) /= 7) error stop
    if (abs(id_r(2.5) - 2.5) > 1e-6) error stop
    if (abs(ID(3.5d0) - 3.5d0) > 1d-12) error stop
    print *, gi(5), id_i(7), id_r(2.5)
end program
