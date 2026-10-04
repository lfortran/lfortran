module bindc_63_mod
    use iso_c_binding, only: c_int
    implicit none
    type, bind(c) :: pair
        integer(c_int) :: a, b
    end type pair
contains
    ! A dummy of a bind(c) procedure has to be of an interoperable type.
    subroutine pair_sum(p, r) bind(c, name="bindc_63_pair_sum")
        type(pair), intent(in) :: p
        integer(c_int), intent(out) :: r
        r = p%a + p%b
    end subroutine pair_sum
end module bindc_63_mod

program bindc_63
    use bindc_63_mod, only: pair, pair_sum
    use iso_c_binding, only: c_int
    implicit none
    type(pair) :: p
    integer(c_int) :: r
    p%a = 3
    p%b = 4
    call pair_sum(p, r)
    if (r /= 7) error stop
    print *, r
end program bindc_63
