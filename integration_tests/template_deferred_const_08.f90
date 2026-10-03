! Instantiation arguments for deferred constants that are constant
! expressions (a literal, a keyword argument, an expression of named
! constants) rather than just named constants (#13411).

module template_deferred_const_08_m
    implicit none
    private
    public :: tmpl, get_seven

    template tmpl {n, neg}
        deferred integer, parameter :: n
        deferred logical, parameter :: neg
        private
        public :: get_n, sum_n, signed
    contains
        function get_n() result(r)
            integer :: r
            r = n
        end function

        function sum_n() result(r)
            integer :: r, i
            r = 0
            do i = 1, n
                r = r + i
            end do
        end function

        function signed() result(r)
            integer :: r
            r = n
            if (neg) r = -n
        end function
    end template

    instantiate tmpl {7, .false.}, only: get_seven => get_n

end module

program template_deferred_const_08
    use template_deferred_const_08_m, only: tmpl, get_seven
    implicit none
    integer, parameter :: two = 2

    instantiate tmpl {2, .true.}, only: get_2 => get_n, signed_2 => signed
    instantiate tmpl {neg = .false., n = 3}, only: sum_3 => sum_n
    instantiate tmpl {2 * two + 1, two > 1}, only: get_5 => get_n, &
        signed_5 => signed

    if (get_2() /= 2) error stop
    if (signed_2() /= -2) error stop
    if (sum_3() /= 6) error stop
    if (get_5() /= 5) error stop
    if (signed_5() /= -5) error stop
    if (get_seven() /= 7) error stop
    print *, get_2(), signed_2(), sum_3(), get_5(), signed_5(), get_seven()
end program
