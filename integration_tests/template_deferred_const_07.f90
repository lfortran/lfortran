! A REQUIRE statement passes instantiation arguments too, so a constant
! expression may be given for a deferred constant of the requirement
! (#13411).

module template_deferred_const_07_m
    implicit none
    private
    public :: tmpl

    integer, parameter :: two = 2

    requirement r{T, n}
        deferred type :: T
        deferred integer, parameter :: n
    end requirement

    template tmpl {T, m}
        deferred type :: T
        deferred integer, parameter :: m
        require :: r{T, 3}
        require :: r{T, two * 5 + 1}
        require :: r{n=-two, T=T}
        private
        public :: get_m
    contains
        function get_m(x) result(r)
            type(T), intent(in) :: x
            integer :: r
            r = m
        end function
    end template

end module

program template_deferred_const_07
    use template_deferred_const_07_m, only: tmpl
    implicit none
    instantiate tmpl {integer, 4}, only: get_m4 => get_m
    instantiate tmpl {real, 2 * 3}, only: get_m6 => get_m

    if (get_m4(1) /= 4) error stop
    if (get_m6(1.0) /= 6) error stop
    print *, get_m4(1), get_m6(1.0)
end program
