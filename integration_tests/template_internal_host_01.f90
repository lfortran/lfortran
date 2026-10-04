! Internal procedures of a template procedure use the host's variables
! (dummies and locals) by host association; instantiation maps them to the
! instantiated host's variables instead of copying them.
module template_internal_host_01_m
    implicit none
    template tt {t}
        deferred type :: t
    contains
        integer function f(k)
            integer, intent(in) :: k
            f = inner()
        contains
            integer function inner()
                inner = k
            end function
        end function

        function outer(x, y, mask) result(r)
            type(t), intent(in) :: x, y
            logical, intent(in) :: mask
            type(t) :: r
            r = pick(x, y)
        contains
            function pick(a, b) result(q)
                type(t), intent(in) :: a, b
                type(t) :: q
                if (mask) then
                    q = a
                else
                    q = b
                end if
            end function
        end function

        subroutine s(k, x, y)
            integer, intent(out) :: k
            type(t), intent(in) :: x
            type(t), intent(out) :: y
            integer :: m
            m = 3
            call set()
        contains
            subroutine set()
                integer :: a(m)
                a = 2
                k = sum(a) + get()
                y = x
            end subroutine
            integer function get()
                get = m * 10
            end function
        end subroutine
    end template
end module

module template_internal_host_01_inst
    use template_internal_host_01_m
    implicit none
    instantiate tt {integer}, only: s_i => s
    instantiate tt {real}, only: s_r => s
end module

program template_internal_host_01
    use template_internal_host_01_m
    use template_internal_host_01_inst
    implicit none
    instantiate tt {integer}, only: f_i => f, o_i => outer
    integer :: k, yi
    real :: yr
    if (f_i(7) /= 7) error stop
    if (o_i(4, 9, .false.) /= 9) error stop
    if (o_i(4, 9, .true.) /= 4) error stop
    call s_i(k, 5, yi)
    if (k /= 36) error stop
    if (yi /= 5) error stop
    k = 0
    call s_r(k, 2.5, yr)
    if (k /= 36) error stop
    if (abs(yr - 2.5) > 1e-6) error stop
    print *, "ok"
end program
