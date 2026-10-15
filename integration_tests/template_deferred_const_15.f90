module template_deferred_const_15_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
        integer, parameter :: k = n*2
        integer, parameter :: m = k + 1
        type :: w
            integer :: c(m)
            integer :: d(k)
        end type
    end template
end module

! The instance's `m` is the template's `k + 1`, not this `k + 1`.
module template_deferred_const_15_u
    use template_deferred_const_15_m
    implicit none
    integer, parameter :: k = 100
    instantiate tm {3}, only: w3 => w
end module

! Only `m3` is declared, not the template's `k` that it uses.
module template_deferred_const_15_v
    use template_deferred_const_15_m
    implicit none
contains
    subroutine check_only_m()
        instantiate tm {3}, only: m3 => m
        integer :: k
        k = 42
        if (m3 /= 7 .or. k /= 42) error stop
    end subroutine
end module

program template_deferred_const_15
    use template_deferred_const_15_u
    use template_deferred_const_15_v
    implicit none
    ! The constants are listed after the type that uses them, or before it.
    instantiate tm {4}, only: w4 => w, k4 => k, m4 => m, kk4 => k
    instantiate tm {5}, only: k5 => k, m5 => m, w5 => w
    instantiate tm {6}, only: m6 => m
    type(w3) :: s3
    type(w4) :: s4
    type(w5) :: s5
    ! Not declared by the instantiations above.
    integer :: m
    m = 42
    if (size(s3%c) /= 7 .or. size(s3%d) /= 6) error stop
    if (size(s4%c) /= 9 .or. size(s4%d) /= 8) error stop
    if (k4 /= 8 .or. m4 /= 9 .or. kk4 /= 8) error stop
    if (size(s5%c) /= 11 .or. size(s5%d) /= 10) error stop
    if (k5 /= 10 .or. m5 /= 11) error stop
    if (m6 /= 13) error stop
    if (k /= 100 .or. m /= 42) error stop
    call check_only_m()
    print *, size(s3%c), size(s4%c), k4, m4, size(s5%c), k5, m5, m6, k, m
end program
