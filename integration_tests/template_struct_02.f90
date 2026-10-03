module template_struct_02_m
    implicit none

    template tm {t}
        deferred type :: t
        type :: pair
            type(t) :: a
        end type
    contains
        function first(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            type(pair) :: p
            p%a = x
            r = p%a
        end function

        subroutine copy(x, y)
            type(t), intent(in) :: x
            type(t), intent(out) :: y
            type(pair) :: p
            p%a = x
            y = p%a
        end subroutine
    end template
end module

program template_struct_02
    use template_struct_02_m
    implicit none
    type :: pair
        logical :: untouched
    end type
    instantiate tm {integer}, only: ifirst => first, icopy => copy
    instantiate tm {real}, only: rfirst => first, rcopy => copy
    instantiate tm {integer}, only: explicit_pair => pair, explicit_first => first
    type(pair) :: local_pair
    integer :: i
    real :: r

    local_pair%untouched = .true.
    if (ifirst(2) /= 2) error stop
    if (ifirst(-7) /= -7) error stop
    call icopy(5, i)
    if (i /= 5) error stop
    if (abs(rfirst(1.5) - 1.5) > 1e-6) error stop
    call rcopy(-2.5, r)
    if (abs(r + 2.5) > 1e-6) error stop
    if (explicit_first(9) /= 9) error stop
    if (.not. local_pair%untouched) error stop
    call check_all()
contains
    subroutine check_all()
        instantiate tm {integer}
        integer :: n
        if (first(3) /= 3) error stop
        call copy(8, n)
        if (n /= 8) error stop
    end subroutine
end program
