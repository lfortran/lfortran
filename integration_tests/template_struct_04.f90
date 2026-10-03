module template_struct_04_m
    implicit none

    template tm {t}
        deferred type :: t
        type :: pair
            type(t) :: a
        contains
            procedure :: store
            procedure :: set
        end type
        type :: counter
            integer :: n
        contains
            procedure :: step => count_steps
        end type
    contains
        subroutine store(this, value)
            class(pair), intent(inout) :: this
            type(t), intent(in) :: value
            this%a = value
        end subroutine

        subroutine set(this, value)
            class(pair), intent(inout) :: this
            type(t), intent(in) :: value
            call this%store(value)
        end subroutine

        recursive subroutine count_steps(this, n)
            class(counter), intent(inout) :: this
            integer, intent(in) :: n
            if (n > 0) then
                this%n = this%n + 1
                call this%step(n - 1)
            end if
        end subroutine

        function first(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r
            type(pair) :: p
            call p%store(x)
            call p%set(y)
            r = p%a
        end function

        integer function count(n) result(r)
            integer, intent(in) :: n
            type(counter) :: c
            c%n = 0
            call c%step(n)
            r = c%n
        end function
    end template
end module

program template_struct_04
    use template_struct_04_m
    implicit none
    instantiate tm {integer}, only: ifirst => first, icount => count
    instantiate tm {real}, only: rfirst => first, rcount => count
    instantiate tm {integer}, only: ipair => pair, icounter => counter, &
        explicit_first => first, explicit_count => count
    type(ipair) :: p
    type(icounter) :: c

    if (ifirst(0, 17) /= 17) error stop
    if (ifirst(6, -11) /= -11) error stop
    if (abs(rfirst(0.0, 1.5) - 1.5) > 1e-6) error stop
    if (abs(rfirst(2.0, -2.5) + 2.5) > 1e-6) error stop
    if (icount(17) /= 17) error stop
    if (icount(0) /= 0) error stop
    if (rcount(3) /= 3) error stop
    if (explicit_first(0, 31) /= 31) error stop
    if (explicit_count(4) /= 4) error stop
    call p%store(0)
    call p%set(42)
    if (p%a /= 42) error stop
    c%n = 5
    call c%step(3)
    if (c%n /= 8) error stop
    call check_local()
contains
    subroutine check_local()
        instantiate tm {integer}, only: local_first => first, local_count => count
        if (local_first(0, 53) /= 53) error stop
        if (local_count(7) /= 7) error stop
    end subroutine
end program
