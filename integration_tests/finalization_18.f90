! A nonpointer, nonallocatable INTENT(OUT) array dummy argument is finalized
! when the procedure is invoked (F2018 7.5.6.3 p7): the final subroutine of
! its rank with the whole array, or else an elemental one for every element,
! then those of the parent type with the parent component (7.5.6.2). The
! actual argument can be a section whose elements are not adjacent, and the
! elements outside of it are left alone.
module finalization_18_mod
    implicit none
    integer :: log(100), nlog = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t1
        final :: fin_t2
    end type
    type, extends(t) :: e
        integer :: w = 0
    contains
        final :: fin_e1
    end type
    type :: u
        integer :: v = 0
    contains
        final :: fin_u
    end type
contains
    subroutine record(values)
        integer, intent(in) :: values(:)
        log(nlog + 1:nlog + size(values)) = values
        nlog = nlog + size(values)
    end subroutine

    subroutine check(expected, code)
        integer, intent(in) :: expected(:), code
        if (nlog /= size(expected)) error stop code
        if (any(log(:nlog) /= expected)) error stop code
        nlog = 0
    end subroutine

    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        call record(x%v)
    end subroutine

    subroutine fin_t2(x)
        type(t), intent(inout) :: x(:, :)
        call record([-size(x, 1), -size(x, 2), reshape(x%v, [size(x)])])
    end subroutine

    subroutine fin_e1(x)
        type(e), intent(inout) :: x(:)
        call record(x%w)
        x%v = 2 * x%v
    end subroutine

    impure elemental subroutine fin_u(x)
        type(u), intent(inout) :: x
        call record([x%v])
    end subroutine

    subroutine out_t1(x)
        type(t), intent(out) :: x(:)
    end subroutine

    subroutine out_t2(x)
        type(t), intent(out) :: x(:, :)
    end subroutine

    subroutine out_e1(x)
        type(e), intent(out) :: x(:)
    end subroutine

    subroutine out_explicit(x, n)
        integer, intent(in) :: n
        type(e), intent(out) :: x(n)
    end subroutine

    subroutine out_u(x)
        type(u), intent(out) :: x(:)
    end subroutine

    subroutine run()
        integer :: i
        type(t) :: y(6), z(3, 4)
        type(e) :: ye(5)
        type(u) :: yu(4)

        y%v = [(i, i = 1, 6)]
        call out_t1(y)
        call check([1, 2, 3, 4, 5, 6], 1)

        y%v = [(i, i = 1, 6)]
        call out_t1(y(1:6:2))
        call check([1, 3, 5], 2)
        if (any(y(2:6:2)%v /= [2, 4, 6])) error stop 3

        z%v = reshape([(i, i = 1, 12)], [3, 4])
        call out_t2(z(1:3:2, 2:4:2))
        call check([-2, -2, 4, 6, 10, 12], 4)
        if (any(z(2, :)%v /= [2, 5, 8, 11])) error stop 5

        ! The final subroutine of `e`, then that of `t` with the parent
        ! component, which sees the values that the first one left.
        ye%v = [(i, i = 1, 5)]
        ye%w = [(10 * i, i = 1, 5)]
        call out_e1(ye(5:1:-2))
        call check([50, 30, 10, 10, 6, 2], 6)
        if (any(ye(2:4:2)%v /= [2, 4])) error stop 7
        if (any(ye(2:4:2)%w /= [20, 40])) error stop 8

        ye%v = [(i, i = 1, 5)]
        ye%w = [(10 * i, i = 1, 5)]
        call out_explicit(ye(2:4), 3)
        call check([20, 30, 40, 4, 6, 8], 9)
        if (ye(1)%v /= 1 .or. ye(5)%v /= 5) error stop 10

        yu%v = [1, 2, 3, 4]
        call out_u(yu(2:4:2))
        call check([2, 4], 11)
        if (yu(1)%v /= 1 .or. yu(3)%v /= 3) error stop 12
    end subroutine
end module

program finalization_18
    use finalization_18_mod
    implicit none
    call run()
    print *, "ok"
end program
