! Deferred constants of templated subprograms of other types and uses: a
! logical constant, an assumed-length character constant, and an integer
! constant used as the bound of a local explicit-shape array and of a loop.
! Each is used through instantiation and through an inline instantiation
! (lfortran/lfortran#13360).
module template_deferred_const_07_m
    implicit none
contains
    template subroutine add_sum{n}(x)
        deferred integer, parameter :: n
        integer, intent(inout) :: x
        integer :: buf(n)
        integer :: i
        do i = 1, n
            buf(i) = i
        end do
        x = x + sum(buf)
    end subroutine

    template integer function pick{b}(x, y) result(z)
        deferred logical, parameter :: b
        integer, intent(in) :: x, y
        if (b) then
            z = x
        else
            z = y
        end if
    end function

    template subroutine text_len{s}(k)
        deferred character(*), parameter :: s
        integer, intent(out) :: k
        k = len(s)
    end subroutine
end module

program template_deferred_const_07
    use template_deferred_const_07_m
    implicit none
    integer, parameter :: three = 3, five = 5
    logical, parameter :: yes = .true., no = .false.
    character(*), parameter :: hello = "hello", hi = "hi"
    integer :: x

    instantiate add_sum {five}, only: add_sum5 => add_sum
    instantiate pick {yes}, only: pick_first => pick
    instantiate pick {no}, only: pick_second => pick
    instantiate text_len {hello}, only: hello_len => text_len

    x = 1
    call add_sum5(x)
    if (x /= 16) error stop
    x = 1
    call add_sum{three}(x)
    if (x /= 7) error stop

    if (pick_first(7, 9) /= 7) error stop
    if (pick_second(7, 9) /= 9) error stop
    if (pick{no}(7, 9) /= 9) error stop

    call hello_len(x)
    if (x /= 5) error stop
    call text_len{hi}(x)
    if (x /= 2) error stop

    print *, "ok"
end program
