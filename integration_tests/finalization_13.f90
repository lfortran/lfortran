! The result of a nonpointer function referenced in the header of an
! executable construct, or in the selector of an ASSOCIATE construct, is
! finalized after the construct (F2018 7.5.6.3 p5): not before its body is
! executed, and also when the construct is left by EXIT, CYCLE or RETURN.
!
! gfortran does not finalize some of these results, and finalizes those of
! an ASSOCIATE selector before the body. For it, the checks marked "strict"
! are skipped.
module finalization_13_m
    use iso_fortran_env, only: compiler_version
    implicit none
    logical :: strict = .true.
    integer :: finished = 0
    type :: t
        integer :: v = 0
    contains
        final :: finish
    end type
contains
    function make(n) result(r)
        integer, intent(in) :: n
        type(t) :: r
        r%v = n
    end function
    function wrap(x) result(r)
        type(t), intent(in) :: x
        type(t) :: r
        r%v = x%v
    end function
    logical function positive(x)
        type(t), intent(in) :: x
        positive = x%v > 0
    end function
    integer function value(x)
        type(t), intent(in) :: x
        value = x%v
    end function
    subroutine finish(x)
        type(t), intent(inout) :: x
        if (x%v == 42) finished = finished + 1
    end subroutine

    subroutine check(condition, message)
        logical, intent(in) :: condition
        character(*), intent(in) :: message
        if (.not. condition) then
            print *, message, finished
            error stop
        end if
    end subroutine

    subroutine if_return()
        if (positive(make(42))) then
            call check(finished == 0, "if_return: inside")
            return
        end if
        error stop "if_return"
    end subroutine
end module

program finalization_13
    use finalization_13_m
    implicit none
    integer :: i, n, a(2)

    strict = index(compiler_version(), "GCC") == 0

    finished = 0
    if (positive(make(42))) then
        call check(finished == 0, "if: inside")
    end if
    if (strict) call check(finished == 1, "if: after")

    finished = 0
    if (.not. positive(make(42))) then
        error stop "else"
    else
        call check(finished == 0, "else: inside")
    end if
    if (strict) call check(finished == 1, "else: after")

    finished = 0
    n = 0
    do i = 1, value(make(42)) / 42 + 1
        call check(finished == 0, "do: inside")
        n = n + 1
    end do
    call check(n == 2, "do: iterations")
    if (strict) call check(finished == 1, "do: after")

    finished = 0
    do i = 1, value(make(42))
        call check(finished == 0, "do exit: inside")
        if (i == 2) exit
    end do
    if (strict) call check(finished == 1, "do exit: after")

    finished = 0
    do i = 1, value(make(42))
        call check(finished == 0, "do cycle: inside")
        if (i >= 2) cycle
    end do
    if (strict) call check(finished == 1, "do cycle: after")

    finished = 0
    select case (value(make(42)))
    case (42)
        call check(finished == 0, "select case: inside")
    case default
        error stop "select case"
    end select
    if (strict) call check(finished == 1, "select case: after")

    finished = 0
    a = -1
    do concurrent (i = 1:value(make(42)) / 42 + 1)
        a(i) = finished
    end do
    call check(all(a(1:2) == 0), "do concurrent: inside")
    if (strict) call check(finished == 1, "do concurrent: after")

    finished = 0
    n = 0
    do while (value(make(42)) > n)
        ! The result of each evaluation of the condition is finalized
        ! after the iteration.
        if (strict) call check(finished == n / 21, "do while: inside")
        n = n + 21
    end do
    call check(n == 42, "do while: iterations")
    if (strict) call check(finished == 3, "do while: after")

    finished = 0
    call if_return()
    if (strict) call check(finished == 1, "if return: after")

    finished = 0
    associate (x => value(make(42)))
        if (strict) call check(finished == 0, "associate: inside")
        call check(x == 42, "associate: value")
    end associate
    call check(finished == 1, "associate: after")

    finished = 0
    associate (x => wrap(make(42)))
        if (strict) call check(finished == 0, "nested associate: inside")
        call check(x%v == 42, "nested associate: value")
    end associate
    call check(finished == 2, "nested associate: after")

    ! An assignment to an associate name is a statement of the construct,
    ! not the association with the selector.
    n = 1
    finished = 0
    associate (x => n)
        x = value(make(42))
        call check(finished == 1, "associate assignment: after statement")
    end associate
    call check(n == 42, "associate assignment: value")
    call check(finished == 1, "associate assignment: after")
    print *, "ok"
end program
