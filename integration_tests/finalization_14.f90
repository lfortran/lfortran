! A GO TO statement that branches out of a BLOCK or ASSOCIATE construct, or
! out of a construct whose header references a function with a finalizable
! result, completes the construct: its variables and the results are
! finalized (F2018 7.5.6.3 p3 and p5), and its automatic variables are
! released, however many times the branch is taken.
!
! gfortran does not finalize the results in the header of a construct nor
! those of an ASSOCIATE selector. For it, the checks marked "strict" are
! skipped.
module finalization_14_m
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
    logical function positive(x)
        type(t), intent(in) :: x
        positive = x%v > 0
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

    ! Each iteration leaves a BLOCK with an automatic array of `m` elements
    ! by a GO TO. The array must be released each time.
    integer function automatic_sum(n, m) result(s)
        integer, intent(in) :: n, m
        integer :: i
        s = 0
        do i = 1, n
            block
                integer :: w(m)
                type(t) :: x
                x%v = 42
                w = i
                s = s + w(m)
                if (i > 0) goto 10
                s = -1
            end block
10          continue
        end do
    end function
end module

program finalization_14
    use finalization_14_m
    implicit none
    integer, parameter :: n = 2000
    integer :: i, c

    strict = index(compiler_version(), "GCC") == 0

    ! Forward GO TO out of an IF construct, to the end of the loop.
    finished = 0
    c = 0
    do i = 1, n
        if (positive(make(42))) then
            if (i > 0) goto 20
            c = c + 1
        end if
20      continue
    end do
    call check(c == 0, "forward: skipped")
    if (strict) call check(finished == n, "forward: after")

    ! Backward GO TO out of an IF construct.
    finished = 0
    i = 0
30  continue
    i = i + 1
    if (positive(make(42))) then
        if (strict) call check(finished == i - 1, "backward: inside")
        if (i < n) goto 30
    end if
    call check(i == n, "backward: iterations")
    if (strict) call check(finished == n, "backward: after")

    ! GO TO out of a BLOCK construct with a finalizable variable and an
    ! automatic array, 2000 times 40 kB of stack.
    finished = 0
    call check(automatic_sum(n, 10000) == n * (n + 1) / 2, "block: sum")
    call check(finished == n, "block: after")

    ! GO TO out of an inner BLOCK to a statement of the outer one.
    finished = 0
    block
        type(t) :: outer
        outer%v = 42
        do i = 1, 3
            block
                type(t) :: inner
                inner%v = 42
                if (i > 0) goto 40
            end block
40          continue
            call check(finished == i, "nested block: inside")
        end do
    end block
    call check(finished == 4, "nested block: after")

    ! GO TO inside a BLOCK construct does not complete it.
    finished = 0
    block
        type(t) :: x
        x%v = 42
        i = 0
50      continue
        i = i + 1
        if (i < 3) goto 50
        call check(finished == 0, "goto in block: inside")
    end block
    call check(finished == 1, "goto in block: after")

    ! GO TO out of an ASSOCIATE construct whose selector is a function
    ! reference with a finalizable result.
    finished = 0
    do i = 1, 3
        associate (x => make(42))
            if (x%v == 42) goto 60
            error stop "associate: value"
        end associate
60      continue
    end do
    if (strict) call check(finished == 3, "associate: after")
    print *, "ok"
end program
