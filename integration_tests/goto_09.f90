! An assigned GO TO without a label list may branch to any label of the
! procedure, including labels of statements inside constructs, which a
! branch cannot enter. Those branches are never taken, but they are
! compiled: here into a BLOCK construct, and into the IF and DO constructs
! whose header references a function with a finalizable result.
!
! gfortran does not finalize these results; for it, no finalization is
! accepted.
module goto_09_m
    implicit none
    integer :: finished = 0
    type :: t
        integer :: v = 0
    contains
        final :: finish
    end type
contains
    function make() result(r)
        type(t) :: r
        r%v = 42
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
end module

program goto_09
    use iso_fortran_env, only: compiler_version
    use goto_09_m
    implicit none
    integer :: lab, c, k, i
    logical :: strict
    strict = index(compiler_version(), "GCC") == 0

    c = 0
    assign 10 to lab
    go to lab
    c = c + 100
10  continue
    if (positive(make())) then
        c = c + 1
20      continue
    end if
    if (c /= 1) error stop "if: count"
    if (finished /= 1 .and. (strict .or. finished /= 0)) error stop "if: finalizations"

    finished = 0
    c = 0
    assign 30 to lab
    go to lab
    c = c + 100
30  continue
    do 40 k = 1, value(make()) / 21
        c = c + 1
40  continue
    if (c /= 2) error stop "do: count"
    if (finished /= 1 .and. (strict .or. finished /= 0)) error stop "do: finalizations"

    c = 0
    do i = 1, 3
        assign 50 to lab
        go to lab
        c = c + 100
50      continue
        block
            integer :: b
            b = i
            if (b > 1) go to 60
            c = c + b
60          continue
        end block
    end do
    if (c /= 1) error stop "block: count"
    print *, "ok"
end program
