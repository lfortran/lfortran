! A finalizable function result referenced by an input item of a READ with
! END=, ERR= or EOR= (and no IOSTAT=) is finalized once, after the statement,
! whichever way the statement completes.
!
! gfortran does not finalize these results. For it, the checks marked
! "strict" also accept that nothing was finalized.
module finalization_09_m
    use iso_fortran_env, only: compiler_version
    implicit none
    logical :: strict = .true.
    integer :: nfin = 0, ncalls = 0
    integer :: fin_log(10) = -100
    type :: h
        integer, pointer :: p => null()
    contains
        final :: fin_h
    end type
contains
    function mk(c) result(r)
        integer, intent(in) :: c
        type(h) :: r
        ncalls = ncalls + 1
        allocate(r%p)
        r%p = c
    end function

    integer function pv(s)
        type(h), intent(in) :: s
        if (.not. associated(s%p)) error stop "pv: result already finalized"
        pv = s%p
    end function

    subroutine fin_h(self)
        type(h), intent(inout) :: self
        nfin = nfin + 1
        if (associated(self%p)) then
            fin_log(nfin) = self%p
            deallocate(self%p)
        end if
    end subroutine

    subroutine check(what, expected)
        character(*), intent(in) :: what
        integer, intent(in) :: expected
        if (ncalls /= 1) then
            print *, what, ": ", ncalls, "calls"
            error stop "the function was not called once"
        end if
        if (nfin == 0 .and. .not. strict) then
            ncalls = 0
            return
        end if
        if (nfin /= 1) then
            print *, what, ": finalized", nfin, "results, expected 1"
            error stop "wrong number of finalized results"
        end if
        if (fin_log(1) /= expected) error stop "finalized entity is not the result"
        nfin = 0; ncalls = 0; fin_log = -100
    end subroutine
end module

program finalization_09
    use finalization_09_m
    implicit none
    integer :: u, n, i, a(5)
    strict = index(compiler_version(), "GCC") == 0

    ! END=, taken
    a = 0; n = 0
    open(newunit=u, status="scratch")
    read(u, *, end=10) a(pv(mk(2)))
    n = 99
10  continue
    close(u)
    if (n /= 0 .or. any(a /= 0)) error stop "end=: wrong values"
    call check("end= taken", 2)

    ! END=, not taken
    a = 0
    open(newunit=u, status="scratch")
    write(u, *) 7
    rewind(u)
    read(u, *, end=20) a(pv(mk(3)))
    n = 1
20  continue
    close(u)
    if (n /= 1 .or. a(3) /= 7 .or. sum(a) /= 7) error stop "end=: wrong value read"
    call check("end= not taken", 3)

    ! ERR=, taken
    a = 0; n = 0
    open(newunit=u, status="scratch")
    write(u, *) "abc"
    rewind(u)
    read(u, *, err=30) a(pv(mk(4)))
    n = 99
30  continue
    close(u)
    if (n /= 0 .or. any(a /= 0)) error stop "err=: wrong values"
    call check("err= taken", 4)

    ! formatted, END= and ERR=
    a = 0; n = 0
    open(newunit=u, status="scratch")
    write(u, "(i3)") 5
    rewind(u)
    read(u, "(i3)", end=40, err=40) a(pv(mk(1)))
    n = a(1)
40  continue
    close(u)
    if (n /= 5 .or. a(1) /= 5) error stop "formatted: wrong value read"
    call check("formatted", 1)

    ! non-advancing, EOR=
    a = 0; n = 0
    open(newunit=u, status="scratch")
    write(u, "(i3)") 6
    rewind(u)
    read(u, "(i3)", advance="no", end=50, eor=50) a(pv(mk(5)))
    n = a(5)
50  continue
    close(u)
    if (n /= 6 .or. a(5) /= 6) error stop "eor=: wrong value read"
    call check("eor=", 5)

    ! implied DO whose bound references the result
    a = 0; n = 0
    open(newunit=u, status="scratch")
    write(u, *) 4, 11, 12
    rewind(u)
    read(u, *, end=60) n, (a(i), i = 1, pv(mk(2)))
60  continue
    close(u)
    if (n /= 4 .or. a(1) /= 11 .or. a(2) /= 12 .or. a(3) /= 0) &
        error stop "implied do: wrong values read"
    call check("implied do", 2)

    print *, "ok"
end program
