! An EXTERNAL declared in a PROGRAM must not stay visible to a later
! independent program unit in the same file. `zz` is external only in the
! program; in `s2` it is a local array, so `zz(1)` there is array indexing
! and must not be turned into a call to an external procedure.
program external_23
    implicit none
    integer, external :: zz
    integer :: r
    r = zz(1)
    if (r /= 101) error stop "external in PROGRAM was not called"
    call s2(r)
    if (r /= 1) error stop "array indexing was treated as a call"
    print *, "ok"
end program external_23

subroutine s2(r)
    implicit none
    integer, intent(out) :: r
    integer :: zz(3)
    zz = [1, 2, 3]
    r = zz(1)
end subroutine s2

integer function zz(k)
    implicit none
    integer :: k
    zz = 100 + k
end function zz
