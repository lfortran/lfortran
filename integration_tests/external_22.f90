! An EXTERNAL declared in a module specification part must not stay visible
! to a later independent program unit. `zz` is external only in `m`; in `s2`
! it is a local array, so `zz(1)` there is array indexing and must not be
! turned into a call to an external procedure.
module external_22_mod
    implicit none
    integer, external :: zz
end module external_22_mod

subroutine s2(r)
    implicit none
    integer, intent(out) :: r
    integer :: zz(3)
    zz = [1, 2, 3]
    r = zz(1)
end subroutine s2

program external_22
    implicit none
    integer :: r
    call s2(r)
    if (r /= 1) error stop "array indexing was treated as a call"
    print *, "ok"
end program external_22

integer function zz(k)
    implicit none
    integer :: k
    zz = 100 + k
end function zz
