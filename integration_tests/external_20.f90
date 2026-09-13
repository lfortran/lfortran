! An EXTERNAL declaration inside a BLOCK must not stay visible to the program
! units processed afterwards. `zz` is external only inside s1; in s2 it is a
! plain local array, so `zz(1)` there is array indexing and must not be turned
! into a call to an external procedure.
subroutine s1(r)
    implicit none
    integer, intent(out) :: r
    block
        integer, external :: zz
        r = zz(1)
    end block
end subroutine s1

subroutine s2(r)
    implicit none
    integer, intent(out) :: r
    integer :: zz(3)
    zz = [1, 2, 3]
    r = zz(1)
end subroutine s2

program external_20
    implicit none
    integer :: r
    call s1(r)
    if (r /= 101) error stop "external in BLOCK was not called"
    call s2(r)
    if (r /= 1) error stop "array indexing was treated as a call"
    print *, "ok"
end program external_20

integer function zz(k)
    implicit none
    integer :: k
    zz = 100 + k
end function zz
