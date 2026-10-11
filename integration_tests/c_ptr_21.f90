! A local type(c_ptr) passed to a type(c_ptr) dummy of intent(inout) or of
! unspecified intent is passed by reference, so the procedure can both read
! and change it.
module c_ptr_21_mod
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    integer, target :: other = 7
contains
    subroutine copy_ptr(src, dst)
        type(c_ptr) :: src
        type(c_ptr), intent(inout) :: dst
        dst = src
    end subroutine

    subroutine retarget(p)
        type(c_ptr) :: p
        p = c_loc(other)
    end subroutine
end module

program c_ptr_21
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr, c_associated, c_f_pointer
    use c_ptr_21_mod, only: copy_ptr, retarget, other
    implicit none
    integer, target :: i
    integer, pointer :: ip
    type(c_ptr) :: a, b
    i = 42
    a = c_loc(i)
    b = c_null_ptr
    call copy_ptr(a, b)
    if (.not. c_associated(a, b)) error stop 1
    call c_f_pointer(b, ip)
    if (ip /= 42) error stop 2
    call retarget(b)
    call c_f_pointer(b, ip)
    if (ip /= 7) error stop 3
    if (.not. c_associated(b, c_loc(other))) error stop 4
    print *, "ok"
end program
