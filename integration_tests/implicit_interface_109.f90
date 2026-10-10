! Calls through an implicit interface keep Fortran's argument passing:
! a character actual keeps its own length even when an earlier call with a
! shorter actual shares the call-site interface, a type(c_ptr) actual (a local,
! c_null_ptr, c_loc(x), an intent(in) or a VALUE dummy) is passed by reference,
! and a strided section of an allocatable array is copied back.
subroutine implicit_interface_109_len(msg, n)
    character(len=*) :: msg
    integer :: n
    if (len(msg) /= n) error stop 1
end subroutine

subroutine implicit_interface_109_ptr(p, q)
    use iso_c_binding, only: c_ptr
    type(c_ptr) :: p, q
    q = p
end subroutine

subroutine implicit_interface_109_chk(p, expected)
    use iso_c_binding, only: c_ptr, c_associated, c_f_pointer
    type(c_ptr) :: p
    integer :: expected
    integer, pointer :: ip
    if (expected == 0) then
        if (c_associated(p)) error stop 4
    else
        if (.not. c_associated(p)) error stop 5
        call c_f_pointer(p, ip)
        if (ip /= expected) error stop 6
    end if
end subroutine

subroutine implicit_interface_109_dbl(x, n)
    integer :: n
    real :: x(n)
    x = x * 2
end subroutine

program implicit_interface_109
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr, c_associated
    implicit none
    external implicit_interface_109_len, implicit_interface_109_ptr, &
        implicit_interface_109_chk, implicit_interface_109_dbl
    integer, target :: i
    type(c_ptr) :: a, b
    real, allocatable :: x(:)

    call implicit_interface_109_len('lw = *', 6)
    call implicit_interface_109_len('16m+70 = *', 10)

    a = c_loc(i)
    b = c_null_ptr
    call implicit_interface_109_ptr(a, b)
    if (.not. c_associated(a, b)) error stop 2

    i = 42
    call implicit_interface_109_chk(c_null_ptr, 0)
    call implicit_interface_109_chk(c_loc(i), 42)
    call fwd(c_loc(i), c_loc(i), 42)
    call fwd(c_null_ptr, c_null_ptr, 0)

    allocate(x(6))
    x = 1
    call implicit_interface_109_dbl(x(2:6:2), 3)
    print *, x
    if (any(x /= [1.0, 2.0, 1.0, 2.0, 1.0, 2.0])) error stop 3

contains

    subroutine fwd(p, q, expected)
        type(c_ptr), intent(in) :: p
        type(c_ptr), value :: q
        integer, intent(in) :: expected
        call implicit_interface_109_chk(p, expected)
        call implicit_interface_109_chk(q, expected)
    end subroutine

end program
