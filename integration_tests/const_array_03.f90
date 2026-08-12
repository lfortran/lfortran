module const_array_03_mod
    use iso_c_binding
    implicit none
    type :: my_point
        integer(c_int) :: x, y
    end type my_point
contains
    subroutine test_const_array()
        type(my_point), parameter :: pts(2) = [my_point(1, 2), my_point(3, 4)]
        type(c_ptr), parameter :: ptrs(2) = [c_null_ptr, c_null_ptr]
        if (pts(1)%x /= 1 .or. pts(1)%y /= 2) error stop "const_array_03 struct 1 failed"
        if (pts(2)%x /= 3 .or. pts(2)%y /= 4) error stop "const_array_03 struct 2 failed"
        if (c_associated(ptrs(1)) .or. c_associated(ptrs(2))) error stop "const_array_03 c_ptr failed"
    end subroutine test_const_array
end module const_array_03_mod

program const_array_03
    use const_array_03_mod
    implicit none
    call test_const_array()
    print *, "const_array_03 test passed"
end program const_array_03
