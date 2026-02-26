module c_sizeof_02_mod
    use iso_c_binding, only: c_sizeof, c_int64_t, c_int32_t
    implicit none
    type, bind(c) :: pair_t
        integer(c_int64_t) :: a, b
    end type
contains
    subroutine check()
        type(pair_t) :: x
        integer(c_int32_t) :: i
        if (c_sizeof(x) /= 16) error stop
        if (c_sizeof(i) /= 4) error stop
        print *, "ok"
    end subroutine
end module

program c_sizeof_02
    use c_sizeof_02_mod
    call check()
end program
