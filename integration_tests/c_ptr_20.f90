program c_ptr_20
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none
    type(c_ptr) :: p
    p = c_null_ptr
    call s(p)
contains
    subroutine s(buf)
        type(*) :: buf
    end subroutine
end program c_ptr_20
