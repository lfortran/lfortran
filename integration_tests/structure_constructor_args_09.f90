! `c_null_ptr` and `c_null_funptr` are named constants, not `null()`, so they
! are valid structure constructor arguments for plain `type(c_ptr)` and
! `type(c_funptr)` components, given explicitly or taken from the default.
module structure_constructor_args_09_m
    use iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr
    implicit none
    type :: w_t
        integer :: h = 0
        type(c_ptr) :: p = c_null_ptr
        type(c_funptr) :: f = c_null_funptr
    end type
    type :: n_t
        integer :: h
        type(c_ptr) :: p
        type(c_funptr) :: f
    end type
    type :: base_t
        integer :: h
        type(c_ptr) :: q = c_null_ptr
    end type
    type, extends(base_t) :: ext_t
        type(c_ptr) :: p
    end type
    type(w_t) :: m_omit = w_t(1)
    type(w_t) :: m_explicit = w_t(2, c_null_ptr, c_null_funptr)
    type(n_t) :: m_plain = n_t(3, c_null_ptr, c_null_funptr)
    type(w_t), parameter :: p_omit = w_t(4)
    type(n_t), parameter :: p_keyword = n_t(5, f=c_null_funptr, p=c_null_ptr)
end module

program structure_constructor_args_09
    use iso_c_binding, only: c_associated
    use structure_constructor_args_09_m
    implicit none
    type(w_t) :: v
    type(n_t) :: u
    type(ext_t) :: e

    if (m_omit%h /= 1 .or. c_associated(m_omit%p) .or. c_associated(m_omit%f)) error stop
    if (m_explicit%h /= 2 .or. c_associated(m_explicit%p)) error stop
    if (c_associated(m_explicit%f)) error stop
    if (m_plain%h /= 3 .or. c_associated(m_plain%p) .or. c_associated(m_plain%f)) error stop
    v = p_omit
    if (v%h /= 4 .or. c_associated(v%p) .or. c_associated(v%f)) error stop
    u = p_keyword
    if (u%h /= 5 .or. c_associated(u%p) .or. c_associated(u%f)) error stop

    v = w_t(6)
    if (v%h /= 6 .or. c_associated(v%p) .or. c_associated(v%f)) error stop
    v = w_t(7, c_null_ptr, f=c_null_funptr)
    if (v%h /= 7 .or. c_associated(v%p) .or. c_associated(v%f)) error stop
    u = n_t(8, c_null_ptr, c_null_funptr)
    if (u%h /= 8 .or. c_associated(u%p) .or. c_associated(u%f)) error stop

    e = ext_t(13, c_null_ptr, c_null_ptr)
    if (e%h /= 13 .or. c_associated(e%q) .or. c_associated(e%p)) error stop

    call check_local()
    print *, "ok"

contains

    subroutine check_local()
        type(w_t) :: l_omit = w_t(9)
        type(n_t) :: l_plain = n_t(10, c_null_ptr, c_null_funptr)
        if (l_omit%h /= 9 .or. c_associated(l_omit%p)) error stop
        if (c_associated(l_omit%f)) error stop
        if (l_plain%h /= 10 .or. c_associated(l_plain%p)) error stop
        if (c_associated(l_plain%f)) error stop
    end subroutine

end program
