! A statically initialized variable of a derived type with a type(c_ptr) or
! type(c_funptr) array component, initialized by a structure constructor.
! Issue: https://github.com/lfortran/lfortran/issues/13087
module derived_type_c_ptr_array_ctor_01_mod
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, &
        c_funptr, c_null_funptr
    implicit none
    type :: t
        type(c_ptr) :: p(2) = c_null_ptr
        integer :: h = 0
    end type
    type :: u
        type(c_funptr) :: q(3) = c_null_funptr
        real :: r = 1.0
    end type
    type(t) :: module_var = t(h=7)
end module

program derived_type_c_ptr_array_ctor_01
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, &
        c_funptr, c_null_funptr, c_associated
    use derived_type_c_ptr_array_ctor_01_mod
    implicit none

    if (module_var%h /= 7) error stop
    if (c_associated(module_var%p(1))) error stop
    if (c_associated(module_var%p(2))) error stop

    call omitted_component()
    call explicit_component()
    call funptr_component()

contains

    ! The array component is omitted, so it takes its default initializer.
    subroutine omitted_component()
        type(t), save :: x = t(h=9)
        if (x%h /= 9) error stop
        if (c_associated(x%p(1))) error stop
        if (c_associated(x%p(2))) error stop
    end subroutine

    ! The array component is given the scalar c_null_ptr explicitly.
    subroutine explicit_component()
        type(t), save :: y = t(p=c_null_ptr, h=11)
        if (y%h /= 11) error stop
        if (c_associated(y%p(1))) error stop
        if (c_associated(y%p(2))) error stop
    end subroutine

    subroutine funptr_component()
        type(u), save :: z = u(r=2.5)
        if (z%r /= 2.5) error stop
        if (c_associated(z%q(1))) error stop
        if (c_associated(z%q(2))) error stop
        if (c_associated(z%q(3))) error stop
    end subroutine

end program derived_type_c_ptr_array_ctor_01
