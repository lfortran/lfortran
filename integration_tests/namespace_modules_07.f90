! Intrinsic modules can be imported as module entities. The NAMESPACE and
! INTRINSIC modifiers can appear in either order.
program namespace_modules_07
    use, intrinsic, namespace :: env => iso_fortran_env
    use, namespace, intrinsic :: c => iso_c_binding
    use, intrinsic, namespace :: iso_fortran_env
    implicit none
    real(env%real64) :: x
    real(env%real32) :: y
    integer(env%int8) :: i8
    integer(c%c_int) :: ci
    real(c%c_double) :: cd
    type(c%c_ptr) :: p
    integer(c%c_int), target :: t

    if (kind(x) /= 8) error stop
    if (kind(y) /= 4) error stop
    if (kind(i8) /= 1) error stop
    if (kind(ci) /= kind(1)) error stop
    if (kind(cd) /= kind(1.0d0)) error stop
    ! Both names refer to the same module
    if (env%int64 /= iso_fortran_env%int64) error stop

    x = huge(x)
    y = real(2.5, env%real32)
    i8 = 127

    p = c%c_null_ptr
    if (c%c_associated(p)) error stop
    t = 42
    p = c%c_loc(t)
    if (.not. c%c_associated(p)) error stop

    write(env%output_unit, *) "real64 =", env%real64, y
    write(iso_fortran_env%output_unit, *) i8
end program
