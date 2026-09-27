subroutine external_26_add(x, y, z)
    implicit none
    integer, intent(in) :: x, y
    integer, intent(out) :: z
    z = x + y
end subroutine external_26_add

integer function external_26_twice(x)
    implicit none
    integer, intent(in) :: x
    external_26_twice = 2*x
end function external_26_twice

! An external procedure with its own use statement.
subroutine external_26_shift(x, y)
    use iso_fortran_env, only: numeric_storage_size
    implicit none
    integer, intent(in) :: x
    integer, intent(out) :: y
    y = 10*x + numeric_storage_size
end subroutine external_26_shift
