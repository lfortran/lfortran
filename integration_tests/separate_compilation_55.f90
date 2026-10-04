! Uses the module of separate_compilation_55a.f90, compiled on its own: this
! object file must not define the storage of the module procedure's local
! again, which the module's object file defines.
program separate_compilation_55
    use iso_c_binding, only: c_int
    use separate_compilation_55_m, only: p
    implicit none
    integer(c_int) :: x
    x = 2
    call p(x)
    if (x /= 9) error stop 1
    print *, "ok"
end program separate_compilation_55
