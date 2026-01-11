program intrinsics_401
    use iso_fortran_env, only: logical8
    implicit none

    logical(kind=logical8) :: x
    x = .true.

    if (.not. x) error stop
end program intrinsics_401
