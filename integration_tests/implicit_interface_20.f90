program implicit_interface_20
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: a(2)

    a(1) = 1.0d0
    a(2) = 2.0d0
    call wrapper(a)
end program implicit_interface_20

subroutine wrapper(a)
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: a(*)
    external :: worker

    call worker(a)
end subroutine wrapper

subroutine worker(a)
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: a(*)

    if (a(1) /= 1.0d0) error stop
    if (a(2) /= 2.0d0) error stop
    print *, a(1)
end subroutine worker
