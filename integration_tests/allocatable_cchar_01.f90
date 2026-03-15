program allocatable_cchar_01
    use, intrinsic :: iso_c_binding, only: c_char
    implicit none
    character(kind=c_char, len=:), allocatable :: msg
    call hello(msg)
    if (msg /= "Hello from Fortran") error stop
    print *, msg
contains
    subroutine hello(msg) bind(c)
        use, intrinsic :: iso_c_binding, only: c_char
        character(kind=c_char, len=:), allocatable, intent(out) :: msg
        msg = "Hello from Fortran"
    end subroutine hello
end program allocatable_cchar_01
