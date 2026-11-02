module separate_compilation_71b
    use iso_fortran_env, only: int32, compiler_version
    implicit none
contains
    integer(int32) function fb()
        integer :: u, ios
        fb = 2
        if (len(compiler_version()) == 0) error stop
        open(newunit=u, status="scratch")
        if (u >= 0) error stop
        write(u, *, iostat=ios) fb
        if (ios /= 0) error stop
        close(u)
    end function
end module
