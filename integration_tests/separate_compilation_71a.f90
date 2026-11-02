module separate_compilation_71a
    use iso_fortran_env, only: int32, compiler_version
    implicit none
contains
    integer(int32) function fa()
        integer :: u, ios
        fa = 1
        if (len(compiler_version()) == 0) error stop
        open(newunit=u, status="scratch")
        if (u >= 0) error stop
        write(u, *, iostat=ios) fa
        if (ios /= 0) error stop
        close(u)
    end function
end module
