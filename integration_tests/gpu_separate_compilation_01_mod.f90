! The module of gpu_separate_compilation_01. Its own compilation is the one
! that offloads its loops (or runs them on the CPU): a file that uses it reads
! these procedures from the module file and compiles none of them.
module gpu_separate_compilation_01_mod
    implicit none
contains

    subroutine scale(x)
        real, intent(inout) :: x(:)
        integer :: i
        do concurrent (i = 1:size(x))
            x(i) = 3 * x(i)
        end do
    end subroutine

    ! A loop that prints cannot be offloaded, so the compilation of this
    ! module needs `--gpu-allow-cpu-fallback`; a file that uses it must not.
    subroutine report_negative(x)
        real, intent(in) :: x(:)
        integer :: i
        do concurrent (i = 1:size(x))
            if (x(i) < 0) print *, "negative at", i
        end do
    end subroutine

    ! Called from a parallel loop of the program: the kernel built there
    ! runs this procedure's own loop serially.
    pure real function tri(n)
        integer, intent(in) :: n
        integer :: i
        real :: t(8)
        do concurrent (i = 1:8)
            t(i) = i * n
        end do
        tri = 0
        do i = 1, 8
            tri = tri + t(i)
        end do
    end function

end module
