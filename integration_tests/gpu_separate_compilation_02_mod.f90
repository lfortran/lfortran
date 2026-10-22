! The module of gpu_separate_compilation_02. Its own compilation offloads its
! loops; a file that uses it, compiled with `--openmp` as well, reads these
! procedures from the module file and lowers none of their loops.
module gpu_separate_compilation_02_mod
    implicit none
contains

    subroutine scale(x, a)
        real, intent(inout) :: x(:)
        real, intent(in) :: a
        integer :: i
        do concurrent (i = 1:size(x))
            x(i) = a * x(i)
        end do
    end subroutine

    subroutine shift(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(x))
            y(i) = x(i) + i
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
