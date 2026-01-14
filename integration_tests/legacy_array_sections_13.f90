program legacy_array_sections_13
    implicit none
    real :: work(20)

    work = 1.0
    call process2d(work(5))

    if (work(5) /= 99.0) error stop

contains
    subroutine process2d(matrix)
        real, intent(inout) :: matrix(2, 2)
        matrix(1, 1) = 99.0
    end subroutine process2d
end program
