program arrays_reshape_54
    use iso_fortran_env, only: int16
    implicit none
    call repeated_reshape(64)
    call repeated_reshape(32)
contains
    subroutine repeated_reshape(width)
        integer, intent(in) :: width
        integer(int16), allocatable :: input(:), output(:,:), retained(:,:), small(:)
        integer :: i
        allocate(input(width*width), output(width,width), retained(width,width))
        input = 7
        do i = 1, 2048
            input(1) = int(i, int16)
            output = reshape(input, [width, width])
            if (output(1,1) /= int(i, int16)) error stop
            if (output(width,width) /= 7) error stop
            if (i == 1) retained = output
            input(1) = -int(i, int16)
            if (output(1,1) /= int(i, int16)) error stop
        end do
        allocate(small(width))
        small = reshape(input, [width])
        input(1) = 0
        if (small(1) /= -2048 .or. small(width) /= 7) error stop
        output(2,1) = 9
        output(1,2) = 11
        output = reshape(output, [width, width], order=[2,1])
        if (output(1,2) /= 9 .or. output(2,1) /= 11) error stop
        if (retained(1,1) /= 1) error stop
        if (retained(width,width) /= 7) error stop
    end subroutine
end program
