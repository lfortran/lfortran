module function_result_preparation_05_mod
    implicit none
    integer :: calls = 0
contains
    function words(n, count) result(value)
        integer, intent(in) :: n, count
        character(n), allocatable :: value(:)
        if (allocated(value)) error stop 7
        calls = calls + 1
        call fill(value, count)
    end function

    subroutine fill(value, count)
        character(*), allocatable, intent(out) :: value(:)
        integer, intent(in) :: count
        allocate(value(count))
        value = repeat("x", len(value))
    end subroutine

    function scalar(n) result(value)
        integer, intent(in) :: n
        character(n), allocatable :: value
        if (allocated(value)) error stop 8
        call fill_scalar(value)
    end function

    subroutine fill_scalar(value)
        character(*), allocatable, intent(out) :: value
        allocate(value)
        value = repeat("x", len(value))
    end subroutine

    function grid(n, count) result(value)
        integer, intent(in) :: n, count
        character(n), allocatable :: value(:,:)
        if (allocated(value)) error stop 9
        allocate(value(count, 2))
        value = repeat("x", n)
    end function

    function copy(value) result(result)
        character(*), intent(in) :: value(:)
        character(len(value)), allocatable :: result(:)
        allocate(result(size(value)))
        result = value
    end function

    subroutine check(value, n, count)
        character(*), intent(in) :: value(:)
        integer, intent(in) :: n, count
        if (len(value) /= n) error stop 1
        if (size(value) /= count) error stop 2
        if (any(value /= repeat("x", n))) error stop 3
    end subroutine

    subroutine check_scalar(value, n)
        character(*), intent(in) :: value
        integer, intent(in) :: n
        if (len(value) /= n) error stop 10
        if (value /= repeat("x", n)) error stop 11
    end subroutine

    subroutine check_grid(value, n, count)
        character(*), intent(in) :: value(:,:)
        integer, intent(in) :: n, count
        if (len(value) /= n) error stop 12
        if (any(shape(value) /= [count, 2])) error stop 13
        if (any(value /= repeat("x", n))) error stop 14
    end subroutine
end module

program function_result_preparation_05
    use function_result_preparation_05_mod
    implicit none
    character(:), allocatable :: value(:)
    integer :: i, n, count
    integer, parameter :: lengths(*) = [3, 1, 0, 5]
    do i = 1, size(lengths)
        n = lengths(i)
        call check_scalar(scalar(n), n)
        do count = 2, 0, -2
            calls = 0
            call check(words(n, count), n, count)
            if (calls /= 1) error stop 4
            calls = 0
            value = words(n, count)
            call check(value, n, count)
            if (calls /= 1) error stop 5
            calls = 0
            call check(copy(words(n, count)), n, count)
            if (calls /= 1) error stop 6
            call check_grid(grid(n, count), n, count)
        end do
    end do
end program
