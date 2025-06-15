program implicit_type_promotion
    use iso_fortran_env, only: int8
    implicit none
    integer(int8), allocatable :: arr(:)
    integer :: N = 5

    arr = [-128, 127, 0, -1, 1]
    call radix_sort(N, arr)

contains
    subroutine radix_sort(N, arr)
        integer, intent(in) :: N
        integer(kind=int8), dimension(N), intent(inout) :: arr
        integer :: bin_idx
 
        bin_idx = arr(1)

        if (bin_idx /= -128) error stop

    end subroutine radix_sort
end program implicit_type_promotion