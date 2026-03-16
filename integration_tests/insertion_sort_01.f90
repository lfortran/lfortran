program insertion_sort_01
    implicit none
    integer, parameter :: int_size = selected_int_kind(8)
    character(len=4), allocatable :: array(:)
    character(len=4) :: expected(5)
    integer(int_size) :: i

    array = ['zulu', 'echo', 'brav', 'alph', 'yank']
    call insertion_sort(array)

    expected = ['alph', 'brav', 'echo', 'yank', 'zulu']
    do i = 1, size(array, kind=int_size)
        if (array(i) /= expected(i)) error stop
    end do
contains
    subroutine insertion_sort(array)
        character(len=*), intent(inout) :: array(0:)
        integer(int_size) :: i, j
        character(len=len(array)) :: key

        do j = 1, size(array, kind=int_size) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
                if (array(i) <= key) exit
                array(i+1) = array(i)
                i = i - 1
            end do
            array(i+1) = key
        end do
    end subroutine insertion_sort
end program insertion_sort_01
