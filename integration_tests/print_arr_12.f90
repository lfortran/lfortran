! An assumed-rank dummy inside SELECT RANK, associated with a strided actual
program print_arr_12
implicit none
integer :: x(6) = [1, 2, 3, 4, 5, 6]
integer :: y(3, 2) = reshape([1, 2, 3, 4, 5, 6], [3, 2])
call rnk(x(1:6:2), '  1  3  5')
call rnk(y(1:3:2, :), '  1  3  4  6')
call rnk(x, '  1  2  3  4  5  6')
call rnk(x(4), '  4')
contains
subroutine rnk(a, expected)
    integer, intent(in) :: a(..)
    character(len=*), intent(in) :: expected
    character(len=40) :: buf
    select rank (a)
    rank (0)
        print *, a
        write(buf, '(I3)') a
    rank (1)
        print *, a
        write(buf, '(6I3)') a
    rank (2)
        print *, a
        write(buf, '(6I3)') a
    end select
    if (trim(buf) /= expected) error stop
end subroutine
end program
