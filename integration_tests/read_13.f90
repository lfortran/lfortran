program read_13
    implicit none

    integer :: n
    real :: x(10)

    n = 10
    call read_into_assumed_size(n, x)
contains
    subroutine read_into_assumed_size(n, a)
        implicit none

        integer, intent(in) :: n
        real, intent(out) :: a(*)
        integer :: i

        read(10, *) (a(i), i=1, n)
    end subroutine read_into_assumed_size
end program read_13
