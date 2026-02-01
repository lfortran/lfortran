program allocate_37
    implicit none
    call test(n=100)
    call test(n=200)
contains
    subroutine test(n)
        integer, intent(in) :: n
        integer, allocatable :: arr(:)
        allocate(arr(2), source=n)
        if (arr(1) /= n) error stop
        if (arr(2) /= n) error stop
        print *, 'subroutine result', arr(1), arr(2)
    end subroutine test
end program
