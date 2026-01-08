program read_23
    ! Test READ with implied-do loop into assumed-size array dummy argument
    ! Reproduces LAPACK sdrgvx.f pattern: READ(NIN,*) (STRU(I), I=1,N)
    implicit none
    real :: work(100)
    integer :: i

    open(10, file='_read_23_test.dat', status='replace')
    write(10, *) 1.5, 2.5, 3.5, 4.5
    close(10)

    call read_into_assumed(work, 4)

    close(10, status='delete')

    if (abs(work(1) - 1.5) > 1e-5) error stop
    if (abs(work(2) - 2.5) > 1e-5) error stop
    if (abs(work(3) - 3.5) > 1e-5) error stop
    if (abs(work(4) - 4.5) > 1e-5) error stop

    print *, "PASSED"

contains

    subroutine read_into_assumed(arr, n)
        integer, intent(in) :: n
        real, intent(out) :: arr(*)
        integer :: i

        open(10, file='_read_23_test.dat', status='old')
        read(10, *) (arr(i), i = 1, n)
        close(10)
    end subroutine

end program
