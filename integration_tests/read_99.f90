program read_99
    implicit none
    complex :: expected(3)
    integer :: unit

    expected = [(1.0, -1.0), (2.0, -2.0), (3.0, -3.0)]
    open(newunit=unit, status='scratch', access='direct', &
        form='unformatted', recl=24)
    write(unit, rec=1) expected
    call check_values(unit, 3, expected)
    call check_real_overlong_token()
    call check_double_overlong_token()
    call check_real_premature_eof()
    call check_double_premature_eof()
    close(unit)
contains
    subroutine check_values(unit, n, expected_values)
        integer, intent(in) :: unit, n
        complex, intent(in) :: expected_values(n)
        complex :: values(n)
        integer :: i

        read(unit, rec=1) (values(i), i = 1, n)
        if (any(values /= expected_values)) error stop
    end subroutine
    subroutine check_real_overlong_token()
        integer :: unit, ios
        real :: values(3)
        character(len=100) :: long_token

        long_token = repeat('x', len(long_token))
        values = -77.25
        open(newunit=unit, status='scratch', form='formatted')
        write(unit, '(A,1X,A,1X,A)') '1.25', long_token, '9.0'
        rewind(unit)
        read(unit, *, iostat=ios) values
        close(unit)

        if (ios <= 0) error stop
        if (values(1) /= 1.25) error stop
        if (any(values(2:3) /= -77.25)) error stop
    end subroutine

    subroutine check_double_overlong_token()
        integer :: unit, ios
        double precision :: values(3)
        character(len=100) :: long_token

        long_token = repeat('x', len(long_token))
        values = -77.25d0
        open(newunit=unit, status='scratch', form='formatted')
        write(unit, '(A,1X,A,1X,A)') '1.25', long_token, '9.0'
        rewind(unit)
        read(unit, *, iostat=ios) values
        close(unit)

        if (ios <= 0) error stop
        if (values(1) /= 1.25d0) error stop
        if (any(values(2:3) /= -77.25d0)) error stop
    end subroutine

    subroutine check_real_premature_eof()
        integer :: unit, ios
        real :: values(3)

        values = -88.5
        open(newunit=unit, status='scratch', form='formatted')
        write(unit, '(A)') '2.5'
        rewind(unit)
        read(unit, *, iostat=ios) values
        close(unit)

        if (ios >= 0) error stop
        if (values(1) /= 2.5) error stop
        if (any(values(2:3) /= -88.5)) error stop
    end subroutine

    subroutine check_double_premature_eof()
        integer :: unit, ios
        double precision :: values(3)

        values = -88.5d0
        open(newunit=unit, status='scratch', form='formatted')
        write(unit, '(A)') '2.5'
        rewind(unit)
        read(unit, *, iostat=ios) values
        close(unit)

        if (ios >= 0) error stop
        if (values(1) /= 2.5d0) error stop
        if (any(values(2:3) /= -88.5d0)) error stop
    end subroutine
end program
