program read_129
    ! Formatted input accepts Q as an exponent letter, like E and D (#18199)
    implicit none
    real :: r4
    real(8) :: r8
    character(len=16) :: s
    integer :: ios, u

    s = '1.23Q+04'
    read(s, '(E8.2)', iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop

    s = '1.23q+04'
    read(s, '(E8.2)', iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop

    s = '1.23Q4'
    read(s, '(E6.2)', iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop

    s = '2.5q-2'
    read(s, '(F6.2)', iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 0.025) > 1.0e-7) error stop

    s = '1.23Q+04'
    read(s, '(F8.2)', iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 - 12300.0d0) > 1.0d-9) error stop

    s = '-4.5q+03'
    read(s, '(E8.2)', iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 + 4500.0d0) > 1.0d-9) error stop

    s = '1.25Q-1'
    read(s, '(G7.2)', iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 - 0.125d0) > 1.0d-12) error stop

    open(newunit=u, status='scratch', form='formatted', action='readwrite')
    write(u, '(A)') '  1.23Q+04'
    write(u, '(A)') ' -7.5q-01'
    rewind(u)
    read(u, '(E10.2)', iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop
    read(u, '(F9.2)', iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 + 0.75d0) > 1.0d-12) error stop
    close(u)

    print *, "ok"
end program
