program read_130
    ! List-directed input accepts Q as an exponent letter, like E and D (#18199)
    implicit none
    real :: r4, a4(2)
    real(8) :: r8, a8(2)
    complex :: c4
    complex(8) :: c8
    character(len=32) :: s
    integer :: ios, u

    s = '1.23Q+04'
    read(s, *, iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop

    s = '1.23q4'
    read(s, *, iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop

    s = '-2.5Q-2'
    read(s, *, iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 + 0.025d0) > 1.0d-15) error stop

    s = '1.25q+3'
    read(s, *, iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 - 1250.0d0) > 1.0d-12) error stop

    s = '1.5Q1, 2.5q-1'
    read(s, *, iostat=ios) a4
    if (ios /= 0) error stop
    if (abs(a4(1) - 15.0) > 1.0e-5) error stop
    if (abs(a4(2) - 0.25) > 1.0e-7) error stop

    s = '7.5q-1 -3Q2'
    read(s, *, iostat=ios) a8
    if (ios /= 0) error stop
    if (abs(a8(1) - 0.75d0) > 1.0d-15) error stop
    if (abs(a8(2) + 300.0d0) > 1.0d-12) error stop

    s = '(1.5Q1, -2.5q-1)'
    read(s, *, iostat=ios) c4
    if (ios /= 0) error stop
    if (abs(real(c4) - 15.0) > 1.0e-5) error stop
    if (abs(aimag(c4) + 0.25) > 1.0e-7) error stop

    s = '(1.25q+2, 4Q0)'
    read(s, *, iostat=ios) c8
    if (ios /= 0) error stop
    if (abs(real(c8) - 125.0d0) > 1.0d-12) error stop
    if (abs(aimag(c8) - 4.0d0) > 1.0d-12) error stop

    open(newunit=u, status='scratch', form='formatted', action='readwrite')
    write(u, '(A)') '  1.23Q+04'
    write(u, '(A)') ' -7.5q-01'
    write(u, '(A)') ' 1.5Q1, 2.5q-1'
    write(u, '(A)') ' 7.5q-1 -3Q2'
    write(u, '(A)') ' (1.5Q1, -2.5q-1)'
    write(u, '(A)') ' (1.25q+2, 4Q0)'
    rewind(u)
    read(u, *, iostat=ios) r4
    if (ios /= 0) error stop
    if (abs(r4 - 12300.0) > 1.0e-2) error stop
    read(u, *, iostat=ios) r8
    if (ios /= 0) error stop
    if (abs(r8 + 0.75d0) > 1.0d-15) error stop
    read(u, *, iostat=ios) a4
    if (ios /= 0) error stop
    if (abs(a4(1) - 15.0) > 1.0e-5) error stop
    if (abs(a4(2) - 0.25) > 1.0e-7) error stop
    read(u, *, iostat=ios) a8
    if (ios /= 0) error stop
    if (abs(a8(1) - 0.75d0) > 1.0d-15) error stop
    if (abs(a8(2) + 300.0d0) > 1.0d-12) error stop
    read(u, *, iostat=ios) c4
    if (ios /= 0) error stop
    if (abs(real(c4) - 15.0) > 1.0e-5) error stop
    if (abs(aimag(c4) + 0.25) > 1.0e-7) error stop
    read(u, *, iostat=ios) c8
    if (ios /= 0) error stop
    if (abs(real(c8) - 125.0d0) > 1.0d-12) error stop
    if (abs(aimag(c8) - 4.0d0) > 1.0d-12) error stop
    close(u)

    print *, "ok"
end program
