program complex10_unformatted_01
    ! Unformatted write and read of complex(10) scalars and arrays
    implicit none
    complex(10) :: z, w, a(3), b(3)
    integer :: u, i

    z = (1.5_10, -2.5_10)
    a = [z, 2*z, 3*z]
    open(newunit=u, file='complex10_unformatted_01.bin', form='unformatted', status='replace')
    write(u) z
    write(u) a
    close(u)
    open(newunit=u, file='complex10_unformatted_01.bin', form='unformatted', status='old')
    read(u) w
    read(u) b
    close(u, status='delete')
    if (w /= z) error stop 1
    do i = 1, 3
        if (b(i) /= a(i)) error stop 2
    end do
end program
