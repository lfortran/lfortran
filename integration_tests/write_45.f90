program write_45
    implicit none
    integer, parameter :: phi_count = 32768
    integer :: real_unit, complex_unit, phi_unit, matrix_unit, section_unit
    integer :: i, j
    real :: a(3, 4), row(4)
    complex :: z(2, 3), zrow(3)
    complex, allocatable :: PhiTab(:), PhiRead(:)
    complex, allocatable :: matrix(:,:), matrix_read(:,:), packed(:,:)

    a = reshape([11.0, 12.0, 13.0, 21.0, 22.0, 23.0, &
                 31.0, 32.0, 33.0, 41.0, 42.0, 43.0], shape(a))
    open(newunit=real_unit, status="scratch", access="direct", &
        form="unformatted", recl=4 * storage_size(a) / 8)
    write(real_unit, rec=1) a(2, 1:4)
    read(real_unit, rec=1) row
    close(real_unit)
    if (any(row /= [12.0, 22.0, 32.0, 42.0])) error stop

    z = reshape([cmplx(1.0, -1.0), cmplx(2.0, -2.0), &
                 cmplx(3.0, -3.0), cmplx(4.0, -4.0), &
                 cmplx(5.0, -5.0), cmplx(6.0, -6.0)], shape(z))
    open(newunit=complex_unit, status="scratch", access="direct", &
        form="unformatted", recl=3 * storage_size(z) / 8)
    write(complex_unit, rec=1) z(2, 1:3)
    read(complex_unit, rec=1) zrow
    close(complex_unit)
    if (any(zrow /= [cmplx(2.0, -2.0), cmplx(4.0, -4.0), &
                       cmplx(6.0, -6.0)])) error stop

    allocate(PhiTab(phi_count), PhiRead(phi_count))
    do i = 1, phi_count
        PhiTab(i) = cmplx(real(i), -real(i))
    end do
    open(newunit=phi_unit, status="scratch", access="direct", &
        form="unformatted", recl=phi_count * storage_size(PhiTab) / 8)
    write(phi_unit, rec=1) PhiTab
    read(phi_unit, rec=1) PhiRead
    close(phi_unit)
    if (any(PhiRead /= PhiTab)) error stop

    allocate(matrix(3, 4), matrix_read(3, 4), packed(2, 2))
    do j = 1, 4
        do i = 1, 3
            matrix(i, j) = cmplx(real(10 * j + i), real(i - j))
        end do
    end do
    open(newunit=matrix_unit, status="scratch", access="direct", &
        form="unformatted", recl=size(matrix) * storage_size(matrix) / 8)
    write(matrix_unit, rec=1) matrix
    read(matrix_unit, rec=1) matrix_read
    close(matrix_unit)
    if (any(matrix_read /= matrix)) error stop

    open(newunit=section_unit, status="scratch", access="direct", &
        form="unformatted", recl=size(packed) * storage_size(packed) / 8)
    write(section_unit, rec=1) matrix(1:3:2, 1:4:2)
    read(section_unit, rec=1) packed
    close(section_unit)
    if (any(packed /= matrix(1:3:2, 1:4:2))) error stop
end program
