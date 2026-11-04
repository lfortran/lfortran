module loop_fusion_03_m
    implicit none
contains
    ! TARGET dummy arguments may be associated with overlapping storage
    ! (not fused).
    subroutine overlap(x, y, z, n)
        real, target :: x(:), y(:)
        real :: z(:)
        integer, intent(in) :: n
        integer :: i
        do i = 1, n
            x(i) = 100 + i
        end do
        do i = 1, n
            z(i) = y(i)
        end do
    end subroutine

    subroutine show(r)
        integer, intent(out) :: r
        integer :: k
        common /loop_fusion_03_blk/ k
        r = k
    end subroutine
end module

program loop_fusion_03
    ! Adjacent loop nests and index variables that look fusable, but whose
    ! fusion would change the result (with --fast).
    use loop_fusion_03_m
    implicit none
    real :: a(10), b(10), z(10)
    real, target :: t(11)
    integer :: i, j, k, m, r
    character(len=40) :: lines(4)
    common /loop_fusion_03_blk/ k
    namelist /nl/ m

    ! A bound that reads the loop variable the first loop assigns.
    a = 0
    b = 0
    i = 5
    do i = 1, i
        a(i) = 1
    end do
    do i = 1, i
        b(i) = 2
    end do
    if (int(sum(a)) /= 5 .or. int(sum(b)) /= 12) error stop 1

    ! The outer bound reads the variable of the inner loop.
    a = 0
    b = 0
    j = 2
    do i = 1, j
        do j = 1, 3
            a(j) = a(j) + 1
        end do
    end do
    do i = 1, j
        do j = 1, 3
            b(j) = b(j) + 1
        end do
    end do
    if (int(sum(a)) /= 6 .or. int(sum(b)) /= 12) error stop 2

    ! An index variable in a common block.
    a = [(real(i), i = 1, 10)]
    k = 1
    do i = 1, 10
        b(i) = a(k) + 1
        k = k + 1
    end do
    call show(r)
    if (r /= 11 .or. int(sum(b)) /= 65) error stop 3

    ! A loop variable in a common block.
    do i = 1, 10
        a(i) = i
    end do
    do k = 1, 10
        b(k) = a(k) * 2
    end do
    call show(r)
    if (r /= 11 .or. int(sum(b)) /= 110) error stop 4

    ! An index variable in a namelist group.
    m = 1
    do i = 1, 10
        b(i) = a(m) + 1
        m = m + 1
    end do
    lines = ""
    write(lines, nml=nl)
    if (all(index(lines, "11") == 0) .or. int(sum(b)) /= 65) error stop 5

    ! TARGET dummy arguments associated with overlapping sections.
    t = [(real(i), i = 1, 11)]
    z = 0
    call overlap(t(1:10), t(2:11), z, 10)
    if (any(z /= [(real(100 + i), i = 2, 10), 11.0])) error stop 6
    print *, "ok"
end program
