! Namelist group of a fixed-size array of a derived type, declared in a
! procedure and written and read from its internal procedures (#16612),
! and of an allocatable array of a derived type
module nested_namelist_03_mod
    implicit none
contains
    subroutine s()
        type :: t
            integer :: v = 0
            real :: r = 0
        end type
        type(t) :: x(2), y(2, 3)
        type(t), allocatable :: z(:)
        integer :: i, j, u
        namelist /nl/ x, y, z
        allocate(z(3))
        do i = 1, 2
            x(i)%v = i
            x(i)%r = 10 * i
            do j = 1, 3
                y(i, j)%v = 10 * i + j
                y(i, j)%r = -(10 * i + j)
            end do
        end do
        do i = 1, 3
            z(i)%v = 100 * i
            z(i)%r = 0.5 * i
        end do
        open(newunit=u, status="scratch")
        call write_nl()
        x = t()
        y = t()
        z = t()
        rewind(u)
        call read_nl()
        close(u)
        do i = 1, 2
            if (x(i)%v /= i) error stop
            if (abs(x(i)%r - 10 * i) > 1e-6) error stop
            do j = 1, 3
                if (y(i, j)%v /= 10 * i + j) error stop
                if (abs(y(i, j)%r + (10 * i + j)) > 1e-6) error stop
            end do
        end do
        do i = 1, 3
            if (z(i)%v /= 100 * i) error stop
            if (abs(z(i)%r - 0.5 * i) > 1e-6) error stop
        end do
        call write_stdout()
    contains
        subroutine write_nl()
            write(u, nml=nl)
        end subroutine
        subroutine read_nl()
            read(u, nml=nl)
        end subroutine
        subroutine write_stdout()
            write(*, nml=nl)
        end subroutine
    end subroutine
end module

program nested_namelist_03
    use nested_namelist_03_mod, only: s
    implicit none
    call s()
end program
