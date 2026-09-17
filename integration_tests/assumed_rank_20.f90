module assumed_rank_20_mod
contains
    subroutine read_assumed_shape(u, x)
        integer, intent(in) :: u
        real(8), intent(inout) :: x(:)

        read(u, '(3F6.1)') x
    end subroutine

    subroutine read_assumed_shape_chars(u, x)
        integer, intent(in) :: u
        character(len=3), intent(inout) :: x(:)

        read(u, '(2A3)') x
    end subroutine

    subroutine read_assumed_shape2(u, x)
        integer, intent(in) :: u
        real(8), intent(inout) :: x(:,:)

        read(u, '(6F6.1)') x
    end subroutine

    subroutine read_assumed_shape_ints(u, x)
        integer, intent(in) :: u
        integer, intent(inout) :: x(:)

        read(u, '(3I3)') x
    end subroutine

    subroutine read_assumed_shape_logicals(u, x)
        integer, intent(in) :: u
        logical, intent(inout) :: x(:)

        read(u, '(3L1)') x
    end subroutine

    subroutine read_assumed_rank(u, x)
        integer, intent(in) :: u
        real(8), intent(inout) :: x(..)

        select rank (x)
        rank (1)
            read(u, '(3F6.1)') x
        rank default
            error stop
        end select
    end subroutine
end module

program assumed_rank_20
    use assumed_rank_20_mod
    implicit none

    real(8) :: a(3), b(3)
    real(8) :: c(6)
    real(8) :: d(5,3), z(3)
    integer :: iz(3)
    logical :: l(3)
    character(len=3) :: s(2)

    a = 0.0d0
    call fill_file_and_read_shape(a)
    call check_values(a, 1.0d0, 2.0d0, 3.0d0)

    c = -1.0d0
    call fill_file_and_read_shape(c(1:6:2))
    call check_strided_values(c)

    d = -1.0d0
    call fill_file_and_read_shape2(d(1:5:2,1:2))
    call check_rank2_strided_values(d)

    z = -1.0d0
    call fill_file_and_read_shape(z(1:0))
    if (any(z /= -1.0d0)) error stop

    iz = -1
    call fill_file_and_read_shape_ints(iz(1:0))
    if (any(iz /= -1)) error stop

    l = .false.
    call fill_file_and_read_shape_logicals(l)
    call check_logical_values(l)

    s = '---'
    call fill_file_and_read_shape_chars(s)
    call check_char_values(s)

    b = 0.0d0
    call fill_file_and_read_rank(b)
    call check_values(b, 4.0d0, 5.0d0, 6.0d0)

contains
    subroutine fill_file_and_read_shape(x)
        real(8), intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_shape.tmp', status='replace', &
             action='readwrite')
        write(u, '(3F6.1)') 1.0d0, 2.0d0, 3.0d0
        rewind(u)
        call read_assumed_shape(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_shape_chars(x)
        character(len=3), intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_char_shape.tmp', status='replace', &
             action='readwrite')
        write(u, '(2A3)') 'abc', 'def'
        rewind(u)
        call read_assumed_shape_chars(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_shape2(x)
        real(8), intent(inout) :: x(:,:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_shape2.tmp', status='replace', &
             action='readwrite')
        write(u, '(6F6.1)') 1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0
        rewind(u)
        call read_assumed_shape2(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_shape_ints(x)
        integer, intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_int_shape.tmp', status='replace', &
             action='readwrite')
        write(u, '(3I3)') 1, 2, 3
        rewind(u)
        call read_assumed_shape_ints(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_shape_logicals(x)
        logical, intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_logical_shape.tmp', &
             status='replace', action='readwrite')
        write(u, '(3L1)') .true., .false., .true.
        rewind(u)
        call read_assumed_shape_logicals(u, x)
        close(u, status='delete')
    end subroutine

    subroutine fill_file_and_read_rank(x)
        real(8), intent(inout) :: x(:)
        integer :: u

        open(newunit=u, file='assumed_rank_20_rank.tmp', status='replace', &
             action='readwrite')
        write(u, '(3F6.1)') 4.0d0, 5.0d0, 6.0d0
        rewind(u)
        call read_assumed_rank(u, x)
        close(u, status='delete')
    end subroutine

    subroutine check_values(x, e1, e2, e3)
        real(8), intent(in) :: x(:)
        real(8), intent(in) :: e1, e2, e3

        if (size(x) /= 3) error stop
        if (x(1) /= e1) error stop
        if (x(2) /= e2) error stop
        if (x(3) /= e3) error stop
    end subroutine

    subroutine check_strided_values(x)
        real(8), intent(in) :: x(:)

        if (size(x) /= 6) error stop
        if (x(1) /= 1.0d0) error stop
        if (x(2) /= -1.0d0) error stop
        if (x(3) /= 2.0d0) error stop
        if (x(4) /= -1.0d0) error stop
        if (x(5) /= 3.0d0) error stop
        if (x(6) /= -1.0d0) error stop
    end subroutine

    subroutine check_rank2_strided_values(x)
        real(8), intent(in) :: x(:,:)

        if (size(x, 1) /= 5) error stop
        if (size(x, 2) /= 3) error stop
        if (x(1,1) /= 1.0d0) error stop
        if (x(3,1) /= 2.0d0) error stop
        if (x(5,1) /= 3.0d0) error stop
        if (x(1,2) /= 4.0d0) error stop
        if (x(3,2) /= 5.0d0) error stop
        if (x(5,2) /= 6.0d0) error stop
        if (x(2,1) /= -1.0d0) error stop
        if (x(4,1) /= -1.0d0) error stop
        if (any(x(:,3) /= -1.0d0)) error stop
    end subroutine

    subroutine check_logical_values(x)
        logical, intent(in) :: x(:)

        if (size(x) /= 3) error stop
        if (.not. x(1)) error stop
        if (x(2)) error stop
        if (.not. x(3)) error stop
    end subroutine

    subroutine check_char_values(x)
        character(len=3), intent(in) :: x(:)

        if (size(x) /= 2) error stop
        if (x(1) /= 'abc') error stop
        if (x(2) /= 'def') error stop
    end subroutine
end program
