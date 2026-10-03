module block_19_mod
    implicit none
    type :: counted
        integer :: c = 0
    contains
        final :: finalize_counted
    end type
    integer :: nfin = 0, lastfin = 0
contains
    subroutine finalize_counted(self)
        type(counted), intent(inout) :: self
        nfin = nfin + 1
        lastfin = self%c
    end subroutine

    subroutine select_rank_exit(x, n)
        integer, intent(in) :: x(..)
        integer, intent(out) :: n
        integer :: i
        n = 0
        do i = 1, 5
            sr: select rank (x)
            rank (1)
                if (i == 2) exit
            end select sr
            n = n + 1
        end do
    end subroutine
end module

program block_19
    ! An EXIT without a construct name exits the innermost DO construct,
    ! also when it is nested in a BLOCK or in another named construct
    use block_19_mod
    implicit none
    integer :: i, j, n, a(3)
    class(*), allocatable :: u, v(:)

    n = 0
    do i = 1, 5
        block
            if (i == 2) exit
        end block
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop

    n = 0
    do i = 1, 5
        block
            real :: x(4)
            x = 1.0
            block
                integer :: y(10)
                y = i
                block
                    if (y(3) == 3) exit
                end block
            end block
            n = n + 1
        end block
        n = n + 10
    end do
    if (i /= 3 .or. n /= 22) error stop

    n = 0
    i = 0
    do while (i < 5)
        i = i + 1
        block
            if (i == 2) cycle
            if (i == 4) exit
        end block
        n = n + 1
    end do
    if (i /= 4 .or. n /= 2) error stop

    ! A named EXIT finalizes the locals of the BLOCKs it leaves
    n = 0
    nfin = 0
    outer: do i = 1, 5
        do j = 1, 3
            block
                type(counted) :: p
                p%c = i
                block
                    type(counted) :: q
                    q%c = j
                    if (i == 2 .and. j == 2) exit outer
                end block
            end block
            n = n + 1
        end do
    end do outer
    if (i /= 2 .or. j /= 2 .or. n /= 4) error stop
    if (nfin /= 10) error stop

    n = 0
    nfin = 0
    do i = 1, 3
        blk: block
            type(counted) :: p
            p%c = i
            block
                type(counted) :: q
                integer :: b(2)
                q%c = i
                b = i
                if (b(1) == 2) exit blk
            end block
            n = n + 1
        end block blk
        n = n + 10
        if (i == 3) exit
    end do
    if (i /= 3 .or. n /= 32) error stop
    if (nfin /= 6) error stop

    n = 0
    do i = 1, 5
        chk: if (i == 2) then
            exit
        end if chk
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop

    n = 0
    do i = 1, 5
        chk2: if (i > 10) then
            n = n + 100
        end if chk2
        if (i == 2) exit
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop

    n = 0
    do i = 1, 5
        sc: select case (i)
        case (2)
            exit
        end select sc
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop

    u = 1
    n = 0
    do i = 1, 5
        select type (u)
        type is (integer)
            if (i == 2) exit
        end select
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop

    call select_rank_exit(a, n)
    if (n /= 1) error stop

    ! Changes to a character selector made in a SELECT TYPE arm left by
    ! EXIT are kept
    allocate(v(2), source=['ab', 'cd'])
    do i = 1, 3
        select type (v)
        type is (character(*))
            v(1) = 'zz'
            if (i == 1) exit
        end select
    end do
    if (i /= 1) error stop
    select type (v)
    type is (character(*))
        if (v(1) /= 'zz' .or. v(2) /= 'cd') error stop
    class default
        error stop
    end select
    sel: block
        select type (v)
        type is (character(*))
            v(2) = 'yy'
            exit sel
        end select
    end block sel
    select type (v)
    type is (character(*))
        if (v(1) /= 'zz' .or. v(2) /= 'yy') error stop
    class default
        error stop
    end select

    ! The locals of a BLOCK left by EXIT are still finalized
    n = 0
    nfin = 0
    do i = 1, 5
        block
            type(counted) :: x
            integer, allocatable :: z(:)
            allocate(z(3))
            x%c = i
            if (i == 2) exit
        end block
        n = n + 1
    end do
    if (i /= 2 .or. n /= 1) error stop
    if (nfin /= 2 .or. lastfin /= 2) error stop
    print *, "ok"
end program
