program inquire_size_regions
    use iso_fortran_env, only: int64
    implicit none
    integer :: n, i, tid
    integer(int64) :: wide_n
    logical :: opened

    open(20, status="scratch")
    !$omp parallel num_threads(1)
    close(20)
    inquire(20, size=n, opened=opened)
    !$omp end parallel
    if (opened) error stop 1

    open(20, status="scratch", access="stream", form="unformatted")
    !$omp parallel num_threads(1)
    write(20) "abc"
    inquire(20, size=n, opened=opened)
    inquire(20, size=wide_n)
    !$omp end parallel
    if (.not. opened .or. n /= 3 .or. wide_n /= 3_int64) error stop 2

    opened = .false.
    n = 17
    if (opened) then
        inquire(20, size=n, opened=opened)
    end if
    if (opened .or. n /= 17) error stop 3
    do i = 1, 3
        if (i /= 2) then
            block
                write(20) "ab"
                inquire(20, size=n)
                inquire(20, size=wide_n)
                if (n /= 3 + 2 * ((i + 1) / 2)) error stop 4
                if (wide_n /= int(n, int64)) error stop 5
            end block
        end if
    end do
    close(20)

    open(20, status="scratch", access="stream", form="unformatted")
    open(21, status="scratch", access="stream", form="unformatted")
    write(20) "abc"
    write(21) "abcde"
    flush(20)
    flush(21)
    n = 71
    !$omp parallel do num_threads(2) private(n, i) schedule(static)
    do tid = 0, 1
        do i = 1, 8192
            inquire(20 + tid, size=n)
            if (n /= 3 + 2 * tid) error stop 7
            if (mod(i, 2) == 0) then
                block
                    integer :: local_n
                    inquire(20 + tid, size=local_n)
                    if (local_n /= n) error stop 8
                end block
            end if
        end do
    end do
    !$omp end parallel do
    ! Without OpenMP the last iteration assigns the enclosing n.
    if (n /= 71 .and. n /= 5) error stop 9
    close(20)
    close(21)
end program
