program allocate_22
    use iso_fortran_env, only: int64, int32
    implicit none
    integer(int64) :: array_size, stat
    integer(int32), allocatable :: buf(:)

    array_size = 10
    allocate( buf(0:array_size/2-1), stat=stat)
    if (stat /= 0) error stop
end program allocate_22