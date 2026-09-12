! A deferred-length allocatable CHARACTER array passed through an implicit
! interface to a separately compiled procedure. The synthesized dummy drops the
! actual's ALLOCATABLE wrapper, so its element length must become an assumed
! length rather than staying deferred. The callee is implicit_interface_64b.f90.
program implicit_interface_64
    implicit none
    character(len=:), allocatable :: w(:)
    character(len=:), allocatable :: s

    allocate(character(len=5) :: w(3))
    w(1) = 'aa'
    w(2) = 'bb'
    w(3) = 'cc'

    call take_assumed_size(w)
    call take_assumed_shape_len(w, 3)

    ! A deferred-length scalar goes through the same synthesis path.
    s = 'hello'
    call take_scalar(s)

    print *, 'OK'
end program implicit_interface_64
