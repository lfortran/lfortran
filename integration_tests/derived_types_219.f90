! Rank 2 and rank 3 array components with a scalar default initializer,
! declared in a program (#14211).
program derived_types_219
    implicit none
    type :: sometype
        integer :: i
        integer, dimension(2,2) :: x = 1
    end type sometype

    type :: t3
        integer :: i = 0
        integer :: a(2,3) = 7
        real :: r(2,2,2) = 1.5
        character(len=3) :: c(2,2) = "ab"
        logical :: l(3,2) = .true.
        integer :: p(2,3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        real(8) :: d(2,2,3) = reshape([1d0, 2d0, 3d0, 4d0, 5d0, 6d0, &
            7d0, 8d0, 9d0, 10d0, 11d0, 12d0], [2, 2, 3])
    end type t3

    integer :: i_
    type(sometype) :: y
    type(t3) :: u, v, w

    ! The reproducer from the issue
    y = sometype(i=1)
    print *, sum(y%x)
    if (sum(y%x) /= 4) error stop
    if (any(shape(y%x) /= [2, 2])) error stop

    ! A plain declaration, no constructor
    if (u%i /= 0) error stop
    if (any(u%a /= 7) .or. size(u%a) /= 6) error stop
    if (any(u%r /= 1.5) .or. size(u%r) /= 8) error stop
    if (any(u%c /= "ab ") .or. len(u%c) /= 3) error stop
    if (.not. all(u%l)) error stop
    if (u%p(2, 3) /= 6 .or. u%p(1, 2) /= 3) error stop
    if (u%d(2, 1, 3) /= 10.0d0 .or. sum(u%d) /= 78.0d0) error stop

    ! Constructor with the defaults filled in
    v = t3(i=5)
    if (v%i /= 5) error stop
    if (any(v%a /= 7)) error stop
    if (any(v%r /= 1.5)) error stop
    if (any(v%c /= "ab")) error stop
    if (.not. all(v%l)) error stop
    if (v%p(2, 3) /= 6 .or. v%p(1, 2) /= 3) error stop
    if (v%d(2, 1, 3) /= 10.0d0) error stop

    ! Constructor with the array components given
    w = t3(1, reshape([(i_, i_ = 1, 6)], [2, 3]), 2.0, "xyz", .false., 3, 4.0d0)
    if (w%a(2, 3) /= 6 .or. w%a(1, 2) /= 3) error stop
    if (any(w%r /= 2.0)) error stop
    if (any(w%c /= "xyz")) error stop
    if (any(w%l)) error stop
    if (any(w%p /= 3)) error stop
    if (any(w%d /= 4.0d0)) error stop

    w = t3(a=9, l=.false.)
    if (any(w%a /= 9)) error stop
    if (any(w%l)) error stop
    if (any(w%r /= 1.5)) error stop
    if (w%p(2, 3) /= 6) error stop
end program derived_types_219
