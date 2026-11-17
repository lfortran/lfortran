program findloc_value_component_01
    ! Allocatable and pointer scalars of a nondefault kind, as variables and
    ! as derived-type components, converted in mixed-kind expressions and
    ! as a findloc VALUE.
    implicit none
    type t
        integer(1), allocatable :: a
        real(4), pointer :: p => null()
        logical(1), allocatable :: l
    end type
    type(t) :: s
    real(4), target :: tp
    integer(1), allocatable :: ia
    logical(1), allocatable :: la
    complex(8) :: z(4)
    real(8) :: x(4), r
    integer(4) :: i(4), k
    logical(4) :: l(4), l4

    z = [(1d0, 0d0), (2d0, 0d0), (3d0, 0d0), (2d0, 0d0)]
    x = real(z)
    i = int(x)
    l = [.false., .true., .false., .true.]
    allocate(s%a, s%l, ia, la)
    s%a = 2
    tp = 3.0
    s%p => tp
    s%l = .true.
    ia = 3
    la = .true.

    if (any(findloc(x, s%a) /= 2)) error stop 1
    if (any(findloc(z, s%p) /= 3)) error stop 2
    if (any(findloc(i, s%p) /= 3)) error stop 3
    if (any(findloc(l, s%l, back=.true.) /= 4)) error stop 4
    if (any(findloc(x, ia) /= 3)) error stop 5
    if (any(findloc(l, la) /= 2)) error stop 6

    r = 1d0 + s%a
    if (r /= 3d0) error stop 7
    r = 1d0 + s%p
    if (r /= 4d0) error stop 8
    k = 4 + ia
    if (k /= 7) error stop 9
    k = 4 + s%a
    if (k /= 6) error stop 10
    l4 = .false.
    if (.not. (l4 .or. la)) error stop 11
    if (.not. (l4 .or. s%l)) error stop 12
    print *, "ok"
end program
