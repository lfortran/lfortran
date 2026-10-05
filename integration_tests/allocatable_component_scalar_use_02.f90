program allocatable_component_scalar_use_02
    implicit none
    type :: t
        integer, allocatable :: a
        real, allocatable :: r
        integer, pointer :: p => null()
    end type
    type(t) :: s
    integer, target :: it
    integer, allocatable :: ia
    character(len=8) :: buf

    allocate(s%a, s%r, ia)
    s%a = 5
    s%r = 2.5
    it = 3
    s%p => it
    ia = 4

    ! Negating allocatable and pointer scalars
    s%a = -s%a
    if (s%a /= -5) error stop 1
    if (-s%r /= -2.5) error stop 2
    if (-s%p /= -3) error stop 3
    write(buf, '(i4)') -ia
    if (trim(buf) /= "  -4") error stop 4

    print *, "ok"
end program
