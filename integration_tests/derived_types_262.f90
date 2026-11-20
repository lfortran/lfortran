program derived_types_262
    ! A character component of a SEQUENCE type is stored inline. Reading it
    ! as the right-hand side of an assignment, or as a SELECT CASE selector,
    ! must use its bytes rather than treat them as a string descriptor.
    implicit none

    type :: named_t
        sequence
        character(len=8) :: name
        integer :: size
    end type named_t

    type(named_t) :: g, h
    character(len=8) :: nm
    character(len=3) :: short
    character(len=12) :: long
    character(len=:), allocatable :: deferred
    integer :: r

    g%name = 'z'
    g%size = 3
    if (.not. (g%name == 'z')) error stop

    nm = g%name
    if (nm /= 'z') error stop
    if (len_trim(nm) /= 1) error stop

    g%name = 'abcdefgh'
    nm = g%name
    if (nm /= 'abcdefgh') error stop
    short = g%name
    if (short /= 'abc') error stop
    long = g%name
    if (long /= 'abcdefgh    ') error stop
    deferred = g%name
    if (len(deferred) /= 8) error stop
    if (deferred /= 'abcdefgh') error stop

    h%name = g%name
    h%size = g%size
    if (h%name /= 'abcdefgh') error stop
    if (h%size /= 3) error stop

    g%name = 'z'
    r = -1
    select case (g%name)
    case ('x')
        r = 1
    case ('z')
        r = 3
    case default
        r = 0
    end select
    if (r /= 3) error stop

    if (classify(g) /= 3) error stop
    g%name = 'other'
    if (classify(g) /= 0) error stop
    print *, 'ok'

contains

    integer function classify(d) result(res)
        type(named_t), intent(in) :: d
        select case (d%name)
        case ('x')
            res = 1
        case ('z')
            res = 3
        case default
            res = 0
        end select
    end function classify

end program derived_types_262
