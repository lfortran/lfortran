program pdt_23
    ! LEN type parameters of scalar objects: the parameter sizes character
    ! and array components and is inquired as a component.
    implicit none

    type :: label(size)
        integer, len :: size
        character(len=size) :: text
    end type

    type :: vec(n)
        integer, len :: n = 2
        real :: x(n)
        integer :: tag
    end type

    type(label(5)) :: entry
    type(label(3)) :: short
    type(vec(4)) :: v
    type(vec) :: d
    integer :: i

    entry%text = "hello"
    if (entry%text /= "hello") error stop
    if (entry%size /= 5) error stop
    if (len(entry%text) /= 5) error stop
    entry%text = "hi"
    if (len(entry%text) /= 5) error stop
    if (entry%text /= "hi   ") error stop
    short%text = "abcdef"
    if (short%size /= 3 .or. len(short%text) /= 3) error stop
    if (short%text /= "abc") error stop

    do i = 1, v%n
        v%x(i) = real(i)
    end do
    v%tag = 7
    if (v%n /= 4 .or. size(v%x) /= 4) error stop
    if (abs(sum(v%x) - 10.0) > 1e-6) error stop
    if (v%tag /= 7) error stop
    if (d%n /= 2 .or. size(d%x) /= 2) error stop

    call local_objects(6)
    print *, entry%text, short%text, v%n, d%n
contains

    subroutine local_objects(m)
        integer, intent(in) :: m
        type(vec(m)) :: w
        type(label(m - 1)) :: t
        w%x = 1.5
        if (w%n /= 6 .or. size(w%x) /= 6) error stop
        if (abs(sum(w%x) - 9.0) > 1e-6) error stop
        t%text = "lfortran"
        if (t%size /= 5 .or. t%text /= "lfort") error stop
    end subroutine

end program
