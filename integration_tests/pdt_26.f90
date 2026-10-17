module pdt_26_mod
    ! Deferred LEN type parameters: typed allocation, sourced allocation
    ! and reallocation of allocatable objects.
    implicit none

    type :: string_t(n)
        integer, len :: n
        character(len=n) :: text
    end type

    type :: vector_t(n)
        integer, len :: n
        real :: x(n)
    end type

end module

program pdt_26
    use pdt_26_mod
    implicit none
    type(vector_t(:)), allocatable :: v, w
    type(string_t(:)), allocatable :: s, t
    character(len=:), allocatable :: msg
    integer :: i

    allocate(vector_t(n=4) :: v)
    if (v%n /= 4 .or. size(v%x) /= 4) error stop
    do i = 1, v%n
        v%x(i) = 100.0 + real(i)
    end do
    if (abs(sum(v%x) - 410.0) > 1e-4) error stop

    allocate(w, source=v)
    if (w%n /= 4 .or. abs(sum(w%x) - 410.0) > 1e-4) error stop
    w%x(1) = 0.0
    if (abs(v%x(1) - 101.0) > 1e-6) error stop

    deallocate(v)
    allocate(vector_t(2) :: v)
    v%x = [1.0, 2.0]
    if (v%n /= 2 .or. abs(sum(v%x) - 3.0) > 1e-6) error stop

    msg = "parameterized derived types"
    allocate(string_t(n=len(msg)) :: s)
    s%text = msg
    if (s%n /= 27 .or. s%text /= msg) error stop
    allocate(t, source=s)
    if (t%n /= 27 .or. t%text /= msg) error stop
    print *, v%n, w%n, s%n, t%text
end program
