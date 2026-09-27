! Derived-type component bounds inside a template that are constant
! expressions of a deferred integer constant (n+1, n*2, -n:n, n/2+1, n**2,
! max(n, 5), max(n*2, 5), min(n+1, 10), abs(-n)*2). They are folded to fixed sizes at instantiation.

module template_struct_deferred_dim_02_m
    implicit none

    template tm {n}
        deferred integer, parameter :: n

        type :: w
            integer :: b1(n+1)
            integer :: b2(-n:n)
            integer :: b3(n/2+1)
            integer :: b4(n**2)
            integer :: b5(n*2)
        end type

        type :: wmax
            integer :: b(max(n, 5))
        end type

        type :: wintr
            integer :: c1(max(n*2, 5))
            integer :: c2(min(n+1, 10))
            integer :: c3(abs(-n)*2)
        end type
    end template
end module

program template_struct_deferred_dim_02
    use template_struct_deferred_dim_02_m
    implicit none
    integer, parameter :: three = 3
    instantiate tm {three}, only: w3 => w, wmax3 => wmax, &
        wintr3 => wintr
    type(w3) :: s, s2
    type(wmax3) :: t, t2, t3
    type(wintr3) :: u
    integer :: i

    s%b1 = 1
    s%b2 = 2
    s%b3 = 3
    s%b4 = 4
    s%b5 = 5
    print *, size(s%b1), size(s%b2), lbound(s%b2), size(s%b3), size(s%b4), &
        size(s%b5)
    if (size(s%b1) /= 4) error stop
    if (size(s%b2) /= 7) error stop
    if (lbound(s%b2, 1) /= -3 .or. ubound(s%b2, 1) /= 3) error stop
    if (size(s%b3) /= 2) error stop
    if (size(s%b4) /= 9) error stop
    if (size(s%b5) /= 6) error stop
    s2 = s
    if (any(s2%b1 /= 1) .or. any(s2%b2 /= 2) .or. any(s2%b3 /= 3)) error stop
    if (any(s2%b4 /= 4) .or. any(s2%b5 /= 5)) error stop

    do i = 1, 5
        t%b(i) = i
    end do
    t2%b = t%b
    t3 = t
    print *, size(t2%b), t2%b, t3%b
    if (size(t2%b) /= 5 .or. size(t3%b) /= 5) error stop
    do i = 1, 5
        if (t2%b(i) /= i) error stop
        if (t3%b(i) /= i) error stop
    end do

    print *, size(u%c1), size(u%c2), size(u%c3)
    if (size(u%c1) /= 6) error stop
    if (size(u%c2) /= 4) error stop
    if (size(u%c3) /= 6) error stop
end program
