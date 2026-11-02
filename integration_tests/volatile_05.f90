module volatile_05_mod
    ! VOLATILE statement on module variables, after and before the type
    ! declaration
    implicit none
    integer :: m1 = 3
    volatile :: m1
    volatile :: m2
    integer :: m2 = 4
end module volatile_05_mod

program volatile_05
    ! VOLATILE statement (F2018 8.6.17) on dummies, locals and module
    ! variables, after and before the type declaration. It must give the
    ! entity the VOLATILE attribute, as `integer, volatile ::` does.
    use volatile_05_mod, only: m1, m2
    implicit none
    integer :: a, b
    integer :: l1 = 5
    volatile :: l1
    volatile :: l2
    integer :: l2 = 6

    a = 1
    b = 2
    call s(a, b)
    if (a /= 11) error stop
    if (b /= 22) error stop

    l1 = l1 + 1
    l2 = l2 + 1
    if (l1 /= 6) error stop
    if (l2 /= 7) error stop

    m1 = m1 + 1
    m2 = m2 + 1
    if (m1 /= 4) error stop
    if (m2 /= 5) error stop
    print *, a, b, l1, l2, m1, m2

contains

    subroutine s(x, y)
        implicit none
        integer :: x
        volatile :: x
        volatile :: y
        integer :: y
        integer :: t
        volatile :: t
        t = 10
        x = x + t
        y = y + 2*t
    end subroutine s

end program volatile_05
