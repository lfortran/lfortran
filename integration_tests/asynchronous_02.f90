module asynchronous_02_mod
    ! ASYNCHRONOUS on module variables: the attribute, and the statement
    ! after and before the type declaration
    implicit none
    real, asynchronous :: m1(3)
    real :: m2(3)
    asynchronous :: m2
    asynchronous :: m3
    real :: m3(3)
    real :: m4(3)
end module asynchronous_02_mod

program asynchronous_02
    ! ASYNCHRONOUS attribute and statement (F2018 8.5.4, 8.6.3) on dummies,
    ! locals and module variables, after and before the type declaration.
    ! Each of these variables must have the ASYNCHRONOUS attribute in ASR;
    ! m4 gets it from the statement in this scope (use association).
    use asynchronous_02_mod, only: m1, m2, m3, m4
    implicit none
    real, asynchronous :: l1(3)
    real :: l2(3)
    asynchronous :: l2
    asynchronous :: l3
    real :: l3(3)
    asynchronous :: m4

    l1 = 1
    l2 = 2
    l3 = 3
    m1 = 4
    m2 = 5
    m3 = 6
    m4 = 7
    call s(l1, l2, l3)
    if (any(l1 /= [11, 1, 1])) error stop
    if (any(l2 /= [2, 12, 2])) error stop
    if (any(l3 /= [3, 3, 13])) error stop
    call s(m1, m2, m3)
    if (any(m1 /= [14, 4, 4])) error stop
    if (any(m2 /= [5, 15, 5])) error stop
    if (any(m3 /= [6, 6, 16])) error stop
    m4(2) = m4(2) + m1(1)
    if (any(m4 /= [7, 21, 7])) error stop
    print *, l1, l2, l3
    print *, m1, m2, m3, m4

contains

    subroutine s(a, b, c)
        implicit none
        real, asynchronous, intent(inout) :: a(:)
        real, intent(inout) :: b(:)
        asynchronous :: b
        asynchronous :: c
        real, intent(inout) :: c(:)
        real :: t
        asynchronous :: t
        t = 10
        a(1) = a(1) + t
        b(2) = b(2) + t
        c(size(c)) = c(size(c)) + t
    end subroutine s

end program asynchronous_02
