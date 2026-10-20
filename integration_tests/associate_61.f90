module associate_61_mod
    implicit none
    type :: t_int
        integer :: i = 0
    end type
    ! Parentheses around a structure constructor do not change its value
    type(t_int), parameter :: mp(2) = [(t_int(3)), t_int(4)]
end module

program associate_61
    use associate_61_mod, only: t_int, mp
    ! A parenthesized selector `(x)` is an expression, not a variable
    ! (F2018 11.1.3.3): the associate name holds the value the selector had
    ! when the ASSOCIATE statement executed, it does not alias `x`.
    implicit none
    type :: t_type
        real :: arr(4)
    end type
    real :: a(6), m(3, 3)
    character(len=6) :: str
    type(t_type) :: t
    integer :: i
    ! Parentheses around an initialization expression do not matter
    type(t_type), parameter :: tp = (t_type([1., 2., 3., 4.]))
    type(t_type) :: tl = (t_type([5., 6., 7., 8.]))
    type(t_type) :: tarr(2) = [(t_type(0.)), (t_type(1.))]
    type(t_type), parameter :: parr(2) = [(t_type(0.)), t_type(1.)]
    type(t_int), parameter :: pa(2) = [(t_int(3)), (t_int(4))]
    type(t_int), parameter :: pb(2) = [t_int(5), (t_int(6))]
    type(t_int), parameter :: pq(2) = [t_int :: (t_int(8)), t_int(9)]

    ! Array section
    a = [(real(i), i = 1, 6)]
    associate (ae => (a(3:6)))
        a(3) = 100.
        if (size(ae) /= 4) error stop 1
        if (any(ae /= [3., 4., 5., 6.])) error stop 2
    end associate

    ! Associate names of overlapping sections, two of them parenthesized
    a = [(real(i), i = 1, 6)]
    associate (an => a(2:5), ae => (a(3:6)), aw => (a(1:4)))
        an = ae - aw
    end associate
    if (any(a /= [1., 2., 2., 2., 2., 6.])) error stop 3

    ! The variable selector `a(3:6)` still aliases `a`
    a = [(real(i), i = 1, 6)]
    associate (av => a(3:6))
        a(3) = 100.
        if (av(1) /= 100.) error stop 4
    end associate

    ! Array element
    a = [(real(i), i = 1, 6)]
    associate (s => (a(2)))
        a(2) = -1.
        if (s /= 2.) error stop 5
    end associate

    ! Column of a 2-D array
    m = reshape([(real(i), i = 1, 9)], [3, 3])
    associate (c => (m(:, 2)))
        m(:, 2) = 0.
        if (any(c /= [4., 5., 6.])) error stop 6
    end associate

    ! Component section of a derived type
    t%arr = [1., 2., 3., 4.]
    associate (ca => (t%arr(2:3)))
        t%arr = 0.
        if (any(ca /= [2., 3.])) error stop 7
    end associate

    ! Substring
    str = "abcdef"
    associate (sub => (str(2:4)))
        str = "xxxxxx"
        if (len(sub) /= 3) error stop 8
        if (sub /= "bcd") error stop 9
    end associate

    ! Whole array
    a = [(real(i), i = 1, 6)]
    associate (w => (a))
        a = 0.
        if (any(w /= [(real(i), i = 1, 6)])) error stop 10
    end associate

    if (any(tp%arr /= [1., 2., 3., 4.])) error stop 11
    if (any(tl%arr /= [5., 6., 7., 8.])) error stop 12
    if (any(tarr(2)%arr /= 1.)) error stop 13
    if (any(parr(1)%arr /= 0.) .or. any(parr(2)%arr /= 1.)) error stop 14
    if (pa(1)%i /= 3 .or. pa(2)%i /= 4) error stop 15
    if (pb(1)%i /= 5 .or. pb(2)%i /= 6) error stop 16
    if (pq(1)%i /= 8 .or. pq(2)%i /= 9) error stop 17
    if (mp(1)%i /= 3 .or. mp(2)%i /= 4) error stop 18

    print *, "ok"
end program
