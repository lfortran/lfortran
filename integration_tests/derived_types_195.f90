program derived_types_195
    implicit none

    ! `inner_t` holds 12 bytes of data but occupies 16 bytes: the real(8)
    ! gives the type 8-byte alignment, so 4 bytes of padding trail `i`.
    type :: inner_t
        real(8) :: r
        integer :: i
    end type

    ! `j` therefore starts 16 bytes into `outer_t`, not 12.
    type :: outer_t
        type(inner_t) :: n
        integer :: j
    end type

    ! `gap_t` is padded between its components instead of after them.
    type :: gap_t
        integer :: a
        real(8) :: b
        integer :: c
    end type

    ! An extended type stores its parent as its first component, so the
    ! padding of the parent sits in front of `k`.
    type, extends(inner_t) :: ext_t
        integer :: k
    end type

    ! A character component behind a padding gap: the runtime read the
    ! descriptor from the wrong offset and dereferenced it, which crashed.
    type :: char_t
        integer :: a
        real(8) :: b
        character(len=3) :: s
        integer :: c
    end type

    ! Several small integers, each of which needs its own alignment gap.
    type :: small_t
        integer(1) :: a
        integer(2) :: b
        integer(4) :: c
        integer(8) :: d
        integer(1) :: e
    end type

    ! A zero-size component is NOT described: the runtime does not advance
    ! past one, but it does leave its element type current, so the advance
    ! past `c` is the size of that element and not of `a`. This type is a
    ! regression guard rather than a fix: it is already correct before this
    ! change, and an earlier version of it that modelled a zero-size array
    ! as zero bytes read `c` from the padding instead.
    type :: pad_t
        integer :: a
        real(8) :: pad(0)
        integer :: c
    end type

    type(outer_t) :: o(2)
    type(gap_t) :: g(1)
    type(ext_t) :: e(1)
    type(char_t) :: c2(2)
    type(small_t) :: m(1)
    type(pad_t) :: z(1)
    character(len=400) :: line
    character(len=3) :: s1, s2
    real(8) :: r1, r2, b1, cb1, cb2
    integer :: i1, i2, j1, j2, a1, c1, k1
    integer :: ca1, cc1, ca2, cc2, ma, mb, mc, me, za, zc
    integer(8) :: md

    o(1) = outer_t(inner_t(3.5d0, 22), 32)
    o(2) = outer_t(inner_t(4.5d0, 23), 33)
    write(line, *) o
    read(line, *) r1, i1, j1, r2, i2, j2
    if (r1 /= 3.5d0) error stop
    if (i1 /= 22) error stop
    if (j1 /= 32) error stop
    if (r2 /= 4.5d0) error stop
    if (i2 /= 23) error stop
    if (j2 /= 33) error stop

    g(1) = gap_t(7, 2.5d0, 9)
    write(line, *) g
    read(line, *) a1, b1, c1
    if (a1 /= 7) error stop
    if (b1 /= 2.5d0) error stop
    if (c1 /= 9) error stop

    e(1)%r = 1.5d0
    e(1)%i = 2
    e(1)%k = 3
    write(line, *) e
    read(line, *) r1, i1, k1
    if (r1 /= 1.5d0) error stop
    if (i1 /= 2) error stop
    if (k1 /= 3) error stop

    c2(1) = char_t(1, 2.5d0, "abc", 3)
    c2(2) = char_t(4, 5.5d0, "def", 6)
    write(line, *) c2
    read(line, *) ca1, cb1, s1, cc1, ca2, cb2, s2, cc2
    if (ca1 /= 1) error stop
    if (cb1 /= 2.5d0) error stop
    if (s1 /= "abc") error stop
    if (cc1 /= 3) error stop
    if (ca2 /= 4) error stop
    if (cb2 /= 5.5d0) error stop
    if (s2 /= "def") error stop
    if (cc2 /= 6) error stop

    m(1) = small_t(1_1, 2_2, 3, 4_8, 5_1)
    write(line, *) m
    read(line, *) ma, mb, mc, md, me
    if (ma /= 1) error stop
    if (mb /= 2) error stop
    if (mc /= 3) error stop
    if (md /= 4_8) error stop
    if (me /= 5) error stop

    z(1)%a = 11
    z(1)%c = 22
    write(line, *) z
    read(line, *) za, zc
    if (za /= 11) error stop
    if (zc /= 22) error stop

    ! The RUN() macro does not compare stdout, so these assert nothing; they
    ! are here only so the scalar whole-object path is exercised at all, which
    ! is the spelling the issue reports and which crashed for `char_t`.
    print *, o(1)
    print *, g(1)
    print *, e(1)
    print *, c2(1)

end program
