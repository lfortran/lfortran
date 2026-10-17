program character_compare_kind4_01
    implicit none
    character(len=4, kind=4) :: s, t
    character(kind=4) :: c1, c2

    s = 4_"ab"
    if (.not. (s == 4_"ab")) error stop 1
    if (.not. (4_"ab" == s)) error stop 2
    if (s /= 4_"ab") error stop 3
    if (s == 4_"abc") error stop 4
    if (.not. (s < 4_"abc")) error stop 5
    if (.not. (s > 4_"aa")) error stop 6
    if (.not. (s >= 4_"ab")) error stop 7
    if (.not. (s <= 4_"ab")) error stop 8

    t = 4_"ab"
    if (.not. (s == t)) error stop 9

    c1 = 4_"x"
    c2 = 4_"x"
    if (.not. (c1 == c2)) error stop 10
    if (c1 /= c2) error stop 11
    if (.not. (c1 < 4_"y")) error stop 12

    print *, s == 4_"ab", len(s)
end program
