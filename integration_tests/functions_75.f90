! Character function results whose length is a reduction over an
! assumed-shape dummy (sum, maxval), referenced with actual arguments that
! are not stored like the dummy (#17233)
module functions_75_mod
    implicit none
contains
    function stars(k) result(s)
        integer, intent(in) :: k(:)
        character(len=sum(k)) :: s
        s = repeat("*", sum(k))
    end function
end module

program functions_75
    use functions_75_mod
    implicit none
    integer :: counts(3), grid(2, 2)
    integer, allocatable :: acounts(:)
    character(len=5) :: words(3)
    character(len=7) :: r

    counts = [2, 1, 4]
    grid = reshape([2, 1, 3, 1], [2, 2])
    allocate(acounts(3))
    acounts = counts
    words = ["ab   ", "c    ", "defg "]

    ! module function, fixed-size actual
    if (len(stars(counts)) /= 7) error stop 1
    if (stars(counts) /= "*******") error stop 2
    r = stars(counts)
    if (r /= "*******") error stop 3

    ! internal function; fixed-size, allocatable, section, expression, array
    ! constructor and function result actuals
    if (len(plus(counts)) /= 7) error stop 4
    if (plus(counts) /= "+++++++") error stop 5
    if (len(plus(acounts)) /= 7) error stop 6
    if (plus(acounts) /= "+++++++") error stop 7
    if (len(plus(counts(2:3))) /= 5) error stop 8
    if (plus(acounts(2:3)) /= "+++++") error stop 9
    if (len(plus(counts + 1)) /= 10) error stop 10
    if (plus([1, 2]) /= "+++") error stop 11
    if (len(plus(twice(counts))) /= 14) error stop 12
    if (plus(twice([1, 1, 1])) /= "++++++") error stop 13

    ! other length expressions
    if (len(longest(counts)) /= 4) error stop 14
    if (longest(acounts) /= "LLLL") error stop 15
    if (len(padded(counts)) /= 8) error stop 16
    if (padded(counts) /= "pppppppp") error stop 17
    if (len(grid_total(grid)) /= 7) error stop 18
    if (grid_total(grid) /= "ggggggg") error stop 19

    ! character array dummy
    if (len(join(words)) /= 7) error stop 20
    if (join(words) /= "abcdefg") error stop 21
    if (len(widest(words)) /= 4) error stop 22
    if (widest(words) /= "defg") error stop 23

    print *, "OK"
contains
    function twice(k) result(r)
        integer, intent(in) :: k(3)
        integer :: r(3)
        r = 2 * k
    end function

    function plus(k) result(s)
        integer, intent(in) :: k(:)
        character(len=sum(k)) :: s
        s = repeat("+", sum(k))
    end function

    function longest(k) result(s)
        integer, intent(in) :: k(:)
        character(len=maxval(k)) :: s
        s = repeat("L", maxval(k))
    end function

    function padded(k) result(s)
        integer, intent(in) :: k(:)
        character(len=sum(k) + 1) :: s
        s = repeat("p", sum(k) + 1)
    end function

    function grid_total(k) result(s)
        integer, intent(in) :: k(:, :)
        character(len=sum(k)) :: s
        s = repeat("g", sum(k))
    end function

    function join(w) result(s)
        character(len=*), intent(in) :: w(:)
        character(len=sum(len_trim(w))) :: s
        integer :: i, p
        p = 1
        do i = 1, size(w)
            s(p:p + len_trim(w(i)) - 1) = trim(w(i))
            p = p + len_trim(w(i))
        end do
    end function

    function widest(w) result(s)
        character(len=*), intent(in) :: w(:)
        character(len=maxval(len_trim(w))) :: s
        integer :: i
        s = ""
        do i = 1, size(w)
            if (len_trim(w(i)) == len(s)) s = w(i)
        end do
    end function
end program
