module merge_derived_01_m
    implicit none

    type :: pt
        integer :: x
        integer :: y
    end type

    type :: color
        integer :: r
        integer :: g
        integer :: b
    end type

contains

    function choose_pt(a, b, mask) result(res)
        type(pt), intent(in) :: a, b
        logical, intent(in) :: mask
        type(pt) :: res
        res = merge(a, b, mask)
    end function

    function choose_color(c1, c2, mask) result(res)
        type(color), intent(in) :: c1, c2
        logical, intent(in) :: mask
        type(color) :: res
        res = merge(c1, c2, mask)
    end function

end module

program merge_derived_01
    use merge_derived_01_m
    implicit none

    type(pt) :: p1, p2, p3, p4
    type(color) :: c1, c2, c3
    type(pt) :: parr(2)

    p1 = pt(10, 20)
    p2 = pt(30, 40)

    ! Test module functions with .true. and .false.
    p3 = choose_pt(p1, p2, .true.)
    if (p3%x /= 10 .or. p3%y /= 20) error stop 1

    p3 = choose_pt(p1, p2, .false.)
    if (p3%x /= 30 .or. p3%y /= 40) error stop 2

    ! Test direct inline merge with structure constructor
    p4 = merge(pt(1, 2), pt(3, 4), .true.)
    if (p4%x /= 1 .or. p4%y /= 2) error stop 3

    p4 = merge(pt(1, 2), pt(3, 4), .false.)
    if (p4%x /= 3 .or. p4%y /= 4) error stop 4

    ! Test array items
    parr(1) = pt(100, 200)
    parr(2) = pt(300, 400)
    p4 = merge(parr(1), parr(2), .true.)
    if (p4%x /= 100 .or. p4%y /= 200) error stop 5

    p4 = merge(parr(1), parr(2), .false.)
    if (p4%x /= 300 .or. p4%y /= 400) error stop 6

    ! Test different derived type in same scope
    c1 = color(255, 0, 0)
    c2 = color(0, 255, 0)
    c3 = choose_color(c1, c2, .false.)
    if (c3%r /= 0 .or. c3%g /= 255 .or. c3%b /= 0) error stop 7

    c3 = merge(color(1, 2, 3), color(4, 5, 6), .true.)
    if (c3%r /= 1 .or. c3%g /= 2 .or. c3%b /= 3) error stop 8

    print *, "ok"
end program
