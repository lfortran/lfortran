module string_124_mod
    implicit none
contains
    function f() result(z)
        character(len=5), allocatable :: z(:)
        call s(z)
    end function f

    function g() result(z)
        character(len=3), allocatable :: z(:,:)
        call s2(z)
    end function g

    function h(x) result(z)
        character(len=*), intent(in) :: x
        character(len=4), allocatable :: z(:)
        call s(z)
        z(1) = x
    end function h

    subroutine s(y)
        character(len=*), allocatable, intent(out) :: y(:)
        allocate(y(2))
        y = ["alpha", "gamma"]
    end subroutine s

    subroutine s2(y)
        character(len=*), allocatable, intent(inout) :: y(:,:)
        allocate(y(2,2))
        y = "pq"
        y(2,1) = "rst"
    end subroutine s2
end module string_124_mod

program string_124
    use string_124_mod, only: f, g, h
    implicit none
    character(len=:), allocatable :: r(:), q(:,:)
    integer :: i

    r = f()
    print *, size(r), len(r), r
    if (size(r) /= 2) error stop 1
    if (len(r) /= 5) error stop 2
    if (r(1) /= "alpha" .or. r(2) /= "gamma") error stop 3

    deallocate(r)
    allocate(character(len=9) :: r(5))
    do i = 1, 2
        r = f()
        if (size(r) /= 2 .or. len(r) /= 5) error stop 4
        if (r(2) /= "gamma") error stop 5
    end do

    q = g()
    print *, shape(q), len(q), q
    if (any(shape(q) /= [2, 2]) .or. len(q) /= 3) error stop 6
    if (q(1,1) /= "pq" .or. q(2,1) /= "rst") error stop 7

    r = h("zz")
    print *, size(r), len(r), r
    if (size(r) /= 2 .or. len(r) /= 4) error stop 8
    if (r(1) /= "zz" .or. r(2) /= "gamm") error stop 9

    r = h(r(1))
    print *, size(r), len(r), r
    if (size(r) /= 2 .or. len(r) /= 4) error stop 10
    if (r(1) /= "zz" .or. r(2) /= "gamm") error stop 11
end program string_124
