function iota(n) result(r)
    implicit none
    integer, intent(in) :: n
    integer :: r(n), i
    r = [(i, i = 1, n)]
end function iota

function filled(a) result(r)
    implicit none
    integer, intent(in) :: a
    integer, allocatable :: r(:)
    allocate(r(3))
    r = a
end function filled

function letters(n) result(r)
    implicit none
    integer, intent(in) :: n
    character(len=n) :: r
    r = "abcdefgh"(1:n)
end function letters

subroutine opt(a, b)
    implicit none
    integer, intent(out) :: a
    integer, intent(in), optional :: b
    if (present(b)) then
        a = b
    else
        a = -1
    end if
end subroutine opt

integer function total(a)
    implicit none
    integer, intent(in) :: a(:)
    total = sum(a)
end function total
