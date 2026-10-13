program class_171
! Intrinsic assignment of a derived type with class(t) / class(*)
! allocatable array components must give the copy a fully initialised
! array descriptor (including its offset). `b` is a local variable whose
! descriptor memory is reused from freed heap blocks filled by `poison`.
implicit none
type :: w
    integer :: c
end type
type :: o
    class(w), allocatable :: p(:)
end type
type :: u
    class(*), allocatable :: q(:)
end type
type(o) :: a
type(u) :: x
integer :: k
allocate(a%p(-1:1))
a%p%c = [10, 20, 30]
allocate(x%q(3:4), source=[7, 8])
do k = 1, 3
    call poison()
    call check_class(a)
    call poison()
    call check_upoly(x)
end do
print *, "ok"

contains

subroutine poison()
    integer(8), allocatable :: t(:)
    integer :: n
    do n = 1, 16
        allocate(t(n))
        t = 1000000
        deallocate(t)
    end do
end subroutine

subroutine check_class(s)
    type(o), intent(in) :: s
    type(o) :: b
    b = s
    if (lbound(b%p, 1) /= -1 .or. ubound(b%p, 1) /= 1) error stop 1
    if (b%p(-1)%c /= 10) error stop 2
    if (b%p(0)%c /= 20) error stop 3
    if (b%p(1)%c /= 30) error stop 4
    if (any(b%p%c /= [10, 20, 30])) error stop 5
end subroutine

subroutine check_upoly(s)
    type(u), intent(in) :: s
    type(u) :: b
    b = s
    if (lbound(b%q, 1) /= 3 .or. ubound(b%q, 1) /= 4) error stop 6
    select type (e => b%q)
    type is (integer)
        if (e(3) /= 7) error stop 7
        if (e(4) /= 8) error stop 8
    class default
        error stop 9
    end select
end subroutine

end program
