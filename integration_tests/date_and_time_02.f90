program date_and_time_02
! Assumed-size VALUES of date_and_time and GET/PUT of random_seed
implicit none
integer :: v(10), g(10), s(10)

v = -7
call values_of(v)
if (v(1) < 2000 .or. v(2) < 1 .or. v(2) > 12) error stop 1
if (any(v(9:10) /= -7)) error stop 2

g = -7
call get_seed(g)
if (any(g(9:10) /= -7)) error stop 3
call put_seed(g)
s = -7
call get_seed(s)
if (any(s(1:8) /= g(1:8))) error stop 4
if (any(s(9:10) /= -7)) error stop 5

print *, "ok"

contains

    subroutine values_of(a)
    integer :: a(*)
    call date_and_time(values=a)
    end subroutine

    subroutine get_seed(a)
    integer :: a(*)
    call random_seed(get=a)
    end subroutine

    subroutine put_seed(a)
    integer :: a(*)
    call random_seed(put=a)
    end subroutine

end program
