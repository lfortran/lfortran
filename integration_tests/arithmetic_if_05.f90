! The expression of an arithmetic IF is evaluated once, so a function it
! references is invoked once and the branch taken follows that single value.
module arithmetic_if_05_m
implicit none
integer :: ncalls = 0
contains
    integer function next(k)
        integer, intent(in) :: k
        ncalls = ncalls + 1
        next = k
    end function

    real function rnext(x)
        real, intent(in) :: x
        ncalls = ncalls + 1
        rnext = x
    end function

    integer function bump(i)
        integer, intent(inout) :: i
        i = i + 1
        bump = i
    end function

    subroutine branch(k, c)
        integer, intent(in) :: k
        integer, intent(out) :: c
        if (next(k)) 1, 2, 3
1       c = 1
        return
2       c = 2
        return
3       c = 3
    end subroutine
end module

program arithmetic_if_05
use arithmetic_if_05_m
implicit none
integer :: n = 0
integer :: c, i, k

! The code of the issue
if (f()) 10, 20, 30
10 continue
20 continue
30 continue
print *, n
if (n /= 1) error stop

! Each branch, with an integer and a real function
c = 0
if (next(-1)) 41, 42, 43
41 c = 1
go to 44
42 c = 2
go to 44
43 c = 3
44 continue
if (c /= 1) error stop
if (ncalls /= 1) error stop

c = 0
if (next(0)) 51, 52, 53
51 c = 1
go to 54
52 c = 2
go to 54
53 c = 3
54 continue
if (c /= 2) error stop
if (ncalls /= 2) error stop

c = 0
if (rnext(2.5)) 61, 62, 63
61 c = 1
go to 64
62 c = 2
go to 64
63 c = 3
64 continue
if (c /= 3) error stop
if (ncalls /= 3) error stop

! A function referenced in a subexpression
c = 0
if (next(3) - 3) 71, 72, 73
71 c = 1
go to 74
72 c = 2
go to 74
73 c = 3
74 continue
if (c /= 2) error stop
if (ncalls /= 4) error stop

! A function that changes its argument picks the branch of the one value
i = -1
c = 0
if (bump(i)) 81, 82, 83
81 c = 1
go to 84
82 c = 2
go to 84
83 c = 3
84 continue
if (c /= 2) error stop
if (i /= 0) error stop

! The action statement of a logical IF
c = 0
if (c == 0) if (next(-5)) 91, 92, 93
c = 4
go to 94
91 c = 1
go to 94
92 c = 2
go to 94
93 c = 3
94 continue
if (c /= 1) error stop
if (ncalls /= 5) error stop

! Branching back to the arithmetic IF evaluates it again, once
k = 0
101 if (bump(k) - 3) 101, 102, 103
102 c = 2
go to 104
103 c = 3
104 continue
if (c /= 2) error stop
if (k /= 3) error stop

! In a module procedure
call branch(7, c)
if (c /= 3) error stop
if (ncalls /= 6) error stop
call branch(-7, c)
if (c /= 1) error stop
if (ncalls /= 7) error stop
print *, ncalls

contains
integer function f()
n = n + 1
f = 1
end function
end program
