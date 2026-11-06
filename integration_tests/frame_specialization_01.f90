! Knuth's man or boy test (#9979, #16665) with --fast. Every instance of
! the recursive `a` with k > 0 passes its own internal procedure `b`, which
! captures `m` and the dummy procedures `x1`..`x4`, on to `a`. Without
! --fast, each such instance holds a thunk of `b` (384 at once for k = 10);
! with --fast, `a` gets a copy whose dummy procedures are passed with the
! frame of their host, so no thunks are used and any k works at the default
! --internal-procedure-thunks.
module frame_specialization_01_m
implicit none
contains
    recursive integer function a(k, x1, x2, x3, x4, x5) result(res)
        integer, intent(in) :: k
        interface
            recursive integer function x1()
            end function
            recursive integer function x2()
            end function
            recursive integer function x3()
            end function
            recursive integer function x4()
            end function
            recursive integer function x5()
            end function
        end interface
        integer :: m
        if (k <= 0) then
            res = x4() + x5()
        else
            m = k
            res = b()
        end if
    contains
        recursive integer function b() result(res)
            m = m - 1
            res = a(m, b, x1, x2, x3, x4)
        end function
    end function

    recursive integer function one() result(res)
        res = 1
    end function

    recursive integer function minus_one() result(res)
        res = -1
    end function

    recursive integer function zero() result(res)
        res = 0
    end function
end module

program frame_specialization_01
use frame_specialization_01_m
implicit none
integer, parameter :: expected(0:10) = [1, 0, -2, 0, 1, 0, 1, -1, -10, -30, -67]
integer :: k, r
do k = 0, 10
    r = a(k, one, minus_one, minus_one, one, zero)
    if (r /= expected(k)) error stop 1
end do
r = a(15, one, minus_one, minus_one, one, zero)
if (r /= -3250) error stop 2
print *, r
end program
