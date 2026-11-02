! Knuth's man or boy test (#9979): `b` is internal to the recursive `a` and
! captures `m` and the dummy procedures `x1`..`x4`, and every instance of
! `a` with k > 0 passes its own `b` on. For k = 10, 384 instances hold a
! procedure value of `b` at the same time, so LFortran compiles it with
! --internal-procedure-thunks=512.
module host_assoc_22_m
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

program host_assoc_22
use host_assoc_22_m
implicit none
integer, parameter :: expected(0:10) = [1, 0, -2, 0, 1, 0, 1, -1, -10, -30, -67]
integer :: k, r
do k = 0, 10
    r = a(k, one, minus_one, minus_one, one, zero)
    if (r /= expected(k)) error stop 1
end do
print *, r
end program
