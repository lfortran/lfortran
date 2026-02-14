module nested_24_mod
implicit none
contains

    recursive integer function A(k, x1, x2, x3, x4, x5) result(res)
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
            res = B()
        end if

    contains

        recursive integer function B() result(res)
            m = m - 1
            res = A(m, B, x1, x2, x3, x4)
        end function B

    end function A

    recursive integer function one() result(res)
        res = 1
    end function one

    recursive integer function minus_one() result(res)
        res = -1
    end function minus_one

    recursive integer function zero() result(res)
        res = 0
    end function zero

end module nested_24_mod

program nested_24
    use nested_24_mod, only: A, one, minus_one, zero
    implicit none
    integer :: r

    r = A(10, one, minus_one, minus_one, one, zero)
    if (r /= -67) error stop
end program nested_24
