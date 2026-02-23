! Test reshape of array passed directly to function inside select rank
module select_rank_16_mod
    implicit none

    type :: my_type
        integer :: val
    end type

contains

    pure function helper(arr) result(r)
        type(my_type), intent(in) :: arr(:)
        type(my_type) :: r
        r = arr(1)
    end function

    function foo(x) result(res)
        type(my_type), intent(in) :: x(..)
        type(my_type) :: res
        select rank(x)
            rank(2)
                res = foo_helper(reshape(x, shape=[size(x)]))
        end select
    contains
        pure function foo_helper(arr) result(r)
            type(my_type), intent(in) :: arr(:)
            type(my_type) :: r
            r = arr(1)
        end function
    end function

    ! Same test with integers
    function foo_int(x) result(res)
        integer, intent(in) :: x(..)
        integer :: res
        select rank(x)
            rank(2)
                res = foo_int_helper(reshape(x, shape=[size(x)]))
        end select
    contains
        pure function foo_int_helper(arr) result(r)
            integer, intent(in) :: arr(:)
            integer :: r
            r = arr(1)
        end function
    end function

end module

program select_rank_16
    use select_rank_16_mod
    implicit none

    type(my_type) :: a(2,2), b
    integer :: c(2,2), d

    a(1,1)%val = 10
    a(2,1)%val = 20
    a(1,2)%val = 30
    a(2,2)%val = 40
    b = foo(a)
    if (b%val /= 10) error stop

    c(1,1) = 100
    c(2,1) = 200
    c(1,2) = 300
    c(2,2) = 400
    d = foo_int(c)
    if (d /= 100) error stop
end program
