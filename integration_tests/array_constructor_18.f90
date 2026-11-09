module array_constructor_18_mod
    implicit none
    type :: item
        integer :: v = 0
    end type
contains
    function f(n) result(r)
        integer, intent(in) :: n
        integer :: r
        r = n * 2
    end function
    function tot(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r
        r = sum(a)
    end function
    function mk(n) result(r)
        integer, intent(in) :: n
        type(item) :: r
        r%v = n
    end function
    function tot_item(a) result(r)
        type(item), intent(in) :: a(:)
        type(item) :: r
        r%v = sum(a%v)
    end function
    function tests_int() result(t)
        integer :: t(1)
        integer :: ii
        t = [tot([(f(ii), ii = 1, 5)])]
    end function
    function tests_item() result(t)
        type(item) :: t(1)
        integer :: ii
        t = [tot_item([(mk(ii), ii = 1, 5)])]
    end function
end module

program array_constructor_18
    use array_constructor_18_mod
    implicit none
    integer :: a(1)
    type(item) :: b(1)
    a = tests_int()
    if (a(1) /= 30) error stop 1
    b = tests_item()
    if (b(1)%v /= 15) error stop 2
    print *, a(1), b(1)%v
end program
