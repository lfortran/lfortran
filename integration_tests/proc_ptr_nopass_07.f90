module proc_ptr_nopass_07_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_nopass_07_m
    implicit none
    abstract interface
        function callback(a) result(r)
            use proc_ptr_nopass_07_sizes, only: n
            real, intent(in) :: a(n)
            real :: r(n)
        end function
    end interface
    type :: t
        procedure(callback), pointer, nopass :: f => null()
    end type
contains
    function twice(a) result(r)
        use proc_ptr_nopass_07_sizes, only: n
        real, intent(in) :: a(n)
        real :: r(n)
        r = 2.0 * a
    end function
end module

program proc_ptr_nopass_07
    use proc_ptr_nopass_07_m
    implicit none
    type(t) :: x
    real :: a(2), b(2)
    a = 1.5
    x%f => twice
    b = x%f(a)
    if (any(b /= 3.0)) error stop
    print *, b
end program
