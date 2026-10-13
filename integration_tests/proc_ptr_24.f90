module proc_ptr_24_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_24_m
    implicit none
    abstract interface
        function callback(a) result(r)
            use proc_ptr_24_sizes, only: n
            real, intent(in) :: a(n)
            real :: r(n)
        end function
    end interface
contains
    function twice(a) result(r)
        use proc_ptr_24_sizes, only: n
        real, intent(in) :: a(n)
        real :: r(n)
        r = 2.0 * a
    end function
end module

program proc_ptr_24
    use proc_ptr_24_m
    implicit none
    procedure(callback), pointer :: p
    real :: a(2), b(2)
    a = 1.5
    p => twice
    b = p(a)
    if (any(b /= 3.0)) error stop
    print *, b
end program
