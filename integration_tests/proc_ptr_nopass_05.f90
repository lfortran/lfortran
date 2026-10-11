module proc_ptr_nopass_05_sizes
    integer :: n = 2
end module

module proc_ptr_nopass_05_m
    type t
        procedure(callback), nopass, pointer :: f => null()
    end type
    abstract interface
        real function callback(x, a)
            use proc_ptr_nopass_05_sizes, only: n
            import t
            class(t) :: x
            real :: a(n)
        end function
    end interface
contains
    real function total(x, a)
        use proc_ptr_nopass_05_sizes, only: n
        class(t) :: x
        real :: a(n)
        total = sum(a)
    end function
end module

program proc_ptr_nopass_05
    use proc_ptr_nopass_05_m
    type(t) :: x
    real :: a(2), r
    a = 1.5
    x%f => total
    r = x%f(x, a)
    if (abs(r - 3.0) > 1e-6) error stop
    print *, r
end program
