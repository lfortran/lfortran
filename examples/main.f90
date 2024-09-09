program main
    use iso_c_binding, only: c_int
    implicit none

    interface
        function add(a, b) bind(C)
            import :: c_int
            integer(c_int) :: add
            integer(c_int), value :: a, b
        end function add
    end interface

    integer(c_int) :: a, b, result

    a = 5
    b = 3
    result = add(a, b)
    print *, 'The sum is: ', result
end program main

