! The template syntax is an LFortran extension.
module template_copy_order_01_m
    implicit none
    template counts {T}
        deferred type :: T
    contains
        function count(n, a, marker) result(r)
            integer, intent(in) :: n, a(n)
            type(T), intent(in) :: marker
            integer :: r
            integer, parameter :: zseed = 10, aseed = zseed
            r = aseed + sum(a)
        end function
    end template
contains
    subroutine check()
        instantiate counts {integer}, only: count_integer => count
        instantiate counts {real}, only: count_real => count
        if (count_integer(3, [1, 2, 3], 0) /= 16) error stop 1
        if (count_real(1, [7], 0.0) /= 17) error stop 2
    end subroutine
end module

program template_copy_order_01
    use template_copy_order_01_m
    implicit none
    call check()
end program
