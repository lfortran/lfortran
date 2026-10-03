module template_nested_contained_01_m
    implicit none
    template outer {T}
        deferred type :: T
        type :: box
            integer :: v
        end type
        template inner {f}
            deferred interface
                subroutine f(x)
                    integer, intent(inout) :: x
                end subroutine
            end interface
        contains
            function h() result(y)
                type(box) :: y
                y%v = 1
                call f(y%v)
            end function
            subroutine g(r)
                integer, intent(out) :: r
                type(box) :: q
                q = h()
                r = q%v
            end subroutine
        end template
    end template
end module

program template_nested_contained_01
    use template_nested_contained_01_m
    implicit none
    integer :: r
    call s(r)
    if (r /= 3) error stop
    print *, r
contains
    subroutine s(r)
        integer, intent(out) :: r
        instantiate outer {integer}, only: inner
        instantiate inner {add2}, only: g
        call g(r)
    end subroutine

    subroutine add2(x)
        integer, intent(inout) :: x
        x = x + 2
    end subroutine
end program
