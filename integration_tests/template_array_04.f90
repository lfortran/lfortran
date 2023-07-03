module reverse_m
    implicit none
    private
    public :: reverse_tmpl

    requirement default_behavior(t)
        type, deferred :: t
    end requirement

    template reverse_tmpl(t)
        requires default_behavior(t)
        private
        public :: reverse
    contains
        subroutine swap(x, y)
            type(t), intent(inout) :: x, y
            type(t) :: tmp
            tmp = x
            x = y
            y = tmp
        end subroutine

        subroutine reverse(arr)
            type(t), intent(inout) :: arr(:)
            integer :: i, j
            do i = 1, size(arr)/2
                j = size(arr)+1-i
                call swap(arr(i), arr(j))
            end do
        end subroutine
    end template

contains

    subroutine test_reverse()
        instantiate reverse_tmpl(integer), &
            only: ireverse => reverse
        integer :: a(5)
        a = [1,2,3,4,5]
        call ireverse(a)
        print *, a
    end subroutine

end module

program main
use reverse_m
call test_reverse()
end program
