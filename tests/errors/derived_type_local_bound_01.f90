program local_bound3
    implicit none
    call s()
contains
    subroutine s()
        integer :: k
        type :: t
            integer :: b(k*2)
        end type
        type(t) :: x
        k = 1
        print *, size(x%b)
    end subroutine
end program
