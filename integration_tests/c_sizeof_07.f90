program c_sizeof_07
    implicit none
    call c_sizeof_07_check(3)
contains
    subroutine c_sizeof_07_check(n)
        use iso_c_binding, only: c_sizeof
        integer, intent(in) :: n
        integer :: a(n, 2)
        integer, allocatable :: b(:)
        allocate(b(n))
        print *, c_sizeof(a), sizeof(b)
        if (c_sizeof(a) /= 24) error stop
        if (sizeof(b) /= 12) error stop
    end subroutine
end program
