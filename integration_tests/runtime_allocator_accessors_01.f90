program runtime_allocator_accessors_01
    implicit none
    call check_arrays(3)
    call check_arrays(7)
contains
    subroutine check_arrays(n)
        integer, intent(in) :: n
        real, allocatable :: a(:), b(:)
        integer :: i
        allocate(a(n), b(n))
        do i = 1, n
            a(i) = real(i)
        end do
        b = a + 2.0
        a = [b, b]
        if (size(a) /= 2*n) error stop 1
        do i = 1, n
            if (a(i) /= real(i + 2)) error stop 2
            if (a(n + i) /= b(i)) error stop 3
        end do
        deallocate(b)
        allocate(b(2*n))
        b = a
        if (sum(b) /= real(n*(n + 5))) error stop 4
        deallocate(a, b)
    end subroutine
end program
