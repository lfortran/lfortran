program gpu_metal_718
    ! The allocatable components of an array of a derived type reach the
    ! kernel laid end to end in one buffer, whose elements a kernel without
    ! --descriptor-index-64 (and every Metal kernel) addresses in 32 bits.
    ! Here each component fits in a default integer, but together they hold
    ! 2*1100000000 > huge(0) elements, so the launch must stop with an error
    ! instead of packing them into a buffer it cannot address. The check
    ! stops it before anything is copied, so the components are never
    ! touched beyond their first and last elements.
    implicit none
    type :: t
        logical(1), allocatable :: v(:)
    end type
    type(t) :: x(2)
    integer :: i, m
    m = 1100000000
    do i = 1, 2
        allocate(x(i)%v(m))
        x(i)%v(1) = .false.
        x(i)%v(m) = .false.
    end do
    do concurrent (i = 1:2)
        x(i)%v(m) = .true.
    end do
    print *, x(1)%v(m), x(2)%v(m), x(1)%v(1), x(2)%v(1)
end program
