! gpu decline reason: StructAllocatableArrayMember
! a derived-type kernel argument has an allocatable array component whose
! element type the device layout cannot decompose into a buffer of its own.
program gpu_decline_05
    implicit none
    type :: t
        real :: x
        complex, allocatable :: z(:)
    end type
    type(t) :: s(4)
    integer :: i
    do i = 1, 4
        s(i)%x = 0.0
        allocate(s(i)%z(2))
        s(i)%z = (1.0, 0.0)
    end do
    do concurrent (i = 1:4)
        s(i)%x = real(i)
    end do
    do i = 1, 4
        if (abs(s(i)%x - real(i)) > 1.0e-6) error stop "bad x"
    end do
end program
