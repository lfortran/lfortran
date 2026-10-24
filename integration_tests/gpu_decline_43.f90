! gpu decline reason: LiveoutDerivedType
! An allocatable scalar component of a derived-type variable that one
! iteration defines: whether it is allocated after the loop depends on the
! iterations, and no thread can keep a private copy of it.
module gpu_decline_43_m
    implicit none
    type :: t
        integer, allocatable :: k
        real :: x
    end type
end module

program gpu_decline_43
    use gpu_decline_43_m
    implicit none
    type(t) :: y
    integer :: i, a(100)
    allocate(y%k)
    y%k = 0
    y%x = 0
    do concurrent (i = 1:100)
        a(i) = i
        if (i == 3) y%k = 7
    end do
    print *, y%k, a(100)
    if (y%k /= 7) error stop 1
    if (a(100) /= 100) error stop 2
end program
