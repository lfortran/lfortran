program allocate_72
    ! STAT= can be any assignable integer expression: a plain variable,
    ! a struct member, a nested struct member, or an array element.
    implicit none
    type :: t1
        integer :: err
    end type
    type :: t2
        type(t1) :: e
    end type
    type(t1) :: a
    type(t2) :: b
    integer  :: errs(3)
    integer  :: scalar
    real, allocatable :: arr1(:), arr2(:), arr3(:), arr4(:)

    allocate(arr1(10), stat=scalar)
    if (scalar /= 0) error stop 1

    allocate(arr2(10), stat=a%err)
    if (a%err /= 0) error stop 2

    allocate(arr3(10), stat=b%e%err)
    if (b%e%err /= 0) error stop 3

    errs = -1
    allocate(arr4(10), stat=errs(2))
    if (errs(2) /= 0) error stop 4
end program
