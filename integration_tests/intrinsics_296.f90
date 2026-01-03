program intrinsic_296
    implicit none
    integer(1), parameter :: x1 = storage_size(3_1, 1)
    integer(2), parameter :: x2 = storage_size(11_2, 2)
    integer(4), parameter :: x3 = storage_size(17_4, 4)
    integer(8), parameter :: x4 = storage_size(19_8, 8)
    integer, parameter :: x5 = storage_size(3.0)
    integer(8), parameter :: x6 = storage_size(3.0_8, 8)
    integer, parameter :: x7 = storage_size((3.0, 13.0))
    integer(8), parameter :: x8 = storage_size((3.0_8, 13.0_8), 8)
    integer, parameter :: x9 = storage_size(.true.)
    integer(1), parameter :: x10 = storage_size("hello")
    integer(2), parameter :: x11 = storage_size("hello", 8)
    
    integer(4), parameter :: ar1 = storage_size([0, 112, 17], 4)
    integer(8), parameter :: ar2 = storage_size([1, 11, 17], 8)
    integer, parameter :: ar3 = storage_size([1.0, 11.0, 17.0])
    integer(8), parameter :: ar4 = storage_size([1.0_8, 11.0_8, 17.0_8], 8)
    integer, parameter :: ar5 = storage_size([.true., .false., .true.])
    integer(1), parameter :: ar6 = storage_size(["hello", "world", "hello"])
    integer(2), parameter :: ar7 = storage_size(["hello", "world", "hello"], 2)
    integer, parameter :: ar8 = storage_size([(3.0, 13.0), (3.0, 13.0), (3.0, 13.0)])
    integer(8), parameter :: ar9 = storage_size([(3.0_8, 13.0_8), (3.0_8, 13.0_8), (3.0_8, 13.0_8)], 8)

    integer(1) :: i1 = 3
    integer(2) :: i2 = 3
    integer(4) :: i3 = 3
    integer(8) :: i4 = 3
    real :: r1 = 313.0
    real(8) :: r2 = 313.0_8
    complex :: c1 = (3.0, 13.0)
    complex(8) :: c2 = (3.0_8, 13.0_8)
    logical :: l1 = .true.
    character(1) :: s1 = 'hello'

    integer :: arr1(3) = [0, 112, 17]
    integer(8) :: arr2(3) = [1_8, 11_8, 17_8]
    real :: arr3(3) = [1.0, 11.0, 17.0]
    real(8) :: arr4(3) = [1.0_8, 11.0_8, 17.0_8]
    logical :: arr5(3) = [.true., .false., .true.]
    complex :: arr6(3) = [(3.0, 13.0), (3.0, 13.0), (3.0, 13.0)]
    complex(8) :: arr7(3) = [(3.0_8, 13.0_8), (3.0_8, 13.0_8), (3.0_8, 13.0_8)]
    character(5) :: arr8(3) = ["hello", "world", "hello"]

    print *, x1
    if (x1 /= 8) error stop
    print *, x2
    if (x2 /= 16) error stop
    print *, x3
    if (x3 /= 32) error stop
    print *, x4
    if (x4 /= 64) error stop
    print *, x5
    if (x5 /= 32) error stop
    print *, x6
    if (x6 /= 64) error stop
    print *, x7
    if (x7 /= 64) error stop
    print *, x8
    if (x8 /= 128) error stop
    print *, x9
    if (x9 /= 32) error stop
    print *, x10
    if (x10 /= 40) error stop
    print *, x11
    if (x11 /= 40) error stop

    print *, ar1
    if (ar1 /= 32) error stop
    print *, ar2
    if (ar2 /= 32) error stop
    print *, ar3
    if (ar3 /= 32) error stop
    print *, ar4
    if (ar4 /= 64) error stop
    print *, ar5
    if (ar5 /= 32) error stop
    print *, ar6
    if (ar6 /= 40) error stop
    print *, ar7
    if (ar7 /= 40) error stop
    print *, ar8
    if (ar8 /= 64) error stop
    print *, ar9 
    if (ar9 /= 128) error stop

    print *, storage_size(i1)
    if (storage_size(i1) /= 8) error stop
    print *, storage_size(i2)
    if (storage_size(i2) /= 16) error stop
    print *, storage_size(i3)
    if (storage_size(i3) /= 32) error stop
    print *, storage_size(i4)
    if (storage_size(i4) /= 64) error stop
    print *, storage_size(r1)
    if (storage_size(r1) /= 32) error stop
    print *, storage_size(r2)
    if (storage_size(r2) /= 64) error stop
    print *, storage_size(c1)
    if (storage_size(c1) /= 64) error stop
    print *, storage_size(c2)
    if (storage_size(c2) /= 128) error stop
    print *, storage_size(l1)
    if (storage_size(l1) /= 32) error stop
    print *, storage_size(s1)
    if (storage_size(s1) /= 8) error stop

    print *, storage_size(arr1)
    if (storage_size(arr1) /= 32) error stop
    print *, storage_size(arr2)
    if (storage_size(arr2) /= 64) error stop
    print *, storage_size(arr3)
    if (storage_size(arr3) /= 32) error stop
    print *, storage_size(arr4)
    if (storage_size(arr4) /= 64) error stop
    print *, storage_size(arr5)
    if (storage_size(arr5) /= 32) error stop
    print *, storage_size(arr6)
    if (storage_size(arr6) /= 64) error stop
    print *, storage_size(arr7)
    if (storage_size(arr7) /= 128) error stop
    print *, storage_size(arr8)
    if (storage_size(arr8) /= 40) error stop

    print*, kind(storage_size(arr8, 8))
    if (kind(storage_size(arr8, 8)) /= 8) error stop
end program