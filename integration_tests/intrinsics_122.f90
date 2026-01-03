program intrinsic_122
    implicit none
    integer :: a(2,3), b(3,2), c(2), d
    real, allocatable :: e(:,:)
    integer, parameter :: x1 = rank(1)
    integer, parameter :: x2 = rank(4.0)
    integer, parameter :: x3 = rank(.false.)
    integer, parameter :: x4 = rank((1.0, 2.9))
    integer, parameter :: x5 = rank("abc")
    integer, parameter :: ar1 = rank([445, 32, 12])
    integer, parameter :: ar2 = rank([9.08, 0.1, 0.2])
    integer, parameter :: ar3 = rank([.true., .false.])
    integer, parameter :: ar4 = rank([(1.0, 2.9), (3.0, 4.0)])
    integer, parameter :: ar5 = rank(["abc", "def"])
    integer :: i = 789
    real :: r = 120.34
    character(len=3) :: s = "abc"
    logical :: l = .true.
    complex :: ci = (1.0, 2.0)
    integer :: arr1(4) = [11, 14, 66, 32]
    real :: arr2(4) = [1.0, 2.0, 3.0, 4.0]
    complex :: arr3(4) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]
    logical :: arr4(4) = [.true., .false., .true., .false.]
    character(len=3) :: arr5(4) = ["abc", "def", "ghi", "jkl"]

    print *, x1
    if (x1 /= 0) error stop
    print *, x2
    if (x2 /= 0) error stop
    print *, x3
    if (x3 /= 0) error stop
    print *, x4
    if (x4 /= 0) error stop
    print *, x5
    if (x5 /= 0) error stop
    print *, ar1
    if (ar1 /= 1) error stop
    print *, ar2
    if (ar2 /= 1) error stop
    print *, ar3
    if (ar3 /= 1) error stop
    print *, ar4
    if (ar4 /= 1) error stop
    print *, ar5
    if (ar5 /= 1) error stop

    print *, rank(i)
    if (rank(i) /= 0) error stop
    print *, rank(r)
    if (rank(r) /= 0) error stop
    print *, rank(s)
    if (rank(s) /= 0) error stop
    print *, rank(l)
    if (rank(l) /= 0) error stop
    print *, rank(ci)
    if (rank(ci) /= 0) error stop
    print *, rank(arr1)
    if (rank(arr1) /= 1) error stop
    print *, rank(arr2)
    if (rank(arr2) /= 1) error stop
    print *, rank(arr3)
    if (rank(arr3) /= 1) error stop
    print *, rank(arr4)
    if (rank(arr4) /= 1) error stop
    print *, rank(arr5)
    if (rank(arr5) /= 1) error stop

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    b = reshape([10, 20, 30, 40, 50, 60], [3, 2])
  
    print*, rank(1)
    if ( rank(1) /= 0 ) error stop
  
    print*, rank([.true., .false.])
    if ( rank([.true., .false.]) /= 1 ) error stop
  
    print*, rank(a)
    if ( rank(a) /= 2 ) error stop
  
    print*, rank(b)
    if ( rank(b) /= 2 ) error stop
  
    print*, rank(c)
    if ( rank(c) /= 1 ) error stop
  
    print*, rank(d)
    if ( rank(d) /= 0 ) error stop
  
    print*, rank(e)
    if ( rank(e) /= 2 ) error stop
  
  end