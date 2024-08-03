program intrinsics_131
    implicit none
    integer, parameter :: x1 = max(1, 2)
    integer(8), parameter :: x2 = max(1_8, 2_8)
    real, parameter :: x3 = max(1.0, 2.0)
    real(8), parameter :: x4 = max(1.0_8, 2.0_8)
    character(len=10), parameter :: x5 = max("str", "string")
    integer, parameter :: ar1(3) = max([1, 12, 3], [4, 5, 6])
    integer(8), parameter :: ar2(3) = max([1_8, 12_8, 3_8], [4_8, 5_8, 6_8])
    real, parameter :: ar3(3) = max([1.0, 12.0, 3.0], [4.0, 5.0, 6.0])
    real(8), parameter :: ar4(3) = max([1.0_8, 12.0_8, 3.0_8], [4.0_8, 5.0_8, 6.0_8])
    ! character(len=10), parameter :: ar5(3) = max(["str1", "str2", "char"], ["!str#$", "xtring", "charac"]) ! Does not work - #4582

    integer :: i1, i2
    integer(8) :: i3, i4
    real :: r1, r2
    real(8) :: r3, r4
    character(len=10) :: string_var, string_var2, string_var3
    i1 = 123
    i2 = 456 
    i3 = 123_8 
    i4 = 456_8
    r1 = 123.0 
    r2 = 456.0 
    r3 = 123.0_8 
    r4 = 456.0_8
    string_var = "str"
    string_var2 = "string"
    string_var3 = "character"

    print *, x1
    if (x1 /= 2) error stop
    print *, x2
    if (x2 /= 2_8) error stop
    print *, x3
    if (x3 /= 2.0) error stop
    print *, x4
    if (x4 /= 2.0_8) error stop
    print *, x5
    if (x5 /= "string") error stop

    print *, ar1
    if (any(ar1 /= [4, 12, 6])) error stop
    print *, ar2
    if (any(ar2 /= [4_8, 12_8, 6_8])) error stop
    print *, ar3
    if (any(ar3 /= [4.0, 12.0, 6.0])) error stop
    print *, ar4
    if (any(ar4 /= [4.0_8, 12.0_8, 6.0_8])) error stop
    ! print *, ar5
    ! if (any(ar5 /= ["str1", "string", "charac"])) error stop

    print *, max(i1, i2)
    if (max(i1, i2) /= i2) error stop
    print *, max(i3, i4)
    if (max(i3, i4) /= i4) error stop
    print *, max(r1, r2)
    if (max(r1, r2) /= r2) error stop
    print *, max(r3, r4)
    if (max(r3, r4) /= r4) error stop
    

    print*, max("str", "character")
    if (max("str", "character") /= "str") error stop

    print*, max(string_var, string_var2)
    if (max(string_var, string_var2) /= string_var2) error stop

    print*, max(string_var3, string_var2)
    if (max(string_var3, string_var2) /= string_var2) error stop
    
end program
