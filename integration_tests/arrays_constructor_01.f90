program arrays_constructor_01
    implicit none
    character(5) :: str = "Hello"
    integer :: i = 1
    type :: MyClass
        integer :: value
    end type MyClass
    type(MyClass) :: v1, v2, v3, arr(3)
    character(4), parameter :: arr1(1:2,1:2)=reshape(['a ', '1 ', 'b ', '2 '], [2,2])
    
    print *, ["aaa", "aaa"]
    print *, [str(i+1:i+1), str(i:i)]
    print *, ["aaa", str(i+1:i+3), "aaa"]
    print *, [str(i+1:i+3), "aaa"]
    arr = [MyClass :: v1, v2, v3]
    print *, arr

    
    print*, arr1
    if (any(arr1 /= reshape(['a ', '1 ', 'b ', '2 '], [2,2]))) error stop
end program arrays_constructor_01
