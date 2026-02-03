program arrays_constructor_01
    implicit none
    character(5) :: str = "Hello"
    integer :: i = 1, ios, j
    real, pointer :: dw_(:,:)
       
    type :: MyClass
        integer :: value
    end type MyClass
    type(MyClass) :: v1, v2, v3, arr(3)
    character(4), parameter :: arr1(1:2,1:2)=reshape(['a ', '1 ', 'b ', '2 '], [2,2])
    integer, parameter :: lpunc = 4

    character:: input(lpunc) = &
        [("2",i=1,lpunc)]
    
    print *, ["aaa", "aaa"]
    print *, [str(i+1:i+1), str(i:i)]
    print *, ["aaa", str(i+1:i+3), "aaa"]
    print *, [str(i+1:i+3), "aaa"]
    arr = [MyClass :: v1, v2, v3]
    print *, arr

    
    print*, arr1
    if (any(arr1 /= reshape(['a ', '1 ', 'b ', '2 '], [2,2]))) error stop
    
    print *, input
    if (any(input /= ['2', '2', '2', '2'])) error stop

    allocate(dw_(2,3))
    
    dw_ = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2,3])

    print *, [dw_]
    if (all([dw_] /= [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])) error stop "Test failed"
    
    deallocate(dw_)
end program arrays_constructor_01
