module module_arrays_95_mm
    contains 
    function foo(ii) result(res)
        integer :: ii
        integer :: res(10)
        res = -8
        res(ii) = 9
    end function
end module

program arrays_95
    use module_arrays_95_mm
    integer :: inn
    integer :: res(10)
    inn = 1
    print *, foo(inn)
    res = foo(inn)
    if (res(inn) /= 9) error stop "Test failed"
    if (size(foo(inn)) /= 10) error stop "Test failed"
    if (all(res(inn:) == -8)) error stop "Test failed"
end program 
