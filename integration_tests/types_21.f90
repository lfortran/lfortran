program types_21

    integer(8) :: res
    integer(8) :: x(5)
    res = fun(x)
    print*, res
    if ( res /= 5 ) error stop
    
    contains
    
        function fun (x) result(res)
        integer(8) :: res
        integer(8), intent(in) :: x(:)
        res = size(x, kind=8)
        end
    
end