module optional_02_m
contains
    function f1(x) result(r)
        integer, optional, intent(in) :: x
        integer, allocatable :: r
        r = 0
    end function f1
end module

program optional_02
    use optional_02_m
    integer, allocatable :: i
    i = 10
    print *, f1(i)
end program
