module arrays_14_size_mod
    contains 

    function foo(xx) result(iord)
        integer, intent(in) :: xx(:)
        integer             :: iord(size(xx))
        iord = boo(1.0*xx)
        if(any(iord /= [1,2,3])) error stop
        iord = boo(xx*1.0)
    end function
   
    function boo(xx) result(iord)
        real, intent(in) :: xx(:)
        integer :: iord(size(xx))
        iord(1) = 1
        iord(2) = 2
        iord(3) = 3
    end function
end module 


program arrays_14_size

    use arrays_14_size_mod
    implicit none
    integer :: x(3)
    x = foo(x)
    if(any(x /= [1,2,3])) error stop

end program 