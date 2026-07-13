program p
    type :: t
        integer, allocatable :: arr(:)
    end type t
    
    type(t) :: varr
    allocate(varr%arr(3))
    
    varr%arr = [1,2,3]
    varr = ff(varr)

    contains 

    function ff(s) result(ret) 
       type(t),intent(in) :: s
       type(t) :: ret
       print *, s%arr
       allocate(ret%arr(3))

       ret%arr = s%arr
       print *, ret%arr
    end function 
end program