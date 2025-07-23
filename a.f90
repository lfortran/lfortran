program p
    integer, allocatable :: arr(:)
    
    allocate(arr(3))
    
    arr = [1,2,3]
    arr = ff(arr)

    contains 

    function ff(s) result(ret) 
       integer, allocatable :: ret(:)
       integer, allocatable :: s(:)
       print *, s
       allocate(ret(3))

       ret = s
       print *, ret
    end function 
end program