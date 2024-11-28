module functions_26_mod
    type tt
        contains
        procedure :: eee
    end type tt
    contains 
    function eee(self) result(arr)
        class(tt) :: self
        integer, allocatable :: arr(:)
        allocate(arr(3))
        arr = [1,2,3]
    end function
end module functions_26_mod

program functions_26
    use functions_26_mod
    type(tt) :: sstruct
    print *, sstruct%eee() 
end program functions_26