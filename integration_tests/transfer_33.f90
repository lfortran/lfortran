program transfer_33
    implicit none
    type :: attr_t
        character(len=1) :: name(4)
    end type attr_t
    type(attr_t) :: a
    character(len=4) :: s
    a%name = ['a', 'b', 'c', 'd']
    s = transfer(a%name, s)
    if (s /= 'abcd') error stop
end program transfer_33
