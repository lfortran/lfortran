! Test lhs realloc of derived type
module derived_types_98_mod
  type :: tt
     character(len=:), allocatable :: name
    end type tt
end module


program derived_types_98
    use derived_types_98_mod
    type(tt), allocatable :: dl(:)
    integer :: i
    i = 1
    
    allocate(dl(2))
    print *, allocated(dl(1)%name)
    if(allocated(dl(1)%name)) error stop 
    print *, allocated(dl(2)%name)
    if(allocated(dl(2)%name)) error stop 
    
    print *, size(dl)
    if(size(dl) /= 2) error stop
    
    
    dl(1)%name = "first"
    dl(2)%name = "second"
    print *, allocated(dl(1)%name)
    if(.not. allocated(dl(1)%name)) error stop 
    print *, allocated(dl(2)%name)
    if(.not. allocated(dl(2)%name)) error stop 
    
    dl = dl(1:i)
    print *, allocated(dl(1)%name)
    if(.not. allocated(dl(1)%name)) error stop 
    print *, size(dl)
    if(size(dl) /= 1) error stop


end program