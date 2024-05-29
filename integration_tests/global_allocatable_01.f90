module a
    implicit none
    integer, allocatable, dimension(:) :: f
 end module a


 program mm
    use a
    implicit none
    allocate(f(4))
    f = [1, 2, 3, 4]
    print *,f
    logical :: ret
    ret = all( f == [1,2,3,4]) 
    if(ret == .false.) error stop

 end program mm
