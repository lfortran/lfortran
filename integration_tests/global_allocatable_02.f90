! Checking global scope of f allocatable array.
module a
    implicit none
    integer, allocatable, dimension(:,:) :: f
 end module a

subroutine r
    use a
    implicit none
    logical :: ret
    ret  = all (f(1,:) == [1,2,3,4])
    if (ret == .false.) error stop
    deallocate(f)
    allocate(f(6,6))
    f(2,:) = [10,10,10,10,10,10]
end subroutine r

 program mm
    use a
    implicit none
    allocate(f(4,4))
    f(1,:) = [1, 2, 3, 4]
    print *,f
    call r
    print *,f
    logical :: ret
    ret = all (f(2,:) == [10,10,10,10,10,10])
    if(ret == .false.) error stop

 end program mm