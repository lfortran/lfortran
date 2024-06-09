! Checking global scope of f allocatable array.
module mod_test_allocatable_02
    implicit none
    integer, allocatable, dimension(:,:) :: f
 end module mod_test_allocatable_02

subroutine r
    use mod_test_allocatable_02
    implicit none
    logical :: ret
    ret  = all (f(1,:) == [1,2,3,4])
    if (ret .eqv. .false.) error stop
    deallocate(f)
    allocate(f(6,6))
    f(2,:) = [10,10,10,10,10,10]
end subroutine r

 program mm
    use mod_test_allocatable_02
    implicit none
    logical :: ret
    allocate(f(4,4))
    f(1,:) = [1, 2, 3, 4]
    print *,f
    call r
    print *,f
    ret = all (f(2,:) == [10,10,10,10,10,10])
    if(ret .eqv. .false.) error stop

 end program mm