module mod_test_allocatable_01
    implicit none
    integer, allocatable, dimension(:) :: f
 end module mod_test_allocatable_01


 program mm
    use mod_test_allocatable_01
    implicit none
    logical :: ret
    allocate(f(4))
    f = [1, 2, 3, 4]
    print *,f
    ret = all( f == [1,2,3,4]) 
    if(ret .eqv. .false.) error stop

 end program mm
