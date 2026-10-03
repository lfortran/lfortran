program submodule_50a
    use submodule_50_mod
    implicit none

    type(wrapper_type) :: w
    integer :: i

    allocate(w%containers(3))
    do i = 1, 3
        call w%containers(i)%setup([10, 20])
    end do

    print *, "PASS"
end program
