module block_36_mod
    implicit none
contains
    subroutine check_in_module_procedure()
        block
            integer :: j, v(3) = [(j, j=1,3)]
            if (any(v /= [1, 2, 3])) error stop
        end block
    end subroutine
end module

program block_36
    ! Implied-do array constructors in the initializer of BLOCK-local
    ! variables are compile-time folded (#16159).
    use block_36_mod, only: check_in_module_procedure
    implicit none
    integer :: k
    block
        integer :: j, v(3) = [(j, j=1,3)]
        print *, v
        if (any(v /= [1, 2, 3])) error stop
    end block
    block
        integer :: j
        real :: r(3) = [(real(j), j=1,3)]
        print *, r
        if (any(abs(r - [1.0, 2.0, 3.0]) > 1e-6)) error stop
    end block
    block
        integer :: j, m(3) = [1, (j, j=2,3)]
        print *, m
        if (any(m /= [1, 2, 3])) error stop
    end block
    block
        integer :: j
        integer, save :: s(4) = [(2*j, j=1,4)]
        print *, s
        if (any(s /= [2, 4, 6, 8])) error stop
    end block
    block
        integer :: h(3) = [(k*k, k=1,3)]
        print *, h
        if (any(h /= [1, 4, 9])) error stop
    end block
    call check_in_module_procedure()
end program
