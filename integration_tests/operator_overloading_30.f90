module operator_overloading_30_m
    implicit none

    interface operator(/)
        module procedure join_path
    end interface
contains
    function join_path(a, b) result(c)
        character(*), intent(in) :: a, b
        character(:), allocatable :: c
        c = a // '/' // b
    end function
end module

program operator_overloading_30
    use operator_overloading_30_m, only: operator(/)
    implicit none
    character(:), allocatable :: p

    p = 'home'/'user'
    if (p /= 'home/user') error stop
    p = p/'documents'
    if (p /= 'home/user/documents') error stop
    print *, p
end program
