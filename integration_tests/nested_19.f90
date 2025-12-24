module nested_19_mod
    type t
        integer :: i = 1
        integer :: arr(2) = [10, 20]
    end type

    type(t), parameter :: t_param = t() 
    contains
    

end module 

program nested_19
    use nested_19_mod
    integer :: i
    i  = 1
    print *, t_param%arr(i)
end program