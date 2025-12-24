module nested_17_mod
    type t
        integer :: i = 1
        integer :: arr(2) = [10, 20]
    end type

    type(t), parameter :: t_param = t() 
    contains
    

end module 

program nested_17
    use nested_17_mod
    integer :: i
    i  = 1
    print *, t_param%arr(i)
end program