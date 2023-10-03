module interface_14_data
    implicit none
    public data_t

    type data_t
        integer :: x
    end type
end module

program interface_14
    use interface_14_data, only: data_t
    implicit none

    integer :: x
    x = get_x_from_data(return_x_from_data)
    print *, x
    if (x /= 100) error stop

    contains

    function return_x_from_data(d)
        type(data_t), intent(inout) :: d
        integer :: return_x_from_data
        return_x_from_data = d%x
    end function

    function get_x_from_data(R)
    interface
        function R(d)
        use interface_14_data
        implicit none
        type(data_t), intent(inout) :: d
        integer :: R
        end function
    end interface
    integer :: get_x_from_data
    type(data_t) :: d
    d%x = 100
    get_x_from_data = R(d)
    end function

end program
