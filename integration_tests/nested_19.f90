module nested_19_mod
    implicit none

    type t
        integer :: i = 1
        integer :: arr(2) = [10, 20]
    end type t

    type(t), parameter :: t_param = t()
end module nested_19_mod

program nested_19
    use nested_19_mod
    implicit none

    ! Check scalar component
    if (t_param%i /= 1) error stop

    ! Check array component
    if (any(t_param%arr /= [10, 20])) error stop
end program nested_19
