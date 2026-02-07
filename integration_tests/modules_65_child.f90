module modules_65_child
    use modules_65_base, only: base_t
    implicit none

    type, extends(base_t) :: child_t
        integer :: extra = 42
    end type child_t

end module modules_65_child
