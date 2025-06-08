module derived_types_24_module_01
    implicit none
    type t_1
        integer :: i
    contains
        procedure :: sub_01, sub_03
    end type t_1

    type t_2
        type(t_1) :: yt
    contains
        procedure :: sub_02
    end type t_2

contains
    subroutine sub_01(this)
        class(t_1), intent(in) :: this
        print *, this%i
        if(this%i /= 123) error stop
    end subroutine sub_01

    subroutine sub_02(this)
        class(t_2), intent(in) :: this
        type(t_2) :: t
        call this%yt%sub_01()
        t%yt%i = 42
        call t%yt%sub_03()
    end subroutine sub_02

    subroutine sub_03(this)
        class(t_1), intent(in) :: this
        print *, this%i
        if(this%i /= 42) error stop
    end subroutine sub_03
end module derived_types_24_module_01

program derived_types_24
    use derived_types_24_module_01
    implicit none
    type(t_2) :: xt
    xt%yt%i = 123
    call xt%sub_02()
end program derived_types_24
