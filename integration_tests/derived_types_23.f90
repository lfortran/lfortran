module module_01
    implicit none
    type t_1
        integer :: i
    contains
        procedure :: sub_02
    end type t_1

    type t_2
        type(t_1) :: yt
    contains
        procedure :: sub_01
    end type t_2

contains
    subroutine sub_02(this)
        class(t_1), intent(in) :: this
        print *, this%i
    end subroutine sub_02

    subroutine sub_01(this)
        class(t_2), intent(in) :: this
        call this%yt%sub_02()
    end subroutine sub_01
end module module_01

program derived_types_01
    use module_01
    implicit none
    type(t_2) :: xt
    ! Fix LLVM error
    xt%yt%i = 1
    call xt%sub_01()
end program derived_types_01

