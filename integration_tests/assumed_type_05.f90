module assumed_type_05_mod
    implicit none

    type :: object_t
        integer :: i = 0
        logical :: l = .false.
        character(len=8) :: c = ""
        complex :: z = (0.0, 0.0)
    end type

contains

    subroutine sub_assumed_type_assumed_rank(a)
        type(*), intent(inout), target :: a(..)
    end subroutine

end module

program assumed_type_05
    use assumed_type_05_mod
    implicit none
    type(object_t) :: object
    integer :: scalar_int

    object = object_t(7, .true., "hello", (1.0, 2.0))
    call sub_assumed_type_assumed_rank(object)

    scalar_int = 42
    call sub_assumed_type_assumed_rank(scalar_int)

    print *, "ok"
end program
