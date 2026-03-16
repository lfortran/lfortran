module assumed_type_01_mod
    implicit none
contains

    subroutine sub_assumed_type(a)
        type(*), intent(in) :: a
    end subroutine

    subroutine sub_assumed_type_array(a)
        type(*), intent(inout), target :: a(:)
    end subroutine

    subroutine sub_assumed_type_assumed_rank(a)
        type(*), intent(inout), target :: a(..)
    end subroutine

end module

program assumed_type_01
    use assumed_type_01_mod
    implicit none
    integer :: x
    x = 42
    call sub_assumed_type(x)
    print *, "ok"
end program
