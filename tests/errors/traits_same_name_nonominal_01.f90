module traits_same_name_nonominal_01_m
    implicit none

    abstract interface :: IFirst
        subroutine touch(out)
            integer, intent(out) :: out
        end subroutine touch
    end interface IFirst

    abstract interface :: ISecond
        subroutine touch(out)
            integer, intent(out) :: out
        end subroutine touch
    end interface ISecond

    type :: Box
        integer :: value
    contains
        procedure, pass :: touch => box_touch
    end type Box

    implements IFirst :: Box
    end implements Box

contains

    subroutine box_touch(self, out)
        class(Box), intent(in) :: self
        integer, intent(out) :: out
        out = self%value
    end subroutine box_touch

    subroutine use_second{ISecond :: T}(x, out)
        type(T), intent(in) :: x
        integer, intent(out) :: out
        call x%touch(out)
    end subroutine use_second
end module traits_same_name_nonominal_01_m

program traits_same_name_nonominal_01
    use traits_same_name_nonominal_01_m
    implicit none
    type(Box) :: value
    integer :: result
    value = Box(3)
    call use_second(value, result)
end program traits_same_name_nonominal_01
