module pdt_12
    implicit none

    type recursive_t(k)
        integer, kind :: k = kind(1.0)
        type(recursive_t(k)), allocatable :: next
    end type recursive_t

    interface
        module subroutine touch(self)
            import :: recursive_t
            type(recursive_t), intent(inout) :: self
        end subroutine touch
    end interface

end module pdt_12


submodule (pdt_12) pdt_12_s
contains

    module procedure touch
        if (.not. allocated(self%next)) then
            allocate(self%next)
        end if
    end procedure touch

end submodule pdt_12_s


program test_pdt
    use pdt_12
    implicit none

    type(recursive_t) :: obj

    call touch(obj)

    if (.not. allocated(obj%next)) error stop 

end program test_pdt