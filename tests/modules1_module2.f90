module y
    use abc, only: t1
    implicit none
    type :: t2
        class(t1), allocatable :: val
    end type t2
contains
    subroutine sub(self)
        class(t2), intent(inout), target :: self
        ! TODO: Add one test for SubroutineCall as well
        if(self%val%f()) print *, 1            !  <---- Error
    end subroutine sub
end module y
