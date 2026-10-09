module traits_type_adoption_04_consumer
    use traits_type_adoption_04_provider, only: Parent
    implicit none
contains
    integer function via_parent(self) result(n)
        class(Parent), intent(in) :: self
        n = self%value()
    end function

    pure integer function via_named(self, with_offset) result(n)
        class(Parent), intent(in) :: self
        logical, intent(in) :: with_offset
        if (with_offset) then
            n = self%named(offset=3, scale=2)
        else
            n = self%named(scale=2)
        end if
    end function

    subroutine via_add(self)
        class(Parent), intent(inout) :: self
        call self%add(step=5)
    end subroutine
end module
