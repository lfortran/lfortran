module nested_15_mod
    type :: child
        character(:), allocatable :: value
        logical :: par = .true.
    end type child
contains 

    subroutine temp_sub(model)
        type(child), intent(inout) :: model
        call nested_sub()
    contains
        subroutine nested_sub()
            if (.not. model%par) error stop
            if (model%value /= "Hello") error stop
        end subroutine
    end subroutine
end module

program nested_15
    use nested_15_mod
    type(child) :: model
    model%value = "Hello"
    call temp_sub(model)
end program nested_15