program present_01

    real, pointer :: tmp(:)
    tmp => null()
    call foo1(tmp)

contains

    subroutine foo1(a)
        real, intent(in), optional :: a(:)  ! Optional argument
        if (present(a)) error stop
    end subroutine

end program
