program scoped_attribute_01
    use scoped_attribute_01_wrapper
    implicit none
    renamed = 1
    v = 2
    call check()
contains
    subroutine check()
        use scoped_attribute_01_wrapper, local => renamed
        if (any(local /= 1)) error stop
        if (any(v /= 2)) error stop
    end subroutine
end program
