module nested_13_mod
    implicit none
contains
    subroutine get_prototype(prototype)
        character(len=:), allocatable, intent(out) :: prototype
        call expand_name()
        contains
        subroutine expand_name()
            prototype = "Hello"
        end subroutine expand_name
    end subroutine get_prototype
end module nested_13_mod

program nested_13
    use nested_13_mod
    implicit none
    character(len=:), allocatable :: proto
    call get_prototype(proto)
    if (proto /= "Hello") error stop
end program nested_13