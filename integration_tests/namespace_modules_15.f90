! Interface bodies do not access their host by host association. A
! namespace is made accessible in an interface body either by importing
! it from the host with IMPORT, or by a namespace import inside the
! interface body.
module namespace_modules_15_types
    implicit none
    type :: box_t
        integer :: v = 0
    end type
end module

subroutine fill_box(b, v)
    use, namespace :: t => namespace_modules_15_types
    implicit none
    type(t%box_t), intent(out) :: b
    integer, intent(in) :: v
    b%v = v
end subroutine

integer function read_box(b)
    use, namespace :: t => namespace_modules_15_types
    implicit none
    type(t%box_t), intent(in) :: b
    read_box = b%v
end function

program namespace_modules_15
    use, namespace :: t => namespace_modules_15_types
    implicit none
    interface
        subroutine fill_box(b, v)
            import :: t
            type(t%box_t), intent(out) :: b
            integer, intent(in) :: v
        end subroutine

        integer function read_box(b)
            use, namespace :: tt => namespace_modules_15_types
            type(tt%box_t), intent(in) :: b
        end function
    end interface
    type(t%box_t) :: b

    call fill_box(b, 17)
    if (b%v /= 17) error stop
    if (read_box(b) /= 17) error stop
    print *, b%v
end program
