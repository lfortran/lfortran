module string_117_module
    implicit none

    type :: TestType
        integer, allocatable :: buf_size
    end type TestType

contains

    subroutine do_work(me)
        class(TestType), intent(in) :: me
        character(len=me%buf_size) :: buf
        
        if (len(buf) /= 8) error stop
        buf = '12345678'
        if (buf /= '12345678') error stop
        print *, len(buf)
    end subroutine do_work

end module string_117_module

program string_117
    use string_117_module
    implicit none
    type(TestType) :: obj
    
    allocate(obj%buf_size)
    obj%buf_size = 8
    
    call do_work(obj)
end program string_117
