module derived_types_105_my_type
    implicit none

    type :: my_type
        integer :: x
    contains
        procedure, private :: output
        generic, public :: write(formatted) => output
    end type my_type

contains

    subroutine output(self, unit, iotype, v_list, iostat, iomsg)
        class(my_type), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        iostat = 0
        iomsg = ""
        write(unit, '(i0)', iostat=iostat, iomsg=iomsg) self%x
    end subroutine output

end module derived_types_105_my_type

! This module has a subroutine named "output" (same as the write(formatted)
! implementation in my_type). This tests that overloaded write resolution
! correctly handles name collisions.
module derived_types_105_client
    use derived_types_105_my_type, only: my_type
    implicit none

    type :: client_type
    contains
        procedure, nopass :: output
    end type client_type

contains

    subroutine output(obj)
        type(my_type), intent(in) :: obj
        write(10, '(dt)') obj
    end subroutine output

end module derived_types_105_client

program derived_types_105
    use derived_types_105_client
    implicit none

    type(client_type) :: t
    type(my_type) :: m
    character(len=20) :: tmp

    m%x = 42
    open(10, file="derived_types_105_file.txt", form="formatted")
    call t%output(m)
    close(10)

    open(10, file="derived_types_105_file.txt", form="formatted")
    read(10, '(a)') tmp
    close(10, status="delete")

    if (trim(tmp) /= "42") error stop
    print *, "PASS"
end program derived_types_105
