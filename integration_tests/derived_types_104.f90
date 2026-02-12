module derived_types_104_my_type
    implicit none

    type :: my_type
        real, allocatable :: a(:)
    contains
        procedure, private :: write_formatted
        generic, public :: write(formatted) => write_formatted
    end type my_type

contains

    subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(my_type), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        iostat = 0
        iomsg = ""
        write(unit, '(a)') "OK"
    end subroutine write_formatted

end module derived_types_104_my_type

module derived_types_104_client
    use derived_types_104_my_type
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

end module derived_types_104_client

program derived_types_104
    use derived_types_104_client
    implicit none

    type(client_type) :: t
    type(my_type) :: m
    character(len=10) :: tmp

    open(10, file="derived_types_104_file.txt", form="formatted")
    call t%output(m)
    close(10)

    open(10, file="derived_types_104_file.txt", form="formatted")
    read(10, '(a)') tmp
    close(10)

    if (trim(tmp) /= "OK") error stop
end program derived_types_104
