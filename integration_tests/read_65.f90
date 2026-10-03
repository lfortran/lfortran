module read_65_mod
    implicit none
    private

    public :: string_type
    public :: assignment(=)
    public :: len
    public :: operator(==)
    public :: write(unformatted), read(unformatted)

    integer, parameter :: long = selected_int_kind(18)

    type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface assignment(=)
        module procedure :: assign_string_char
    end interface

    interface len
        module procedure :: len_string
    end interface

    interface operator(==)
        module procedure :: eq_string_char
        module procedure :: eq_char_string
        module procedure :: eq_string_string
    end interface

    interface write(unformatted)
        module procedure :: write_unformatted
    end interface

    interface read(unformatted)
        module procedure :: read_unformatted
    end interface

contains

    elemental subroutine assign_string_char(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_string_char

    elemental function len_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length
        if (allocated(string%raw)) then
            length = len(string%raw)
        else
            length = 0
        end if
    end function len_string

    elemental function eq_string_char(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_eq
        if (.not. allocated(lhs%raw)) then
            is_eq = (len(rhs) == 0)
        else
            is_eq = (lhs%raw == rhs)
        end if
    end function eq_string_char

    elemental function eq_char_string(lhs, rhs) result(is_eq)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_eq
        is_eq = (rhs == lhs)
    end function eq_char_string

    elemental function eq_string_string(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_eq
        if (.not. allocated(lhs%raw) .and. .not. allocated(rhs%raw)) then
            is_eq = .true.
        else if (allocated(lhs%raw) .neqv. allocated(rhs%raw)) then
            is_eq = .false.
        else
            is_eq = (lhs%raw == rhs%raw)
        end if
    end function eq_string_string

    subroutine write_unformatted(string, unit, iostat, iomsg)
        type(string_type), intent(in) :: string
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        write(unit, iostat=iostat, iomsg=iomsg) int(len(string), long)
        if (iostat == 0) then
            if (len(string) > 0) then
                write(unit, iostat=iostat, iomsg=iomsg) string%raw
            else
                write(unit, iostat=iostat, iomsg=iomsg) ''
            end if
        end if
    end subroutine write_unformatted

    subroutine read_unformatted(string, unit, iostat, iomsg)
        type(string_type), intent(inout) :: string
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        character(len=:), allocatable :: buffer
        integer(long) :: chunk

        read(unit, iostat=iostat, iomsg=iomsg) chunk
        if (iostat == 0) then
            allocate(character(len=chunk) :: buffer)
            read(unit, iostat=iostat, iomsg=iomsg) buffer
            string%raw = buffer
        end if
    end subroutine read_unformatted

end module read_65_mod

program read_65
    use read_65_mod, only: string_type, assignment(=), len, operator(==), &
        write(unformatted), read(unformatted)
    implicit none

    type(string_type) :: string
    integer :: io, stat
    character(len=256) :: msg

    string = 'Important saved value'

    open(newunit=io, form='unformatted', status='scratch')
    write(io, iostat=stat, iomsg=msg) string
    if (stat /= 0) then
        print *, 'write failed, stat=', stat, ' msg=', trim(msg)
        stop 1
    end if

    string = ''
    rewind(io)

    read(io, iostat=stat, iomsg=msg) string
    if (stat /= 0) then
        print *, 'read failed, stat=', stat, ' msg=', trim(msg)
        stop 2
    end if
    close(io)

    print *, 'len(string)=', len(string)
    print *, 'equal=', (string == 'Important saved value')
    if (len(string) /= 21 .or. .not. (string == 'Important saved value')) error stop
end program read_65
