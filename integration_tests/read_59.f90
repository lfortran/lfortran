module read_59_mod
    implicit none
    type :: t
        integer :: x = 0
    contains
        procedure :: ru
        generic :: read(unformatted) => ru
    end type t

contains
    subroutine ru(dtv, unit, iostat, iomsg)
        class(t), intent(inout)     :: dtv
        integer, intent(in)         :: unit
        integer, intent(out)        :: iostat
        character(*), intent(inout) :: iomsg
        read(unit, iostat=iostat, iomsg=iomsg) dtv%x
    end subroutine ru
end module read_59_mod

program read_59
    use read_59_mod
    implicit none
    type(t) :: a
    integer :: unit
    integer :: iostat
    character(100) :: iomsg
    open(newunit=unit, file="tmp.bin", form="unformatted", status="replace")
    write(unit) 42
    close(unit)
    open(newunit=unit, file="tmp.bin", form="unformatted", status="old")
    read(unit, iostat=iostat, iomsg=iomsg) a
    close(unit)
    if (iostat /= 0) error stop "read failed"
    if (a%x /= 42) error stop "incorrect value"
    print *, "value:", a%x
end program read_59