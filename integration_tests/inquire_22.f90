program inquire_22
    ! SIZE= and POS= in INQUIRE accept any scalar integer variable:
    ! all integer kinds, components, array elements, pointers and allocatables.
    implicit none
    type :: file_info
        integer(4) :: size4
        integer(8) :: pos8
        integer(4) :: sizes(2)
    end type
    type(file_info) :: info
    integer(1) :: size1, pos1
    integer(2) :: size2, pos2
    integer(4) :: size4, pos4
    integer(8) :: size8, pos8
    integer :: sizes(2), unit, stat
    integer, pointer :: psize
    integer, target :: psize_target
    integer, allocatable :: asize
    character :: c
    character(*), parameter :: fname = "inquire_22_data.txt"
    integer, parameter :: nbytes = 10

    open(newunit=unit, file=fname, access="stream", form="unformatted", &
         status="replace", iostat=stat)
    if (stat /= 0) error stop 1
    write(unit) "0123456789"
    close(unit)
    open(newunit=unit, file=fname, access="stream", form="unformatted", &
         status="old", iostat=stat)
    if (stat /= 0) error stop 2

    ! Every integer kind is accepted for SIZE= and POS=.
    inquire(unit=unit, size=size1, pos=pos1, iostat=stat)
    if (stat /= 0 .or. size1 /= nbytes .or. pos1 /= 1) error stop 3
    inquire(unit=unit, size=size2, pos=pos2, iostat=stat)
    if (stat /= 0 .or. size2 /= nbytes .or. pos2 /= 1) error stop 4
    inquire(unit=unit, size=size4, pos=pos4, iostat=stat)
    if (stat /= 0 .or. size4 /= nbytes .or. pos4 /= 1) error stop 5
    inquire(unit=unit, size=size8, pos=pos8, iostat=stat)
    if (stat /= 0 .or. size8 /= nbytes .or. pos8 /= 1) error stop 6

    ! A scalar derived-type component and an array element are definable.
    inquire(unit=unit, size=info%size4, pos=sizes(2), iostat=stat)
    if (stat /= 0 .or. info%size4 /= nbytes .or. sizes(2) /= 1) error stop 7

    ! An element of a derived-type component array is definable too.
    inquire(unit=unit, size=info%sizes(1), pos=info%pos8, iostat=stat)
    if (stat /= 0 .or. info%sizes(1) /= nbytes .or. info%pos8 /= 1) error stop 8

    ! Pointers and allocatables are definable.
    psize => psize_target
    allocate(asize)
    inquire(unit=unit, size=psize, pos=asize, iostat=stat)
    if (stat /= 0 .or. psize /= nbytes .or. asize /= 1) error stop 9
    deallocate(asize)

    ! POS= reports the stream position, proving the values are not stale.
    read(unit, iostat=stat) c
    if (stat /= 0 .or. c /= "0") error stop 10
    inquire(unit=unit, size=size8, pos=info%sizes(2), iostat=stat)
    if (stat /= 0 .or. size8 /= nbytes .or. info%sizes(2) /= 2) error stop 11

    close(unit, status="delete")
    print *, "ok"
end program
