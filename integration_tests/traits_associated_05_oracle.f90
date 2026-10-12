! Standard-Fortran oracle of traits_associated_05: each generic type instance
! is written out as an ordinary derived type with its own procedures.
module traits_associated_05_oracle_labels_m
    implicit none

    type :: Label
        integer :: n = 0
    contains
        procedure :: write_label
        generic :: write(formatted) => write_label
    end type Label

    type :: Entry
        integer :: n = 0
    contains
        procedure :: read_entry
        generic :: read(formatted) => read_entry
    end type Entry

contains

    subroutine write_label(dtv, unit, iotype, v_list, iostat, iomsg)
        class(Label), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write(unit, '(a,i0)', iostat=iostat, iomsg=iomsg) 'label ', dtv%n
    end subroutine write_label

    subroutine read_entry(dtv, unit, iotype, v_list, iostat, iomsg)
        class(Entry), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        read(unit, *, iostat=iostat, iomsg=iomsg) dtv%n
    end subroutine read_entry
end module traits_associated_05_oracle_labels_m

module traits_associated_05_oracle_pack_m
    use traits_associated_05_oracle_labels_m, only: Label, Entry
    implicit none
    private
    public :: ShelfEntry, StackLabel, MarkerInteger, LoaderInteger

    type :: ShelfEntry
        type(Entry), allocatable :: items(:)
    contains
        procedure :: render => render_entry
    end type ShelfEntry

    type :: StackLabel
        type(Label), allocatable :: items(:)
    contains
        procedure :: depth => depth_label
    end type StackLabel

    type :: MarkerInteger
        integer :: item = 0
    contains
        procedure :: stamp => stamp_marker
    end type MarkerInteger

    type :: LoaderInteger
        integer :: item = 0
    contains
        procedure :: load => load_integer
        procedure :: get => get_integer
    end type LoaderInteger

contains

    function render_entry(self) result(text)
        class(ShelfEntry), intent(in) :: self
        character(len=80)             :: text
        write(text, *) self%items
    end function render_entry

    function depth_label(self) result(n)
        class(StackLabel), intent(in) :: self
        integer                       :: n
        n = size(self%items)
    end function depth_label

    subroutine stamp_marker(self, unit)
        class(MarkerInteger), intent(in) :: self
        integer,              intent(in) :: unit
        type(Label) :: mark
        mark%n = 3
        write(unit, *) mark
    end subroutine stamp_marker

    subroutine load_integer(self, text)
        class(LoaderInteger), intent(inout) :: self
        character(*),         intent(in)    :: text
        read(text, *) self%item
    end subroutine load_integer

    function get_integer(self) result(item)
        class(LoaderInteger), intent(in) :: self
        integer                          :: item
        item = self%item
    end function get_integer
end module traits_associated_05_oracle_pack_m

program traits_associated_05_oracle
    use traits_associated_05_oracle_labels_m, only: Label, Entry
    use traits_associated_05_oracle_pack_m, only: ShelfEntry, StackLabel, &
        MarkerInteger, LoaderInteger
    implicit none
    integer :: checks, unit, n
    character(len=80) :: line
    type(Label) :: tag
    type(MarkerInteger) :: markers
    type(ShelfEntry) :: entries
    type(StackLabel) :: labels
    type(LoaderInteger) :: numbers

    checks = 0

    tag%n = 7
    open(newunit=unit, status='scratch', form='formatted')
    write(unit, *) tag
    rewind(unit)
    read(unit, '(a)') line
    close(unit)
    call check(index(line, 'label 7') > 0, 1)

    markers%item = 1
    open(newunit=unit, status='scratch', form='formatted')
    call markers%stamp(unit)
    rewind(unit)
    read(unit, '(a)') line
    close(unit)
    call check(index(line, 'label 3') > 0, 2)

    entries%items = [Entry(4)]
    line = entries%render()
    read(line, *) n
    call check(n == 4, 3)

    labels%items = [Label(5)]
    call check(labels%depth() == 1, 4)

    numbers%item = 0
    call numbers%load('8')
    call check(numbers%get() == 8, 5)

    print '(a,i0)', 'traits_associated_05_oracle checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_05_oracle failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_05_oracle
