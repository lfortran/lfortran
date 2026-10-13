! Defined input/output and generic type instances: the positive controls of
! the temporary limitation that continue_compilation_traits_associated
! (A11-A13) diagnoses. An ordinary data transfer of a type with a defined
! output procedure calls it, and so does every instance's copy of a package
! transfer of such a concrete type; a type whose defined input/output is for
! another transfer is printed by default output; a package without
! input/output takes a type with defined output; and an instance of an
! intrinsic type reads its item.
module traits_associated_05_labels_m
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
end module traits_associated_05_labels_m

module traits_associated_05_pack_m
    use traits_associated_05_labels_m, only: Label
    implicit none
    private

    type, public, sealed :: Shelf{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: shelf_init
        procedure, pass :: render
    end type Shelf

    type, public, sealed :: Stack{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: stack_init
        procedure, pass :: depth
    end type Stack

    type, public, sealed :: Marker{U}
        private
        type(U) :: item
    contains
        initial :: marker_init
        procedure, pass :: stamp
    end type Marker

    type, public, sealed :: Loader{U}
        private
        type(U) :: item
    contains
        initial :: loader_init
        procedure, pass :: load
        procedure, pass :: get
    end type Loader

contains

    function shelf_init(item) result(res)
        type(U), intent(in) :: item
        type(Shelf{U})      :: res
        res%items = [item]
    end function shelf_init

    function render(self) result(text)
        type(Shelf{U}), intent(in) :: self
        character(len=80)          :: text
        write(text, *) self%items
    end function render

    function stack_init(item) result(res)
        type(U), intent(in) :: item
        type(Stack{U})      :: res
        res%items = [item]
    end function stack_init

    function depth(self) result(n)
        type(Stack{U}), intent(in) :: self
        integer                    :: n
        n = size(self%items)
    end function depth

    function marker_init(item) result(res)
        type(U), intent(in) :: item
        type(Marker{U})     :: res
        res%item = item
    end function marker_init

    subroutine stamp(self, unit)
        type(Marker{U}), intent(in) :: self
        integer,         intent(in) :: unit
        type(Label) :: mark
        mark%n = 3
        write(unit, *) mark
    end subroutine stamp

    function loader_init(item) result(res)
        type(U), intent(in) :: item
        type(Loader{U})     :: res
        res%item = item
    end function loader_init

    subroutine load(self, text)
        type(Loader{U}), intent(inout) :: self
        character(*),    intent(in)    :: text
        read(text, *) self%item
    end subroutine load

    function get(self) result(item)
        type(Loader{U}), intent(in) :: self
        type(U)                     :: item
        item = self%item
    end function get
end module traits_associated_05_pack_m

program traits_associated_05
    use traits_associated_05_labels_m, only: Label, Entry
    use traits_associated_05_pack_m, only: Shelf, Stack, Marker, Loader
    implicit none
    integer :: checks, unit, n
    character(len=80) :: line
    type(Label) :: tag

    checks = 0

    tag%n = 7
    open(newunit=unit, status='scratch', form='formatted')
    write(unit, *) tag
    rewind(unit)
    read(unit, '(a)') line
    close(unit)
    call check(index(line, 'label 7') > 0, 1)

    markers := Marker(1)
    open(newunit=unit, status='scratch', form='formatted')
    call markers%stamp(unit)
    rewind(unit)
    read(unit, '(a)') line
    close(unit)
    call check(index(line, 'label 3') > 0, 2)

    entries := Shelf(Entry(4))
    line = entries%render()
    read(line, *) n
    call check(n == 4, 3)

    labels := Stack(Label(5))
    call check(labels%depth() == 1, 4)

    numbers := Loader(0)
    call numbers%load('8')
    call check(numbers%get() == 8, 5)

    print '(a,i0)', 'traits_associated_05 checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_05 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_05
