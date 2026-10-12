! Located diagnostics for traits with declared associated types (paper
! sect:tats) and for implementations that mutate their receiver (PAPER-D07:
! direct concrete calls only). Each numbered case is an independent
! violation; all other code is valid.
module cc_traits_associated_contracts
    implicit none
    abstract interface :: IPeek
        type :: Element
        function count_after(item) result(n)
            type(Element), intent(in) :: item
            integer                   :: n
        end function count_after
        function first_or(item) result(e)
            type(Element), intent(in) :: item
            type(Element)             :: e
        end function first_or
    end interface IPeek

    abstract interface :: IAppendable
        type :: Element
        subroutine append(item)
            type(Element), intent(in) :: item
        end subroutine append
    end interface IAppendable

    abstract interface :: ICounter
        subroutine bump()
        end subroutine bump
    end interface ICounter
end module cc_traits_associated_contracts

! A01: an associated type no message uses cannot be inferred.
module cc_traits_associated_unused
    implicit none
    abstract interface :: IUnused
        type :: Thing
        subroutine reset()
        end subroutine reset
    end interface IUnused
end module cc_traits_associated_unused

! A08: itself is the implementing type, not an associated type.
module cc_traits_associated_itself
    implicit none
    abstract interface :: ISelf
        type :: itself
        subroutine take(item)
            type(itself), intent(in) :: item
        end subroutine take
    end interface ISelf
end module cc_traits_associated_itself

module cc_traits_associated_views
    use cc_traits_associated_contracts, only: IPeek
    implicit none
contains
    ! A02: a trait with a declared associated type is a constraint only.
    subroutine show_peek(x)
        class(IPeek), intent(in) :: x
    end subroutine show_peek
end module cc_traits_associated_views

! A03: Element is bound to integer by count_after and to real by first_or.
module cc_traits_associated_element_conflict
    use cc_traits_associated_contracts, only: IPeek
    implicit none
    type, sealed, implements(IPeek) :: Mixed
        integer :: n = 0
    contains
        procedure, pass :: count_after => mixed_count_after
        procedure, pass :: first_or => mixed_first_or
    end type Mixed
contains
    function mixed_count_after(self, item) result(n)
        type(Mixed), intent(in) :: self
        integer,     intent(in) :: item
        integer                 :: n
        n = self%n + item
    end function mixed_count_after
    function mixed_first_or(self, item) result(e)
        type(Mixed), intent(in) :: self
        real,        intent(in) :: item
        real                    :: e
        e = item + self%n
    end function mixed_first_or
end module cc_traits_associated_element_conflict

! A04: a character associated type needs a constant length.
module cc_traits_associated_assumed_length
    use cc_traits_associated_contracts, only: IAppendable
    implicit none
    type, sealed, implements(IAppendable) :: Text
        integer :: n = 0
    contains
        procedure, pass :: append => text_append
    end type Text
contains
    subroutine text_append(self, item)
        type(Text),       intent(in) :: self
        character(len=*), intent(in) :: item
        print *, self%n + len(item)
    end subroutine text_append
end module cc_traits_associated_assumed_length

module cc_traits_associated_generic
    use cc_traits_associated_contracts, only: IPeek
    implicit none
contains
    ! A05: generic code cannot name a binder's associated types yet.
    function peek_count{IPeek :: T}(x) result(n)
        type(T), intent(in) :: x
        integer             :: n
        n = 0
    end function peek_count
end module cc_traits_associated_generic

module cc_traits_associated_peek_counter
    use cc_traits_associated_contracts, only: IPeek
    implicit none
    type, sealed, implements(IPeek) :: Counter
        integer :: n = 0
    contains
        procedure, pass :: count_after => counter_count_after
        procedure, pass :: first_or => counter_first_or
    end type Counter
contains
    function counter_count_after(self, item) result(n)
        type(Counter), intent(in) :: self
        integer,       intent(in) :: item
        integer                   :: n
        n = self%n + item
    end function counter_count_after
    function counter_first_or(self, item) result(e)
        type(Counter), intent(in) :: self
        integer,       intent(in) :: item
        integer                   :: e
        e = item
        if (self%n /= 0) e = self%n
    end function counter_first_or
end module cc_traits_associated_peek_counter

module cc_traits_associated_box
    implicit none
    private
    type, public, sealed :: Box{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: box_init
    end type Box
contains
    function box_init(item) result(res)
        type(U), intent(in) :: item
        type(Box{U})        :: res
        res%items = [item]
    end function box_init
end module cc_traits_associated_box

module cc_traits_associated_box_user
    use cc_traits_associated_box, only: Box
    implicit none
contains
    subroutine wrap(s)
        character(len=*), intent(in) :: s
        type(Box{character(len=4)}) :: fixed
        fixed = Box{character(len=4)}("four")
        ! A07: a character type argument has a constant length.
        print *, size([Box(s)])
    end subroutine wrap
end module cc_traits_associated_box_user

! M01: a receiver must be intent(in) or intent(inout).
module cc_traits_associated_out_receiver
    use cc_traits_associated_contracts, only: ICounter
    implicit none
    type, sealed, implements(ICounter) :: Reset
        integer :: n = 0
    contains
        procedure, pass :: bump => reset_bump
    end type Reset
contains
    subroutine reset_bump(self)
        type(Reset), intent(out) :: self
        self%n = 1
    end subroutine reset_bump
end module cc_traits_associated_out_receiver

module cc_traits_associated_counter
    use cc_traits_associated_contracts, only: ICounter, IAppendable
    implicit none
    type, sealed, implements(ICounter) :: Count
        integer :: n = 0
    contains
        procedure, pass :: bump => count_bump
    end type Count

    type, sealed, implements(IAppendable) :: Tally
        integer :: total = 0
    contains
        procedure, pass :: append => tally_append
    end type Tally
contains
    subroutine count_bump(self)
        type(Count), intent(inout) :: self
        self%n = self%n + 1
    end subroutine count_bump

    subroutine tally_append(self, item)
        type(Tally), intent(inout) :: self
        integer,     intent(in)    :: item
        self%total = self%total + item
    end subroutine tally_append
end module cc_traits_associated_counter

module cc_traits_associated_readonly
    use cc_traits_associated_contracts, only: ICounter
    use cc_traits_associated_counter, only: Count, Tally
    implicit none
contains
    ! M02: a direct call cannot mutate an intent(in) receiver.
    subroutine bump_in(c)
        type(Count), intent(in) :: c
        call c%bump()
    end subroutine bump_in

    ! M03: nor a component of one.
    subroutine append_in(t)
        type(Tally), intent(in) :: t(2)
        call t(1)%append(4)
    end subroutine append_in

    subroutine generic_bump{ICounter :: T}(x)
        type(T), intent(in) :: x
        call x%bump()
    end subroutine generic_bump

    subroutine view_bump(x)
        class(ICounter), intent(in) :: x
        call x%bump()
    end subroutine view_bump
end module cc_traits_associated_readonly

program continue_compilation_traits_associated
    use cc_traits_associated_peek_counter, only: Counter
    use cc_traits_associated_counter, only: Count, Tally
    use cc_traits_associated_readonly, only: generic_bump, view_bump
    implicit none
    type(Counter) :: k
    type(Count) :: c
    type(Tally) :: t
    type(Tally), parameter :: frozen = Tally(3)
    integer :: i
    k = Counter(1)
    ! A06: Element of Counter is integer, not logical.
    i = k%count_after(.true.)
    ! Direct calls on concrete variables mutate them.
    call c%bump()
    call t%append(2)
    ! M04: a named constant is not definable.
    call frozen%append(1)
    ! M05: nor is an associate name of a constant selector.
    associate (fixed => Tally(5))
        call fixed%append(1)
    end associate
    ! M06: a generic trait call is read-only.
    call generic_bump(c)
    ! M07: a runtime view is read-only.
    call view_bump(c)
    i = c%n + t%total
    print *, i
end program continue_compilation_traits_associated

! A09: an instance copies every procedure of its package, so default output of
! a type argument with an allocatable component is not implemented yet, even
! by a procedure the program never calls. An instance whose type argument can
! be transferred gets no diagnostic.
module cc_traits_associated_shelf
    implicit none
    private
    type, public, sealed :: Shelf{U}
        private
        type(U), allocatable :: items(:)
    contains
        initial :: shelf_init
        procedure, pass :: show
    end type Shelf
contains
    function shelf_init(item) result(res)
        type(U), intent(in) :: item
        type(Shelf{U})      :: res
        res%items = [item]
    end function shelf_init

    subroutine show(self)
        type(Shelf{U}), intent(in) :: self
        print *, self%items
    end subroutine show
end module cc_traits_associated_shelf

module cc_traits_associated_shelf_user
    use cc_traits_associated_shelf, only: Shelf
    implicit none
    type :: Bag
        integer, allocatable :: a(:)
    end type Bag
    type :: Pair
        integer :: a = 0
        real    :: b(2) = 0
    end type Pair
contains
    subroutine fill()
        type(Shelf{Pair}) :: pairs
        type(Shelf{Bag}) :: bags
        pairs = Shelf(Pair(1, [2.0, 3.0]))
    end subroutine fill
end module cc_traits_associated_shelf_user

! A10: a unit abandoned because one of its generic type instances is not
! implemented yet (A09) does not stop the units after it from being checked.
module cc_traits_associated_chain
    use cc_traits_associated_shelf, only: Shelf
    implicit none
    type :: Link
        integer          :: n = 0
        integer, pointer :: next => null()
    end type Link
contains
    subroutine chain()
        type(Shelf{Link}) :: links
    end subroutine chain
end module cc_traits_associated_chain

! A11: an instance does not call the defined input/output procedures of the
! items it transfers yet, so an item that needs one for the transfer is
! diagnosed instead of being transferred by default.
module cc_traits_associated_labels
    implicit none
    type :: Label
        integer :: n = 0
    contains
        procedure :: write_label
        generic :: write(formatted) => write_label
    end type Label
    type :: Sleeve
        integer     :: k = 0
        type(Label) :: tag
    end type Sleeve
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
end module cc_traits_associated_labels

module cc_traits_associated_labels_user
    use cc_traits_associated_shelf, only: Shelf
    use cc_traits_associated_labels, only: Label
    implicit none
contains
    subroutine shelve_labels()
        type(Shelf{Label}) :: labels
    end subroutine shelve_labels
end module cc_traits_associated_labels_user

! A12: a component that needs a defined input/output procedure keeps the
! default output of the item that contains it from the instance as well.
module cc_traits_associated_sleeves_user
    use cc_traits_associated_shelf, only: Shelf
    use cc_traits_associated_labels, only: Sleeve
    implicit none
contains
    subroutine shelve_sleeves()
        type(Shelf{Sleeve}) :: sleeves
    end subroutine shelve_sleeves
end module cc_traits_associated_sleeves_user

! A13: an instance transfers a derived type item without defined
! input/output only by formatted output, so input of one is not implemented
! yet; an instance of an intrinsic type reads as usual.
module cc_traits_associated_loader
    implicit none
    private
    type, public, sealed :: Loader{U}
        private
        type(U) :: item
    contains
        initial :: loader_init
        procedure, pass :: load
    end type Loader
contains
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
end module cc_traits_associated_loader

module cc_traits_associated_loader_user
    use cc_traits_associated_loader, only: Loader
    implicit none
    type :: Coordinates
        integer :: x = 0
        integer :: y = 0
    end type Coordinates
contains
    subroutine load_all()
        type(Loader{integer}) :: numbers
        type(Loader{Coordinates}) :: places
    end subroutine load_all
end module cc_traits_associated_loader_user
