! Located diagnostics for traits with declared associated types (paper
! sect:tats). Each numbered case is an independent violation; all other code
! is valid.
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

program continue_compilation_traits_associated
    use cc_traits_associated_peek_counter, only: Counter
    implicit none
    type(Counter) :: k
    integer :: i
    k = Counter(1)
    ! A06: Element of Counter is integer, not logical.
    i = k%count_after(.true.)
    print *, i
end program continue_compilation_traits_associated
