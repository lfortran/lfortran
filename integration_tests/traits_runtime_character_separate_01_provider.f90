module traits_runtime_character_separate_01_provider_m
    use traits_runtime_character_separate_01_contracts_m, only: ICode, IPick, ILabel
    implicit none
    private
    public :: build_picker, build_label

    type, sealed, implements(IPick) :: HiddenMin
    contains
        procedure, nopass :: pick => hidden_min_pick
    end type HiddenMin

    ! The passed object is the third dummy argument.
    type, sealed, implements(IPick) :: HiddenPairwise
        private
        class(IPick), allocatable :: other
    contains
        initial :: hidden_pairwise_init
        procedure, pass(self) :: pick => hidden_pairwise_pick
    end type HiddenPairwise

    type, sealed, implements(IPick) :: HiddenReverse
    contains
        procedure, pass :: pick => hidden_reverse_pick
    end type HiddenReverse

    type, sealed, implements(ILabel) :: HiddenLabel
        private
        character(len=2) :: tag = '#:'
    contains
        procedure, pass(self) :: label => hidden_label
        procedure, nopass :: first_letter => hidden_first_letter
    end type HiddenLabel

contains

    function hidden_min_pick{ICode :: T}(x, floor) result(r)
        type(T), intent(in) :: x(:)
        type(T), intent(in) :: floor
        type(T)             :: r, m
        integer             :: i
        r = floor
        if (size(x) == 0) return
        m = x(1)
        do i = 2, size(x)
            if (x(i) < m) m = x(i)
        end do
        if (m > floor) r = m
    end function hidden_min_pick

    function hidden_pairwise_init(other) result(res)
        class(IPick), intent(in) :: other
        type(HiddenPairwise)     :: res
        res%other = other
    end function hidden_pairwise_init

    function hidden_pairwise_pick{ICode :: T}(x, floor, self) result(r)
        type(T),              intent(in) :: x(:)
        type(T),              intent(in) :: floor
        type(HiddenPairwise), intent(in) :: self
        type(T)                          :: r, a, b
        integer                          :: m
        if (size(x) <= 2) then
            r = self%other%pick(x, floor)
        else
            m = size(x) / 2
            a = self%pick(x(:m), floor)
            b = self%pick(floor=floor, x=x(m+1:))
            r = a
            if (b < a) r = b
        end if
    end function hidden_pairwise_pick

    function hidden_reverse_pick{ICode :: T}(self, x, floor) result(r)
        type(HiddenReverse), intent(in) :: self
        type(T),             intent(in) :: x(:)
        type(T),             intent(in) :: floor
        type(T)                         :: r, m
        integer                         :: i
        r = floor
        if (size(x) == 0) return
        m = x(size(x))
        do i = size(x) - 1, 1, -1
            if (x(i) <= m) m = x(i)
        end do
        if (m >= floor) r = m
    end function hidden_reverse_pick

    function hidden_label(code, self, width) result(s)
        character(len=*),  intent(in) :: code
        type(HiddenLabel), intent(in) :: self
        integer,           intent(in) :: width
        character(len=5)              :: s
        s = self%tag // code(1:min(width, len(code)))
    end function hidden_label

    function hidden_first_letter(text) result(c)
        character(len=*), intent(in) :: text
        character                    :: c
        c = ' '
        if (len(text) > 0) c = text(1:1)
    end function hidden_first_letter

    subroutine build_picker(choice, object)
        integer, intent(in) :: choice
        class(IPick), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            object = HiddenMin()
        case (2)
            object = HiddenPairwise(HiddenMin())
        case (3)
            object = HiddenPairwise(HiddenReverse())
        case (4)
            object = HiddenReverse()
        case default
            error stop 'unknown picker choice'
        end select
    end subroutine build_picker

    subroutine build_label(object)
        class(ILabel), allocatable, intent(out) :: object
        object = HiddenLabel()
    end subroutine build_label
end module traits_runtime_character_separate_01_provider_m

subroutine make_picker(choice, object)
    use traits_runtime_character_separate_01_contracts_m, only: IPick
    use traits_runtime_character_separate_01_provider_m, only: build_picker
    implicit none
    integer, intent(in) :: choice
    class(IPick), allocatable, intent(out) :: object
    call build_picker(choice, object)
end subroutine make_picker

subroutine make_label(object)
    use traits_runtime_character_separate_01_contracts_m, only: ILabel
    use traits_runtime_character_separate_01_provider_m, only: build_label
    implicit none
    class(ILabel), allocatable, intent(out) :: object
    call build_label(object)
end subroutine make_label
