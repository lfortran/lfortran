module traits_runtime_character_separate_01_consumer_m
    use traits_runtime_character_separate_01_contracts_m, only: ICode, IPick, ILabel
    implicit none
    private
    public :: pick3, pick5, reversed_pick, padded_label

contains

    function pick3(picker, x, floor) result(r)
        class(IPick),     intent(in) :: picker
        character(len=3), intent(in) :: x(:), floor
        character(len=3)             :: r
        r = picker%pick(x, floor)
    end function pick3

    function pick5(picker, x, floor) result(r)
        class(IPick),     intent(in) :: picker
        character(len=5), intent(in) :: x(:), floor
        character(len=5)             :: r
        r = picker%pick{character(len=5)}(floor=floor, x=x)
    end function pick5

    ! Checked and serialized before any client instantiates it.
    function reversed_pick{ICode :: T}(picker, x, floor) result(r)
        class(IPick), intent(in) :: picker
        type(T),      intent(in) :: x(:), floor
        type(T)                  :: r
        r = picker%pick(x(size(x):1:-1), floor)
    end function reversed_pick

    function padded_label(labeler, code) result(s)
        class(ILabel),    intent(in) :: labeler
        character(len=*), intent(in) :: code
        character(len=5)             :: s
        s = labeler%label(width=len(code), code=code)
    end function padded_label
end module traits_runtime_character_separate_01_consumer_m
