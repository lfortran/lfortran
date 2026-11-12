program prog_ext_sub
implicit none
type :: t
    procedure(__p_iface_implicit), pointer :: p
end type t
type(t) :: a
integer(4) :: i
integer(4) :: y
external :: __p_iface_implicit
integer(4), external :: f
external :: g
external :: sub
i = 1
y = f(1)
call g(i)
a%p => sub
call a%p(1)

contains

subroutine lcompilers_intent_out_reset_t(object)
    type(t), intent(inout) :: object
end subroutine lcompilers_intent_out_reset_t

end program prog_ext_sub
