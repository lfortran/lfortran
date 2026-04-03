module derived_types_134_module
implicit none

type :: inner_t
    integer :: x = 0
contains
    procedure :: reset => inner_reset
end type inner_t

type :: ctx_t
    integer :: val = 0
    type(inner_t) :: sub
contains
    procedure :: shutdown => ctx_shutdown
end type ctx_t

type(ctx_t), pointer :: default_ctx => null()

contains

subroutine inner_reset(self)
    class(inner_t), intent(inout) :: self
    self%x = 0
end subroutine inner_reset

subroutine ctx_shutdown(self)
    class(ctx_t), intent(inout) :: self
    call self%sub%reset()
    self%val = 0
end subroutine ctx_shutdown

function get_default() result(ctx)
    type(ctx_t), pointer :: ctx
    ctx => default_ctx
end function get_default

subroutine set_default(ctx)
    type(ctx_t), pointer, intent(in) :: ctx
    default_ctx => ctx
end subroutine set_default

function create_ctx() result(ctx)
    type(ctx_t), pointer :: ctx
    allocate(ctx)
end function create_ctx

end module derived_types_134_module
