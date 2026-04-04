module derived_types_134_core
use derived_types_134_module, only: ctx_t, get_default, set_default, create_ctx
implicit none
contains

subroutine do_init()
    type(ctx_t), pointer :: ctx
    ctx => get_default()
    if (.not. associated(ctx)) then
        ctx => create_ctx()
        call set_default(ctx)
    end if
    ctx%val = 42
    ctx%sub%x = 10
end subroutine do_init

subroutine do_shutdown()
    type(ctx_t), pointer :: ctx
    ctx => get_default()
    if (.not. associated(ctx)) return
    call ctx%shutdown()
    call set_default(null())
end subroutine do_shutdown

end module derived_types_134_core

program derived_types_134
use derived_types_134_core, only: do_init
use derived_types_134_module, only: get_default, ctx_t
implicit none
type(ctx_t), pointer :: ctx

call do_init()
ctx => get_default()
if (ctx%val /= 42) error stop
if (ctx%sub%x /= 10) error stop
deallocate(ctx)

print *, "PASS"
end program derived_types_134
