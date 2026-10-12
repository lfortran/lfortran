! Compiled into an object file of its own. It uses this_image(),
! num_images() and coindexed reads and writes of the same type as the main
! program, so both object files define the same compiler-generated helpers,
! which must not clash at link time.
subroutine coarrays_53_sub(me, n, left)
    implicit none
    integer, intent(out) :: me, n, left
    integer, save :: x[*]
    me = this_image()
    n = num_images()
    x = 10 * me
    sync all
    left = x[mod(me + n - 2, n) + 1]
    sync all
    x[mod(me, n) + 1] = me
    sync all
    if (x /= mod(me + n - 2, n) + 1) error stop 10
end subroutine
