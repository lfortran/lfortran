program coarrays_53
    implicit none
    interface
        subroutine coarrays_53_sub(me, n, left)
            integer, intent(out) :: me, n, left
        end subroutine
    end interface
    integer :: me, n, left
    integer :: y[*]
    call coarrays_53_sub(me, n, left)
    if (me /= this_image()) error stop 1
    if (n /= num_images()) error stop 2
    if (left /= 10 * (mod(me + n - 2, n) + 1)) error stop 3
    y = 20 * me
    sync all
    if (y[mod(me, n) + 1] /= 20 * (mod(me, n) + 1)) error stop 4
    sync all
    y[mod(me, n) + 1] = me
    sync all
    if (y /= mod(me + n - 2, n) + 1) error stop 5
    print *, "ok"
end program
