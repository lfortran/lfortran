! An automatic array local to a function called from ASSOCIATE inside
! do concurrent is declared at the top of the kernel, backed by a
! per-thread slice of a workspace buffer. The slice of a thread starts at
! the thread's index times the slice size, counted in 64 bits (#15380).
program gpu_metal_711
implicit none
integer :: out(1000), i, n
n = 50
do concurrent (i=1:1000)
    associate (v => i)
        out(i) = f(n, v)
    end associate
end do
do i = 1, 1000
    if (out(i) /= 50*i + 1) error stop 1
end do
print *, "PASS"
contains
pure integer function f(m, v) result(r)
    integer, intent(in) :: m, v
    integer :: work(m)
    work = v
    work(m) = v + 1
    r = sum(work)
end function
end program
