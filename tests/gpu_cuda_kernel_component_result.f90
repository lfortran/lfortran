! The whole allocatable component of an element of an array of a derived
! type is assigned a type-bound function's result inside a kernel. Without
! reallocation on assignment, the kernel copies the per-thread result into
! the components' packed data from the element's offset there, which it
! reads into a local first: with --descriptor-index-64 that local is 64-bit
! like the offsets, so a component starting at or beyond huge(0) elements
! into the packed data is written in place.
module gpu_cuda_kernel_component_result_m
    implicit none
    type :: t
        integer, allocatable :: v(:)
    contains
        procedure :: shifted
    end type
contains
    pure function shifted(self, k) result(w)
        class(t), intent(in) :: self
        integer, intent(in) :: k
        integer, allocatable :: w(:)
        w = self%v + k
    end function
end module

program gpu_cuda_kernel_component_result
    use gpu_cuda_kernel_component_result_m, only: t
    implicit none
    type(t) :: x(4)
    integer :: i, n(4)
    n = [5, 0, 12, 7]
    do i = 1, 4
        allocate(x(i)%v(n(i)))
        x(i)%v = i
    end do
    do concurrent (i = 1:4)
        x(i)%v = x(i)%shifted(10*i)
    end do
    print *, x(4)%v
end program
