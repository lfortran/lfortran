! The consumer, compiled without the provider: it applies the elemental
! messages to arrays through views, an owner component and a trait pointer.
module traits_runtime_elemental_separate_01_consumer_m
    use traits_runtime_elemental_separate_01_contracts_m, only: IFloors
    implicit none
    type :: Model
        class(IFloors), allocatable :: source
    end type Model
contains
    ! A Floors with floor 1 and weight 2.
    subroutine check_view(v)
        class(IFloors), intent(in) :: v
        real(8) :: p(6), q(6), m(2, 3)
        integer :: i
        p = [(i - 3d0, i = 1, 6)]
        q = v%pfloor(p)
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 1
        q = 0
        q(1:6:2) = v%pfloor(p(6:1:-2))
        if (any(q /= [3d0, 0d0, 1d0, 0d0, 1d0, 0d0])) error stop 2
        q = v%blend(p, 2d0)
        if (any(q /= 2 * p + 2)) error stop 3
        if (sum(v%pfloor(p)) /= 9d0 .or. maxval(v%blend(p, p)) /= 9d0) error stop 4
        q = p
        q(2:5) = v%pfloor(q(1:4))
        if (any(q /= [-2d0, 1d0, 1d0, 1d0, 1d0, 3d0])) error stop 5
        m = reshape(p, [2, 3])
        m = v%blend(m, m)
        if (any(m /= 3 * reshape(p, [2, 3]))) error stop 6
        q = p
        call v%raise(q)
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 7
        where (p > 0) q = v%blend(p, -1d0)
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 3d0, 5d0])) error stop 8
        if (v%pfloor(-4d0) /= 1d0) error stop 9
    end subroutine check_view

    subroutine check_model(m)
        type(Model), intent(in) :: m
        real(8) :: p(3)
        p = [-1d0, 5d0, 0d0]
        p = 2 * m%source%pfloor(p) + 1
        if (any(p /= [3d0, 11d0, 3d0])) error stop 10
        call check_view(m%source)
    end subroutine check_model

    subroutine check_pointer(ptr, n, p)
        class(IFloors), pointer, intent(in) :: ptr
        integer, intent(in) :: n
        real(8), intent(inout) :: p(n)
        p = ptr%pfloor(p)
        call check_view(ptr)
    end subroutine check_pointer
end module traits_runtime_elemental_separate_01_consumer_m
