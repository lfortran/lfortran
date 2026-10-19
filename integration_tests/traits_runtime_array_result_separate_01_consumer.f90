! The consumer, compiled without the provider: it receives the array results
! through views, an owner component and a trait pointer.
module traits_runtime_array_result_separate_01_consumer_m
    use traits_runtime_array_result_separate_01_contracts_m, only: ISamples
    implicit none
    type :: Model
        class(ISamples), allocatable :: source
    end type Model
contains
    real(8) function total(view) result(r)
        class(ISamples), intent(in) :: view
        r = sum(view%samples())
    end function total

    function doubled(view) result(y)
        class(ISamples), intent(in) :: view
        real(8), allocatable :: y(:)
        y = view%samples()
        y = 2 * view%samples() + 0 * y
    end function doubled

    integer function window_sum(view, n) result(r)
        class(ISamples), intent(in) :: view
        integer, intent(in) :: n
        integer :: w(n)
        w = view%window(n)
        r = sum(w) + size(view%window(n + 1))
    end function window_sum

    subroutine check_model(m, expected)
        type(Model), intent(in) :: m
        real(8), intent(in) :: expected(:)
        real(8), allocatable :: y(:)
        y = m%source%samples()
        if (size(y) /= size(expected)) error stop 1
        if (any(y /= expected)) error stop 2
        y = m%source%scaled(y)
        if (any(y /= expected * expected(1))) error stop 3
        y = m%source%series(5)
        if (size(y) /= 5 .or. size(m%source%series(2)) /= 2) error stop 6
        if (any(m%source%series(2) - y(1:2) /= 0)) error stop 7
    end subroutine check_model

    subroutine check_pointer(p, n)
        class(ISamples), pointer, intent(in) :: p
        integer, intent(in) :: n
        if (size(p%samples()) /= n) error stop 4
        if (any(p%window(2) /= [n + 1, n + 2])) error stop 5
    end subroutine check_pointer
end module traits_runtime_array_result_separate_01_consumer_m
