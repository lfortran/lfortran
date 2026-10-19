! The consumer, compiled without the provider: it receives the polymorphic
! results through views, an owner component and a trait pointer.
module traits_runtime_polymorphic_result_separate_01_consumer_m
    use traits_runtime_polymorphic_result_separate_01_contracts_m, only: ILookup, Point
    implicit none
    type :: Model
        class(ILookup), allocatable :: source
    end type Model
contains
    function label(view) result(res)
        class(ILookup), intent(in) :: view
        class(*), allocatable :: res
        res = view%lookup("label")
    end function label

    ! The receiver is a view of a table of n entries.
    subroutine check_view(view, n)
        class(ILookup), intent(in) :: view
        integer, intent(in) :: n
        class(*), allocatable :: r
        class(Point), allocatable :: q
        integer, allocatable :: k
        r = view%lookup("n")
        select type (r)
        type is (integer)
            if (r /= n) error stop 1
        class default
            error stop 2
        end select
        r = view%lookup("half")
        select type (r)
        type is (real(8))
            if (r /= n / 2d0) error stop 3
        class default
            error stop 4
        end select
        r = label(view)
        select type (r)
        type is (character(*))
            if (len(r) /= n .or. r /= repeat("t", n)) error stop 5
        class default
            error stop 6
        end select
        ! A type that only the provider declares.
        select type (s => view%lookup("other"))
        class is (Point)
            if (s%x /= n .or. s%y /= 5) error stop 7
        class default
            error stop 8
        end select
        q = view%origin()
        if (n > 2 .and. (q%x /= 1 .or. q%y /= 2)) error stop 9
        if (n <= 2 .and. (q%x /= -1 .or. q%y /= -2)) error stop 10
        k = view%size_of()
        if (k /= n .or. view%size_of() + 1 /= n + 1) error stop 11
    end subroutine check_view

    subroutine check_model(m, n)
        type(Model), intent(in) :: m
        integer, intent(in) :: n
        call check_view(m%source, n)
    end subroutine check_model

    subroutine check_pointer(p, n)
        class(ILookup), pointer, intent(in) :: p
        integer, intent(in) :: n
        call check_view(p, n)
    end subroutine check_pointer
end module traits_runtime_polymorphic_result_separate_01_consumer_m
