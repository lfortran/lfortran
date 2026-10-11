module array_finalization
    implicit none
    integer :: elements = 0, vectors = 0, matrices = 0, scalar_calls = 0, shaped_calls = 0
    integer :: live_payloads = 0
    type elemental_t
        integer, allocatable :: payload(:)
    contains
        final :: finish_element
    end type
    type ranked_t
        integer :: value = 0
    contains
        final :: finish_ranked_element
        final :: finish_vector
        final :: finish_matrix
    end type
    type shaped_t
        integer :: value = 0
    contains
        final :: finish_shaped
    end type
    type scalar_t
        integer :: value = 0
    contains
        final :: finish_scalar
    end type
contains
    subroutine local_scalar()
        type(ranked_t) :: x
        x%value = 1
    end subroutine
    impure elemental subroutine finish_element(x)
        type(elemental_t), intent(inout) :: x
        ! FINAL must execute while the allocatable component is still alive.
        if (allocated(x%payload)) then
            if (size(x%payload) /= 2) error stop 1
            if (any(x%payload /= 7)) error stop 2
            live_payloads = live_payloads + 1
        end if
        elements = elements + 1
    end subroutine
    impure elemental subroutine finish_ranked_element(x)
        type(ranked_t), intent(inout) :: x
        elements = elements + 100
    end subroutine
    subroutine finish_vector(x)
        type(ranked_t), intent(inout) :: x(:)
        vectors = vectors + 1
        if (size(x) /= 0 .and. size(x) /= 3) error stop 3
        if (any(x%value /= 9)) error stop 4
    end subroutine
    subroutine finish_matrix(x)
        type(ranked_t), intent(inout) :: x(:,:)
        matrices = matrices + 1
        if (size(x) /= 6) error stop 5
        if (any(x%value /= 11)) error stop 6
    end subroutine
    subroutine finish_shaped(x)
        type(shaped_t), intent(inout) :: x(2)
        if (any(x%value /= 5)) error stop 18
        shaped_calls = shaped_calls + 1
    end subroutine
    subroutine finish_scalar(x)
        type(scalar_t), intent(inout) :: x
        scalar_calls = scalar_calls + 1
    end subroutine
end module
program test_array_finalization
    use array_finalization
    implicit none
    type(elemental_t), allocatable :: a(:), b(:,:)
    type(ranked_t), allocatable :: v(:), m(:,:), cube(:,:,:), scalar
    type(elemental_t), pointer :: pointer_array(:)
    type(scalar_t), allocatable :: s(:)
    type(shaped_t), allocatable :: shaped(:)
    integer :: i
    allocate(a(-1:1))
    do i = -1, 1
        allocate(a(i)%payload(2))
        a(i)%payload = 7
    end do
    deallocate(a)
    if (elements /= 3 .or. live_payloads /= 3) error stop 7
    allocate(b(2,3))
    deallocate(b)
    if (elements /= 9) error stop 8
    allocate(a(0))
    deallocate(a)
    if (elements /= 9) error stop 9
    allocate(v(-1:1))
    v%value = 9
    deallocate(v)
    if (vectors /= 1 .or. elements /= 9) error stop 10
    allocate(v(0))
    deallocate(v)
    if (vectors /= 2 .or. elements /= 9) error stop 11
    allocate(m(2,3))
    m%value = 11
    deallocate(m)
    if (matrices /= 1 .or. elements /= 9) error stop 12
    ! A nonelemental scalar FINAL never applies to an array.
    allocate(s(2))
    deallocate(s)
    if (scalar_calls /= 0) error stop 13
    ! With no matching-rank FINAL, use the elemental FINAL.
    allocate(cube(2,1,1))
    deallocate(cube)
    if (elements /= 209) error stop 14
    ! DEALLOCATE also finalizes the target of an allocated pointer array.
    allocate(pointer_array(2))
    deallocate(pointer_array)
    if (elements /= 211) error stop 15
    ! Retaining array FINAL interfaces must not call them for a scalar.
    allocate(scalar)
    deallocate(scalar)
    if (elements /= 311 .or. vectors /= 2 .or. matrices /= 1) error stop 16
    call local_scalar()
    if (elements /= 411 .or. vectors /= 2 .or. matrices /= 1) error stop 17
    allocate(shaped(2))
    shaped%value = 5
    deallocate(shaped)
    if (shaped_calls /= 1) error stop 19
end program
