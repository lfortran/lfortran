! An elementwise assignment whose value reads the target section through
! the very same section, selected with scalar integer components such as
! `self%start_index` (optimizer updates), reads each element in place
! (issue #16105). Different components or expressions in the subscripts,
! subscripts that read the target, pointers and shifted sections
! must still evaluate the value before the target is defined.
module array_section_assign_12_mod
    implicit none

    type :: range_t
        integer :: s = 1, e = 1
    end type

    type :: sgd_t
        real :: learning_rate = 0.5, momentum = 0.25
        real :: beta1 = 0.75, beta2 = 0.125
        real, allocatable :: velocity(:), m(:), v(:)
        integer :: start_index = 1
        integer :: a = 1, b = 1, c = 1, d = 1
        type(range_t) :: r
        integer, allocatable :: iv(:)
        type(range_t), pointer :: rp => null()
    contains
        procedure :: minimize
        procedure :: adam
    end type

contains

    subroutine minimize(self, param, gradient)
        class(sgd_t), intent(inout) :: self
        real, intent(inout) :: param(:)
        real, intent(in) :: gradient(:)
        integer :: end_index
        end_index = self%start_index + size(param) - 1
        self%velocity(self%start_index:end_index) = &
            self%momentum*self%velocity(self%start_index:end_index) &
            - self%learning_rate*gradient
        param = param + self%velocity(self%start_index:end_index)
    end subroutine

    subroutine adam(self, g)
        class(sgd_t), intent(inout) :: self
        real, intent(in) :: g(:)
        integer :: end_index
        end_index = self%start_index + size(g) - 1
        self%m(self%start_index:end_index) = &
            self%beta1*self%m(self%start_index:end_index) + (1 - self%beta1)*g
        self%v(self%start_index:end_index) = &
            self%beta2*self%v(self%start_index:end_index) + (1 - self%beta2)*g**2
    end subroutine

    subroutine nested_component(obj, x)
        type(sgd_t), intent(inout) :: obj
        real, intent(in) :: x(:)
        obj%velocity(obj%r%s:obj%r%e) = 2*obj%velocity(obj%r%s:obj%r%e) + x
    end subroutine

    subroutine local_array(a, t, x)
        real, intent(inout) :: a(:,:)
        type(range_t), intent(in) :: t
        real, intent(in) :: x(:)
        a(t%s:t%e, t%s) = a(t%s:t%e, t%s)*3 - x
    end subroutine

    subroutine different_component(self)
        class(sgd_t), intent(inout) :: self
        self%velocity(self%a:self%b) = self%velocity(self%c:self%d) + 1
    end subroutine

    subroutine different_expression(self)
        class(sgd_t), intent(inout) :: self
        self%velocity(self%a:self%b) = self%velocity(self%a:self%b) &
            + self%velocity(self%a+1:self%b+1)
    end subroutine

    subroutine subscript_reads_target(self)
        type(sgd_t), intent(inout) :: self
        self%iv(self%iv(1):self%b) = self%iv(self%iv(1):self%b) + 10
    end subroutine

    subroutine pointer_component(self)
        type(sgd_t), intent(inout) :: self
        self%velocity(self%rp%s:self%rp%e) = self%velocity(self%rp%s:self%rp%e) + 1
    end subroutine

    subroutine pointer_dummy(a, r)
        real, intent(inout) :: a(:)
        type(range_t), pointer, intent(in) :: r
        a(r%s:r%e) = a(r%s:r%e)*2
    end subroutine

    subroutine shifted_section(a, t)
        real, intent(inout) :: a(:)
        type(range_t), intent(in) :: t
        a(t%s+1:t%e+1) = a(t%s:t%e) + 1
    end subroutine

end module

program array_section_assign_12
    use array_section_assign_12_mod
    implicit none
    integer, parameter :: n = 8
    type(sgd_t) :: opt
    type(range_t) :: t
    real :: param(4), param0(4), g(4), vel0(n), m0(n), v0(n), a(n,n), a0(n,n)
    real :: w(n), w0(n)
    integer :: i, j, iv0(n)
    type(range_t), target :: rt
    type(range_t), pointer :: rp

    allocate(opt%velocity(n), opt%m(n), opt%v(n), opt%iv(n))
    do i = 1, n
        opt%velocity(i) = real(i)
        opt%m(i) = real(2*i)
        opt%v(i) = real(i*i)
        opt%iv(i) = i
    end do
    do i = 1, 4
        param(i) = real(10*i)
        g(i) = real(i) - 2.5
    end do

    ! Type-bound sgd and adam updates
    opt%start_index = 3
    vel0 = opt%velocity
    param0 = param
    call opt%minimize(param, g)
    do i = 1, n
        if (i >= 3 .and. i <= 6) then
            if (abs(opt%velocity(i) - (0.25*vel0(i) - 0.5*g(i-2))) > 1e-6) error stop
            if (abs(param(i-2) - (param0(i-2) + opt%velocity(i))) > 1e-5) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do

    m0 = opt%m
    v0 = opt%v
    call opt%adam(g)
    do i = 1, n
        if (i >= 3 .and. i <= 6) then
            if (abs(opt%m(i) - (0.75*m0(i) + 0.25*g(i-2))) > 1e-5) error stop
            if (abs(opt%v(i) - (0.125*v0(i) + 0.875*g(i-2)**2)) > 1e-4) error stop
        else
            if (opt%m(i) /= m0(i) .or. opt%v(i) /= v0(i)) error stop
        end if
    end do

    ! Subscripts from nested components
    opt%r%s = 2
    opt%r%e = 5
    vel0 = opt%velocity
    call nested_component(opt, g)
    do i = 1, n
        if (i >= 2 .and. i <= 5) then
            if (abs(opt%velocity(i) - (2*vel0(i) + g(i-1))) > 1e-5) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do

    ! A local array subscripted by the components of another dummy
    do j = 1, n
        do i = 1, n
            a(i, j) = real(10*i + j)
        end do
    end do
    a0 = a
    t%s = 2
    t%e = 5
    call local_array(a, t, g)
    do i = 1, n
        if (i >= 2 .and. i <= 5) then
            if (abs(a(i,2) - (3*a0(i,2) - g(i-1))) > 1e-4) error stop
        else
            if (a(i,2) /= a0(i,2)) error stop
        end if
    end do
    a0(2:5,2) = a(2:5,2)
    if (any(a /= a0)) error stop

    ! Different components in the subscripts: overlapping shifted sections
    opt%a = 2
    opt%b = 6
    opt%c = 1
    opt%d = 5
    vel0 = opt%velocity
    call different_component(opt)
    do i = 1, n
        if (i >= 2 .and. i <= 6) then
            if (opt%velocity(i) /= vel0(i-1) + 1) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do

    ! A different subscript expression reading the next elements
    vel0 = opt%velocity
    call different_expression(opt)
    do i = 1, n
        if (i >= 2 .and. i <= 6) then
            if (opt%velocity(i) /= vel0(i) + vel0(i+1)) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do

    ! The subscript reads the first element of the target, which the
    ! assignment defines first
    opt%iv(1) = 1
    opt%b = 4
    iv0 = opt%iv
    call subscript_reads_target(opt)
    do i = 1, n
        if (i <= 4) then
            if (opt%iv(i) /= iv0(i) + 10) error stop
        else
            if (opt%iv(i) /= iv0(i)) error stop
        end if
    end do

    ! Subscripts reached through a pointer component or a pointer dummy
    rt%s = 3
    rt%e = 7
    opt%rp => rt
    vel0 = opt%velocity
    call pointer_component(opt)
    do i = 1, n
        if (i >= 3 .and. i <= 7) then
            if (opt%velocity(i) /= vel0(i) + 1) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do
    rp => rt
    vel0 = opt%velocity
    call pointer_dummy(opt%velocity, rp)
    do i = 1, n
        if (i >= 3 .and. i <= 7) then
            if (opt%velocity(i) /= 2*vel0(i)) error stop
        else
            if (opt%velocity(i) /= vel0(i)) error stop
        end if
    end do

    ! A shifted section of a plain array
    do i = 1, n
        w(i) = real(i*i)
    end do
    w0 = w
    t%s = 1
    t%e = 6
    call shifted_section(w, t)
    do i = 1, n
        if (i >= 2 .and. i <= 7) then
            if (w(i) /= w0(i-1) + 1) error stop
        else
            if (w(i) /= w0(i)) error stop
        end if
    end do

    print *, "ok"
end program
