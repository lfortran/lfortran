! Kernels taken from the hottest assignments of two applications that rely
! on reallocation on assignment, Fiats and Formal (see README.md). Each
! kernel comes in two forms: the statement as the application writes it,
! and an explicit form that does the same work without relying on
! reallocation on assignment (its name ends in `_x`), so that the cost of
! the reallocation is the ratio between the two.
!
! Usage: realloc_lhs_apps <kernel> <n> <reps>
!
! Prints one line: "<kernel> <n> <reps> <seconds> <checksum>", as
! realloc_lhs does. The two forms of a kernel print the same checksum.
!
! The types have the structure of those of the applications, without their
! kind parameters: a tensor with an allocatable array of values, a pair of
! tensors, a mini-batch with an allocatable array of pairs, a string with a
! deferred-length component.
module realloc_apps_kernels
implicit none

! In realloc_lhs_ext.f90, for the placement padding of run_benchmark.py
interface
    subroutine ext_tick(k)
    integer, intent(inout) :: k
    end subroutine
end interface

! Fiats tensor_t
type :: tensor_t
    real, allocatable :: values_(:)
end type

! Fiats input_output_pair_t
type :: pair_t
    type(tensor_t) :: inputs_, expected_outputs_
end type

! Fiats mini_batch_t
type :: batch_t
    type(pair_t), allocatable :: pairs_(:)
end type

! julienne string_t
type :: string_t
    character(len=:), allocatable :: string_
end type

! Formal points_2D_t
type :: points_t
    real, allocatable :: values_(:,:)
end type

! Formal faces_to_centers_1D_t, 4th order
type :: interpolator_t
    real :: first_, final_
    real, allocatable :: upper_(:,:), inner_(:), lower_(:,:)
end type

contains

! Fiats tensor_s.f90:17, `tensor_values = self%values_`
pure function values(self) result(tensor_values)
    type(tensor_t), intent(in) :: self
    real, allocatable :: tensor_values(:)
    tensor_values = self%values_
end function

pure function values_x(self) result(tensor_values)
    type(tensor_t), intent(in) :: self
    real, allocatable :: tensor_values(:)
    allocate(tensor_values(size(self%values_)))
    tensor_values(:) = self%values_
end function

! Fiats tensor_s.f90:9, `tensor%values_ = values`
pure function tensor(v) result(t)
    real, intent(in) :: v(:)
    type(tensor_t) :: t
    t%values_ = v
end function

pure function tensor_x(v) result(t)
    real, intent(in) :: v(:)
    type(tensor_t) :: t
    allocate(t%values_(size(v)))
    t%values_(:) = v
end function

! Fiats input_output_pair_s.f90:9-10, a derived-type copy into each
! component (no array switch affects it)
elemental function pair(inputs, outputs) result(p)
    type(tensor_t), intent(in) :: inputs, outputs
    type(pair_t) :: p
    p%inputs_ = inputs
    p%expected_outputs_ = outputs
end function

! Fiats input_output_pair_s.f90:19, `my_inputs = self%inputs_`, a
! derived-type deep copy
elemental function inputs(self) result(my_inputs)
    type(pair_t), intent(in) :: self
    type(tensor_t) :: my_inputs
    my_inputs = self%inputs_
end function

! The same copy written component by component
elemental function inputs_x(self) result(my_inputs)
    type(pair_t), intent(in) :: self
    type(tensor_t) :: my_inputs
    allocate(my_inputs%values_(size(self%inputs_%values_)))
    my_inputs%values_(:) = self%inputs_%values_
end function

! Fiats mini_batch_s.f90:9, `mini_batch%input_output_pairs_ = input_output_pairs`
pure function batch(pairs) result(b)
    type(pair_t), intent(in) :: pairs(:)
    type(batch_t) :: b
    b%pairs_ = pairs
end function

pure function batch_x(pairs) result(b)
    type(pair_t), intent(in) :: pairs(:)
    type(batch_t) :: b
    allocate(b%pairs_(size(pairs)))
    b%pairs_(:) = pairs
end function

! Fiats mini_batch_s.f90:17, `my_input_output_pairs = self%input_output_pairs_`
pure function batch_pairs(self) result(my_pairs)
    type(batch_t), intent(in) :: self
    type(pair_t), allocatable :: my_pairs(:)
    my_pairs = self%pairs_
end function

pure function batch_pairs_x(self) result(my_pairs)
    type(batch_t), intent(in) :: self
    type(pair_t), allocatable :: my_pairs(:)
    allocate(my_pairs(size(self%pairs_)))
    my_pairs(:) = self%pairs_
end function

! Formal interpolator_1D_s.F90:96, center_values_extended
pure function centers(self, faces) result(centers_extended)
    type(interpolator_t), intent(in) :: self
    real, intent(in) :: faces(:)
    real, allocatable :: centers_extended(:)
    integer :: row
    associate(N => size(faces), inner_cols => size(self%inner_), &
            upper_cols => size(self%upper_, 2), lower_cols => size(self%lower_, 2), &
            inner_rows => size(faces) + 1 - (2 + size(self%upper_, 1) + size(self%lower_, 1)))
        centers_extended = &
            [ self%first_ * faces(1) &
            , matmul(self%upper_, faces(1:upper_cols)) &
            , [(dot_product(self%inner_, faces(row : row + inner_cols - 1)), row = 1, inner_rows)] &
            , matmul(self%lower_, faces(N-lower_cols+1:N)) &
            , self%final_ * faces(N) &
            ]
    end associate
end function

pure function centers_x(self, faces) result(centers_extended)
    type(interpolator_t), intent(in) :: self
    real, intent(in) :: faces(:)
    real, allocatable :: centers_extended(:)
    integer :: row
    associate(N => size(faces), inner_cols => size(self%inner_), &
            upper_cols => size(self%upper_, 2), lower_cols => size(self%lower_, 2), &
            inner_rows => size(faces) + 1 - (2 + size(self%upper_, 1) + size(self%lower_, 1)))
        allocate(centers_extended(size(faces) + 1))
        centers_extended(:) = &
            [ self%first_ * faces(1) &
            , matmul(self%upper_, faces(1:upper_cols)) &
            , [(dot_product(self%inner_, faces(row : row + inner_cols - 1)), row = 1, inner_rows)] &
            , matmul(self%lower_, faces(N-lower_cols+1:N)) &
            , self%final_ * faces(N) &
            ]
    end associate
end function

! julienne string_s.F90:581, `lhs%string_ = rhs` in the defined assignment
! of a string_t (a deferred-length character: no array switch affects it)
subroutine assign_string(lhs, rhs)
    type(string_t), intent(out) :: lhs
    character(len=*), intent(in) :: rhs
    lhs%string_ = rhs
end subroutine

subroutine assign_string_x(lhs, rhs)
    type(string_t), intent(out) :: lhs
    character(len=*), intent(in) :: rhs
    allocate(character(len=len(rhs)) :: lhs%string_)
    lhs%string_(:) = rhs
end subroutine

! The kernels. Each perturbs an input every iteration and adds a value of
! the result to the checksum `s`.

! values: the accessor, consumed as it is in the training loop
subroutine k_values(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(tensor_t) :: t
    integer :: i
    allocate(t%values_(n))
    t%values_ = 1
    s = 0
    do i = 1, reps
        t%values_(1) = real(mod(i, 7))
        s = s + sum(values(t))
    end do
end subroutine

subroutine k_values_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(tensor_t) :: t
    integer :: i
    allocate(t%values_(n))
    t%values_ = 1
    s = 0
    do i = 1, reps
        t%values_(1) = real(mod(i, 7))
        s = s + sum(values_x(t))
    end do
end subroutine

! tensor: the constructor, its result assigned to a variable
subroutine k_tensor(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(tensor_t) :: t
    real :: v(n)
    integer :: i
    v = 1
    s = 0
    do i = 1, reps
        v(1) = real(mod(i, 7))
        t = tensor(v)
        s = s + t%values_(1 + mod(i, n))
    end do
end subroutine

subroutine k_tensor_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(tensor_t) :: t
    real :: v(n)
    integer :: i
    v = 1
    s = 0
    do i = 1, reps
        v(1) = real(mod(i, 7))
        t = tensor_x(v)
        s = s + t%values_(1 + mod(i, n))
    end do
end subroutine

! The pairs of a mini-batch of n pairs of tensors of 2 values
subroutine make_pairs(n, p)
    integer, intent(in) :: n
    type(pair_t), intent(out) :: p(n)
    integer :: j
    do j = 1, n
        allocate(p(j)%inputs_%values_(2), p(j)%expected_outputs_%values_(1))
        p(j)%inputs_%values_ = [real(j), 2*real(j)]
        p(j)%expected_outputs_%values_ = [3*real(j)]
    end do
end subroutine

! batch: the mini-batch constructor and its accessor, as the training loop
! calls them for each mini-batch
subroutine k_batch(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(n)
    type(batch_t) :: b
    integer :: i
    call make_pairs(n, p)
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        b = batch(p)
        associate(q => batch_pairs(b))
            s = s + q(1 + mod(i, n))%inputs_%values_(1)
        end associate
    end do
end subroutine

subroutine k_batch_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(n)
    type(batch_t) :: b
    integer :: i
    call make_pairs(n, p)
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        b = batch_x(p)
        associate(q => batch_pairs_x(b))
            s = s + q(1 + mod(i, n))%inputs_%values_(1)
        end associate
    end do
end subroutine

! inputs: `inputs = input_output_pairs%inputs()` in the training loop,
! assigned to an allocated array. Every `period` iterations the mini-batch
! has one pair less, so the shape changes twice per period; Fiats changes it
! in 280 of 601200 iterations (period 4294). period 0: it never changes.
subroutine k_inputs(n, reps, period, s)
    integer, intent(in) :: n, reps, period
    real(8), intent(out) :: s
    type(pair_t) :: p(n)
    type(tensor_t), allocatable :: in(:)
    integer :: i, m
    call make_pairs(n, p)
    s = 0
    do i = 1, reps
        m = n
        if (period > 0) then
            if (mod(i, period) == 0) m = n - 1
        end if
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        in = inputs(p(1:m))
        s = s + in(1 + mod(i, m))%values_(1)
    end do
end subroutine

subroutine k_inputs_x(n, reps, period, s)
    integer, intent(in) :: n, reps, period
    real(8), intent(out) :: s
    type(pair_t) :: p(n)
    type(tensor_t), allocatable :: in(:)
    integer :: i, m
    call make_pairs(n, p)
    s = 0
    do i = 1, reps
        m = n
        if (period > 0) then
            if (mod(i, period) == 0) m = n - 1
        end if
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        if (allocated(in)) then
            if (size(in) /= m) deallocate(in)
        end if
        if (.not. allocated(in)) allocate(in(m))
        in(:) = inputs(p(1:m))
        s = s + in(1 + mod(i, m))%values_(1)
    end do
end subroutine

! deepcopy: the element accessor the training loop applies, alone; a
! derived-type copy, as a reference
subroutine k_deepcopy(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(1)
    type(tensor_t) :: t
    integer :: i
    call make_pairs(1, p)
    deallocate(p(1)%inputs_%values_)
    allocate(p(1)%inputs_%values_(n))
    p(1)%inputs_%values_ = 1
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        t = inputs(p(1))
        s = s + t%values_(1 + mod(i, n))
    end do
end subroutine

subroutine k_deepcopy_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(1)
    type(tensor_t) :: t
    integer :: i
    call make_pairs(1, p)
    deallocate(p(1)%inputs_%values_)
    allocate(p(1)%inputs_%values_(n))
    p(1)%inputs_%values_ = 1
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        t = inputs_x(p(1))
        s = s + t%values_(1 + mod(i, n))
    end do
end subroutine

! minibatches: `mini_batches = [(mini_batch_t(pairs(...)), b = 1, nb)]`
! in the epoch loop: allocated by the first iteration, then conforming. 10
! pairs in n bins.
subroutine k_minibatches(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(10)
    type(batch_t), allocatable :: mb(:)
    integer :: i, b, w
    call make_pairs(10, p)
    w = 10 / n
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        mb = [(batch(p((b-1)*w+1:b*w)), b = 1, n)]
        s = s + mb(1 + mod(i, n))%pairs_(1)%inputs_%values_(1)
    end do
end subroutine

subroutine k_minibatches_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(pair_t) :: p(10)
    type(batch_t), allocatable :: mb(:)
    integer :: i, b, w
    call make_pairs(10, p)
    w = 10 / n
    allocate(mb(n))
    s = 0
    do i = 1, reps
        p(1)%inputs_%values_(1) = real(mod(i, 7))
        mb(:) = [(batch(p((b-1)*w+1:b*w)), b = 1, n)]
        s = s + mb(1 + mod(i, n))%pairs_(1)%inputs_%values_(1)
    end do
end subroutine

! The 4th-order Formal interpolator for n-1 faces (n centers)
subroutine make_interpolator(f)
    type(interpolator_t), intent(out) :: f
    f%first_ = 1
    f%final_ = 1
    f%upper_ = reshape([-16., 70., 70., -14., 2.], [1, 5]) / 112
    f%inner_ = [-7., 63., 63., -7.] / 112
    f%lower_ = reshape([2., -14., 70., 70., -16.], [1, 5]) / 112
end subroutine

! centers: Formal's `centers_extended = [ ... matmul(...) ... ]`, a first
! assignment of the result, called in a loop
subroutine k_centers(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(interpolator_t) :: f
    real :: faces(n-1)
    integer :: i, j
    call make_interpolator(f)
    faces = [(real(j), j = 1, n-1)]
    s = 0
    do i = 1, reps
        faces(1) = real(mod(i, 7))
        associate(c => centers(f, faces))
            s = s + c(1 + mod(i, n))
        end associate
    end do
end subroutine

subroutine k_centers_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(interpolator_t) :: f
    real :: faces(n-1)
    integer :: i, j
    call make_interpolator(f)
    faces = [(real(j), j = 1, n-1)]
    s = 0
    do i = 1, reps
        faces(1) = real(mod(i, 7))
        associate(c => centers_x(f, faces))
            s = s + c(1 + mod(i, n))
        end associate
    end do
end subroutine

! harvest: `harvest = 2.*(harvest - 0.5)` (Fiats tests), always conforming
subroutine k_harvest(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: harvest(:)
    integer :: i
    allocate(harvest(n))
    harvest = 0.5
    s = 0
    do i = 1, reps
        harvest(1) = 0.5 + 1e-3*mod(i, 7)
        harvest = 2.*(harvest - 0.5) + 0.5
        s = s + harvest(1 + mod(i, n))
    end do
end subroutine

subroutine k_harvest_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: harvest(:)
    integer :: i
    allocate(harvest(n))
    harvest = 0.5
    s = 0
    do i = 1, reps
        harvest(1) = 0.5 + 1e-3*mod(i, 7)
        harvest(:) = 2.*(harvest - 0.5) + 0.5
        s = s + harvest(1 + mod(i, n))
    end do
end subroutine

! points: Formal's `divergence_2D%points_(1,1,1,1)%values_ = div_x + div_y`,
! a rank-2 component of an array element, always conforming (40 x n)
subroutine k_points(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(points_t) :: d(1)
    real :: dx(40, n), dy(40, n)
    integer :: i
    allocate(d(1)%values_(40, n))
    dx = 1
    dy = 2
    s = 0
    do i = 1, reps
        dx(1, 1) = real(mod(i, 7))
        d(1)%values_ = dx + dy
        s = s + d(1)%values_(1 + mod(i, 40), 1 + mod(i, n))
    end do
end subroutine

subroutine k_points_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(points_t) :: d(1)
    real :: dx(40, n), dy(40, n)
    integer :: i
    allocate(d(1)%values_(40, n))
    dx = 1
    dy = 2
    s = 0
    do i = 1, reps
        dx(1, 1) = real(mod(i, 7))
        d(1)%values_(:,:) = dx + dy
        s = s + d(1)%values_(1 + mod(i, 40), 1 + mod(i, n))
    end do
end subroutine

! string: the defined assignment of a string_t from a character of length
! n (a deferred-length character, as a reference)
subroutine k_string(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(string_t) :: t
    character(len=n) :: text
    integer :: i
    text = repeat("a", n)
    s = 0
    do i = 1, reps
        text(1:1) = achar(97 + mod(i, 7))
        call assign_string(t, text)
        s = s + iachar(t%string_(1:1)) + len(t%string_)
    end do
end subroutine

subroutine k_string_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(string_t) :: t
    character(len=n) :: text
    integer :: i
    text = repeat("a", n)
    s = 0
    do i = 1, reps
        text(1:1) = achar(97 + mod(i, 7))
        call assign_string_x(t, text)
        s = s + iachar(t%string_(1:1)) + len(t%string_)
    end do
end subroutine

end module

program realloc_lhs_apps
use realloc_apps_kernels
implicit none
character(len=32) :: name, arg
integer :: n, reps
integer(8) :: t0, t1, rate
real(8) :: s, t

call get_command_argument(1, name)
call get_command_argument(2, arg)
read (arg, *) n
call get_command_argument(3, arg)
read (arg, *) reps
call system_clock(t0, rate)
select case (trim(name))
case ("values")
    call k_values(n, reps, s)
case ("values_x")
    call k_values_x(n, reps, s)
case ("tensor")
    call k_tensor(n, reps, s)
case ("tensor_x")
    call k_tensor_x(n, reps, s)
case ("batch")
    call k_batch(n, reps, s)
case ("batch_x")
    call k_batch_x(n, reps, s)
case ("inputs")
    call k_inputs(n, reps, 4294, s)
case ("inputs_x")
    call k_inputs_x(n, reps, 4294, s)
case ("inputsc")
    call k_inputs(n, reps, 0, s)
case ("inputsc_x")
    call k_inputs_x(n, reps, 0, s)
case ("deepcopy")
    call k_deepcopy(n, reps, s)
case ("deepcopy_x")
    call k_deepcopy_x(n, reps, s)
case ("minibatches")
    call k_minibatches(n, reps, s)
case ("minibatches_x")
    call k_minibatches_x(n, reps, s)
case ("centers")
    call k_centers(n, reps, s)
case ("centers_x")
    call k_centers_x(n, reps, s)
case ("harvest")
    call k_harvest(n, reps, s)
case ("harvest_x")
    call k_harvest_x(n, reps, s)
case ("points")
    call k_points(n, reps, s)
case ("points_x")
    call k_points_x(n, reps, s)
case ("string")
    call k_string(n, reps, s)
case ("string_x")
    call k_string_x(n, reps, s)
case default
    error stop "unknown kernel"
end select
call system_clock(t1)
t = real(t1 - t0, 8) / real(rate, 8)
print "(a, 1x, i0, 1x, i0, 1x, es23.16, 1x, es24.16)", trim(name), n, reps, t, s
end program
