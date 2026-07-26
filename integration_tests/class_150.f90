module mod_class150_layer
  implicit none
  private
  public :: layer_type, dense_type, layer_container

  type, abstract :: layer_type
    real, allocatable :: o(:)
  contains
    procedure(forward_iface), deferred, pass(self) :: forward
  end type layer_type

  abstract interface
    subroutine forward_iface(self, x)
      import :: layer_type
      class(layer_type), intent(in out) :: self
      real, intent(in) :: x(:)
    end subroutine forward_iface
  end interface

  type, extends(layer_type) :: dense_type
    real, allocatable :: w(:,:)
    real, allocatable :: b(:)
  contains
    procedure, pass(self) :: forward => dense_forward
  end type dense_type

  interface dense_type
    module procedure :: make_dense
  end interface dense_type

  type layer_container
    class(layer_type), pointer :: p => null()
  end type layer_container

contains

  type(dense_type) function make_dense(val) result(layer)
    real, intent(in) :: val
    allocate(layer % w(2, 2), layer % b(2))
    layer % w = val
    layer % b = val
  end function make_dense

  subroutine dense_forward(self, x)
    class(dense_type), intent(in out) :: self
    real, intent(in) :: x(:)
    if (.not. allocated(self % o)) allocate(self % o(size(x)))
    self % o = matmul(transpose(self % w), x) + self % b
  end subroutine dense_forward

end module mod_class150_layer

module mod_class150_net
  use mod_class150_layer
  implicit none
  private
  public :: net_type, net_container, make_ensemble, ensemble_type, destroy_ensemble

  type net_type
    type(layer_container), allocatable :: layers(:)
  contains
    procedure, pass(self) :: output
  end type net_type

  type net_container
    class(net_type), pointer :: p => null()
  end type net_container

  type ensemble_type
    type(net_container), allocatable :: members(:)
  end type ensemble_type

contains

  function output(self, input) result(a)
    class(net_type), intent(in out) :: self
    real, intent(in) :: input(:)
    real, allocatable :: a(:)

    call self % layers(1) % p % forward(input)
    a = self % layers(1) % p % o
  end function output

  type(ensemble_type) function make_ensemble() result(ens)
    type(net_type) :: net

    allocate(ens % members(2))

    ! i=1: first model
    allocate(net % layers(1))
    allocate(net % layers(1) % p, source=dense_type(1.0))
    allocate(ens % members(1) % p, source=net)
    call destroy_net(net)

    ! i=2: second model
    allocate(net % layers(1))
    allocate(net % layers(1) % p, source=dense_type(2.0))
    allocate(ens % members(2) % p, source=net)
    call destroy_net(net)
  end function make_ensemble

  subroutine destroy_net(net)
    type(net_type), intent(in out) :: net
    integer :: i

    if (.not. allocated(net % layers)) return
    do i = 1, size(net % layers)
      if (associated(net % layers(i) % p)) then
        deallocate(net % layers(i) % p)
        nullify(net % layers(i) % p)
      end if
    end do
    deallocate(net % layers)
  end subroutine destroy_net

  subroutine destroy_ensemble(ens)
    type(ensemble_type), intent(in out) :: ens
    integer :: i, j

    if (.not. allocated(ens % members)) return
    do i = 1, size(ens % members)
      if (associated(ens % members(i) % p)) then
        if (allocated(ens % members(i) % p % layers)) then
          do j = 1, size(ens % members(i) % p % layers)
            if (associated(ens % members(i) % p % layers(j) % p)) then
              deallocate(ens % members(i) % p % layers(j) % p)
              nullify(ens % members(i) % p % layers(j) % p)
            end if
          end do
        end if
        deallocate(ens % members(i) % p)
        nullify(ens % members(i) % p)
      end if
    end do
    deallocate(ens % members)
  end subroutine destroy_ensemble

end module mod_class150_net

program class_150
  use mod_class150_layer
  use mod_class150_net
  implicit none

  type(ensemble_type) :: ens
  real, allocatable :: res(:)

  ens = make_ensemble()

  res = ens % members(1) % p % output([1.0, 2.0])
  print *, 'Member 1 output (i=1):', res
  if (any(res /= [4.0, 4.0])) error stop

  res = ens % members(2) % p % output([1.0, 2.0])
  print *, 'Member 2 output (i=2):', res
  if (any(res /= [8.0, 8.0])) error stop

  if (allocated(res)) deallocate(res)
  call destroy_ensemble(ens)

end program class_150
