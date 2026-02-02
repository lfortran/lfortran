module m_types
  implicit none

  type, abstract :: base_t
  contains
    procedure(load_i), deferred :: load
  end type base_t

  abstract interface
    subroutine load_i(self)
      import :: base_t
      class(base_t), intent(inout) :: self
    end subroutine load_i
  end interface

  type :: srcfile_t
    character(:), allocatable :: file_name
  end type srcfile_t

  type, extends(base_t) :: package_t
    type(srcfile_t), allocatable :: sources(:)
  contains
    procedure :: load => package_load
  end type package_t

contains

  subroutine package_load(self)
    class(package_t), intent(inout) :: self
    if (allocated(self%sources)) deallocate(self%sources)
    allocate(self%sources(1))
    self%sources(1)%file_name = 'file1.f90'
  end subroutine package_load

end module m_types

program allocatable_polymorphic_mold_01
  use m_types
  implicit none

  type(package_t) :: pkg
  class(base_t), allocatable :: copy

  allocate(pkg%sources(1))
  pkg%sources(1)%file_name = 'init'

  allocate(copy, mold=pkg)
  call copy%load()

  select type (copy)
  type is (package_t)
    if (.not. allocated(copy%sources)) error stop
    if (.not. allocated(copy%sources(1)%file_name)) error stop
    if (copy%sources(1)%file_name /= 'file1.f90') error stop
  class default
    error stop
  end select
end program allocatable_polymorphic_mold_01
