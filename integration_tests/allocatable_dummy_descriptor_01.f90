module m_strings
  implicit none
  type :: string_t
    character(:), allocatable :: s
  end type string_t
end module m_strings

module m_toml
  use iso_fortran_env, only: int64
  use m_strings, only: string_t
  implicit none

  type :: toml_key
    character(:), allocatable :: key
  end type toml_key

  type :: toml_table
    character(:), allocatable :: name
    character(:), allocatable :: version
    logical :: module_naming = .false.
    character(:), allocatable :: module_prefix

    type(toml_table), pointer :: sources => null()
    type(toml_table), pointer :: src1 => null()

    character(:), allocatable :: file_name
    character(:), allocatable :: exe_name
    integer(int64) :: digest = 0_int64
    character(:), allocatable :: unit_scope
    character(:), allocatable :: unit_type
    type(string_t), allocatable :: modules_provided(:)
  contains
    procedure :: get_keys
  end type toml_table

contains

  subroutine get_keys(self, keys)
    class(toml_table), intent(in) :: self
    type(toml_key), allocatable, intent(out) :: keys(:)

    if (associated(self%src1)) then
      allocate(keys(1))
      keys(1)%key = 'src_1'
    else if (associated(self%sources)) then
      allocate(keys(1))
      keys(1)%key = 'sources'
    else
      allocate(keys(2))
      keys(1)%key = 'name'
      keys(2)%key = 'version'
    end if
  end subroutine get_keys

  subroutine get_value_table(table, key, ptr)
    type(toml_table), intent(inout) :: table
    type(toml_key), intent(in) :: key
    type(toml_table), pointer :: ptr

    nullify(ptr)
    if (key%key == 'sources') then
      ptr => table%sources
    else if (key%key == 'src_1') then
      ptr => table%src1
    end if
  end subroutine get_value_table

  subroutine get_value_table_char(table, key, val)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    character(:), allocatable, intent(out) :: val

    select case (key)
    case ('name')
      if (allocated(table%name)) val = table%name
    case ('version')
      if (allocated(table%version)) val = table%version
    case ('module-prefix')
      if (allocated(table%module_prefix)) val = table%module_prefix
    case ('file-name')
      if (allocated(table%file_name)) val = table%file_name
    case ('exe-name')
      if (allocated(table%exe_name)) val = table%exe_name
    case ('unit-scope')
      if (allocated(table%unit_scope)) val = table%unit_scope
    case ('unit-type')
      if (allocated(table%unit_type)) val = table%unit_type
    end select
  end subroutine get_value_table_char

  subroutine get_value_table_logical(table, key, val)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    logical, intent(out) :: val

    if (key == 'module-naming') val = table%module_naming
  end subroutine get_value_table_logical

  subroutine get_value_table_int64(table, key, val)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    integer(int64), intent(out) :: val

    if (key == 'digest') val = table%digest
  end subroutine get_value_table_int64

  subroutine get_list(table, key, list)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    type(string_t), allocatable, intent(out) :: list(:)

    integer :: i
    character(:), allocatable :: tmp

    if (key /= 'modules-provided') return
    if (.not. allocated(table%modules_provided)) return

    allocate(list(size(table%modules_provided)))
    do i = 1, size(table%modules_provided)
      if (allocated(tmp)) deallocate(tmp)
      if (allocated(table%modules_provided(i)%s)) then
        tmp = table%modules_provided(i)%s
        call move_alloc(tmp, list(i)%s)
      end if
    end do
  end subroutine get_list

end module m_toml

module m_model
  use iso_fortran_env, only: int64
  use m_strings, only: string_t
  use m_toml, only: toml_table, toml_key, get_value_table, get_value_table_char, &
                    get_value_table_logical, get_value_table_int64, get_list
  implicit none

  integer, parameter :: FPM_SCOPE_LIB = 1
  integer, parameter :: FPM_UNIT_MODULE = 2

  type :: srcfile_t
    character(:), allocatable :: file_name
    character(:), allocatable :: exe_name
    integer :: unit_scope = 0
    type(string_t), allocatable :: modules_provided(:)
    integer :: unit_type = 0
    integer(int64) :: digest
  contains
    procedure :: load_from_toml => srcfile_load_from_toml
  end type srcfile_t

  type :: package_t
    character(:), allocatable :: name
    type(srcfile_t), allocatable :: sources(:)
    logical :: enforce_module_names = .false.
    type(string_t) :: module_prefix
  contains
    procedure :: load_from_toml => package_load_from_toml
  end type package_t

contains

  subroutine srcfile_load_from_toml(self, table)
    class(srcfile_t), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    character(len=:), allocatable :: flag

    call get_value_table_char(table, 'file-name', self%file_name)
    call get_value_table_char(table, 'exe-name', self%exe_name)
    call get_value_table_int64(table, 'digest', self%digest)
    call get_value_table_char(table, 'unit-scope', flag)
    if (allocated(flag)) self%unit_scope = FPM_SCOPE_LIB
    call get_value_table_char(table, 'unit-type', flag)
    if (allocated(flag)) self%unit_type = FPM_UNIT_MODULE

    call get_list(table, 'modules-provided', self%modules_provided)
  end subroutine srcfile_load_from_toml

  subroutine package_load_from_toml(self, table)
    class(package_t), intent(inout) :: self
    type(toml_table), intent(inout) :: table

    integer :: ii, jj
    type(toml_key), allocatable :: keys(:), src_keys(:)
    type(toml_table), pointer :: ptr_sources, ptr

    call get_value_table_char(table, 'name', self%name)
    call get_value_table_logical(table, 'module-naming', self%enforce_module_names)
    call get_value_table_char(table, 'module-prefix', self%module_prefix%s)

    call table%get_keys(keys)

    do ii = 1, size(keys)
      if (keys(ii)%key == 'sources') then
        call get_value_table(table, keys(ii), ptr_sources)
        call ptr_sources%get_keys(src_keys)
        allocate(self%sources(size(src_keys)))
        do jj = 1, size(src_keys)
          call get_value_table(ptr_sources, src_keys(jj), ptr)
          call self%sources(jj)%load_from_toml(ptr)
        end do
      end if
    end do
  end subroutine package_load_from_toml

end module m_model

program allocatable_dummy_descriptor_01
  use iso_fortran_env, only: int64
  use m_toml, only: toml_table
  use m_model, only: package_t
  implicit none

  type(toml_table), target :: root, sources, src1
  type(package_t) :: pkg

  root%name = 'orderpack'
  root%version = '0.1.0'
  root%module_prefix = ''
  root%module_naming = .false.

  sources%src1 => src1
  root%sources => sources

  src1%file_name = 'build/dependencies/orderpack/src/M_valnth.f90'
  src1%digest = 2662523002405134329_int64
  src1%unit_scope = 'FPM_SCOPE_LIB'
  src1%unit_type = 'FPM_UNIT_MODULE'
  allocate(src1%modules_provided(1))
  src1%modules_provided(1)%s = 'm_valnth'

  call pkg%load_from_toml(root)

  if (.not. allocated(pkg%sources)) error stop
  if (size(pkg%sources) /= 1) error stop
  if (.not. allocated(pkg%sources(1)%modules_provided)) error stop
  if (size(pkg%sources(1)%modules_provided) /= 1) error stop
  if (.not. allocated(pkg%sources(1)%modules_provided(1)%s)) error stop
  if (pkg%sources(1)%modules_provided(1)%s /= 'm_valnth') error stop
end program allocatable_dummy_descriptor_01
