module class_10_module
    implicit none
    private

    public :: toml_vector, toml_value, toml_node

    type :: toml_value
        character(len=:), allocatable :: key
    contains
        procedure :: match_key
    end type toml_value

    type :: toml_node
       !> TOML value payload
       class(toml_value), allocatable :: val
    end type toml_node

    !> Stores TOML values in a list of pointers
    type :: toml_vector
       !> Current number of stored TOML values
       integer :: n = 0
       !> List of TOML values
       type(toml_node), allocatable :: lst(:)
    contains
       procedure :: delete
       procedure :: fill_key
    end type toml_vector

contains

    subroutine fill_key(self, key, i)
        character(3), intent(in) :: key
        class(toml_vector), intent(inout) :: self
        integer, intent(in) :: i
        allocate(self%lst(i)%val)
        allocate(character(3) :: self%lst(i)%val%key)
        self%lst(i)%val%key = key
    end subroutine

    !> Compare raw key of TOML value to input key
    pure function match_key(self, key) result(match)
        !> TOML value instance.
        class(toml_value), intent(in) :: self
        !> TOML raw key to compare to
        ! integer, intent(in) :: key
        character(len=*), intent(in) :: key
        logical :: match
        if (self%key == key) then
            match = .true.
        else
            match = .false.
        end if
    end function match_key

    !> Delete TOML value at a given key
    subroutine delete(self, key)
        !> Instance of the structure
        class(toml_vector), intent(inout), target :: self
        !> Key to the TOML value
        character(len=*), intent(in) :: key
        integer :: i, idx
        idx = 0
        do i = 1, self%n
            if (self%lst(i)%val%match_key(key)) then
                idx = i
                exit
            end if
        end do
        do i = idx, self%n - 1
            self%lst(i)%val%key = self%lst(i+1)%val%key
        end do
        deallocate(self%lst(self%n)%val)
        self%n = self%n - 1
    end subroutine delete
end module class_10_module

program class_10_program
    use class_10_module
    implicit none
    type(toml_vector) :: t_vector

    t_vector%n = 5
    allocate(t_vector%lst(t_vector%n))

    call t_vector%fill_key("111", 1)
    call t_vector%fill_key("222", 2)
    call t_vector%fill_key("333", 3)
    call t_vector%fill_key("444", 4)
    call t_vector%fill_key("555", 5)

    call t_vector%delete("111")
    call t_vector%delete("333")

    if (t_vector%n /= 3) error stop
    if (t_vector%lst(1)%val%match_key("111")) error stop
    if (t_vector%lst(3)%val%match_key("333")) error stop
end program class_10_program
