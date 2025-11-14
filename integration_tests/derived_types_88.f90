module derived_types_88_mod
  implicit none

  type :: string_t
     character(len=:), allocatable :: s
  end type string_t

  type :: compile_command_t
        character(:), allocatable :: directory
        type(string_t), allocatable :: str_arr(:)
    end type compile_command_t    

contains

    subroutine cct_new(cc)
        type(compile_command_t), intent(inout) :: cc
        call cct_inside()
    contains 
        subroutine cct_inside()
            if (cc%directory /= "Home") error stop
            if (cc%str_arr(1)%s /= "Hello") error stop
            if (cc%str_arr(2)%s /= "World") error stop
        end subroutine
    end subroutine
end module derived_types_88_mod


program derived_types_88
    use derived_types_88_mod
    type(compile_command_t) :: cc
    cc%directory = "Home" 
    cc%str_arr = [string_t("Hello"), string_t("World")]
    call cct_new(cc)
end program