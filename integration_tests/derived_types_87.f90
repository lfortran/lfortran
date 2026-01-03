module derived_types_87_mod
  implicit none

  type :: string_t
     character(len=:), allocatable :: s
  end type string_t

  type :: compile_command_t
        type(string_t) :: directory
        type(string_t), allocatable :: str_arr(:)
    end type compile_command_t    

    interface compile_command_t
        module procedure cct_new
    end interface compile_command_t

contains

    type(compile_command_t) function cct_new(directory) result(cct)
        character(len=*), intent(in) :: directory
    end function
end module derived_types_87_mod


program derived_types_87
    use derived_types_87_mod
    type(compile_command_t) :: cc
    cc = compile_command_t(directory = string_t("abc"), &
            str_arr = [string_t("def"), string_t("xyz")])
    if (cc%str_arr(1)%s /= "def") error stop
    if (cc%str_arr(2)%s /= "xyz") error stop
    if (cc%directory%s /= "abc") error stop
end program