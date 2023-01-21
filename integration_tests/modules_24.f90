module tomlf_de_tokenizer
    implicit none

    type :: toml_table
       logical :: inline = .false.
    end type toml_table

    type, abstract :: toml_tokenizer
       type(toml_table), pointer :: current => null()
    end type toml_tokenizer

    contains

    subroutine parse_select(de)

       class(toml_tokenizer), intent(inout), target :: de
       type(toml_table), pointer :: table

       nullify(table)
       de%current => table

    end subroutine parse_select

end module tomlf_de_tokenizer

program modules_24
use tomlf_de_tokenizer
implicit none
class(toml_tokenizer), pointer :: tokenizer
call parse_select(tokenizer)
! TODO: Uncomment and fix the ASR verify pass
! error. Struct() is pointing to tomlf_de_tokenizer's
! toml_table but it should instead point to the imported
! toml_table
! print *, tokenizer%current
end program
