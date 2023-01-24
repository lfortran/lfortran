module tomlf_de_tokenizer
    implicit none

    type :: toml_table
       logical :: inline = .false.
    end type toml_table

    type, abstract :: toml_tokenizer
       type(toml_table), pointer :: current => null()
       contains
       procedure(next_token), deferred :: next_token
    end type toml_tokenizer

    abstract interface
      subroutine next_token(de, dot_is_token)
         import :: toml_tokenizer
         class(toml_tokenizer), intent(inout) :: de
         logical, intent(in) :: dot_is_token
      end subroutine next_token
    end interface

    type, extends(toml_tokenizer) :: toml_tokenizer_
    end type toml_tokenizer_

    contains

    subroutine parse_select(de)

       class(toml_tokenizer), intent(inout), target :: de
       type(toml_table), pointer :: table

       nullify(table)
       de%current => table

    end subroutine parse_select

end module tomlf_de_tokenizer
