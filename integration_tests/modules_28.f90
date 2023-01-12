module tomlf_de_tokenizer
   implicit none

   type :: toml_table

      logical :: inline = .false.

   end type toml_table

   type, abstract :: toml_tokenizer
      type(toml_table), pointer :: current => null()
   end type toml_tokenizer


end module tomlf_de_tokenizer
