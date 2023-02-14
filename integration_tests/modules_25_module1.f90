module tomlf_de_character
   use tomlf_de_tokenizer
   implicit none
   private

   public :: toml_character_tokenizer

   type, extends(toml_tokenizer) :: toml_character_tokenizer
      character(len=:), pointer :: conf
   contains
      procedure :: next_token
   end type toml_character_tokenizer


contains

subroutine next_token(de, dot_is_token)
   class(toml_character_tokenizer), intent(inout) :: de
   logical, intent(in) :: dot_is_token
end subroutine next_token

end module tomlf_de_character
