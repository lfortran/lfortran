! Test copy-in/copy-out for class member passed to type-bound procedure
module class_93_mod
  implicit none

  type :: token_t
    integer :: val = 0
  end type

  type :: lexer_t
    integer :: pos = 0
    type(token_t) :: tokens(3)
  contains
    procedure :: next => lexer_next
  end type

  type :: parser_t
    type(token_t) :: token
  end type

contains

  subroutine lexer_next(lexer, token)
    class(lexer_t), intent(inout) :: lexer
    type(token_t), intent(inout) :: token
    lexer%pos = lexer%pos + 1
    token = lexer%tokens(lexer%pos)
  end subroutine

  subroutine parse(parser, lexer)
    class(parser_t), intent(inout) :: parser
    class(lexer_t), intent(inout) :: lexer
    call lexer%next(parser%token)
  end subroutine

end module

program class_93
  use class_93_mod
  implicit none
  type(parser_t) :: parser
  type(lexer_t) :: lexer

  lexer%tokens(1)%val = 10
  lexer%tokens(2)%val = 20
  lexer%tokens(3)%val = 30

  call parse(parser, lexer)
  if (parser%token%val /= 10) error stop

  call parse(parser, lexer)
  if (parser%token%val /= 20) error stop

  print *, "PASSED"
end program
