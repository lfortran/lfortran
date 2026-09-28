# Namespaces for Modules (`use, namespace`)

Status: **design draft for an LFortran prototype**. This is an LFortran
extension that implements
[j3-fortran/fortran_proposals#1](https://github.com/j3-fortran/fortran_proposals/issues/1).
It is not (yet) part of any Fortran standard.

## Summary

```fortran
use, namespace :: utils                 ! import the module as a namespace
use, namespace :: np => numpy           ! ... under a different local name
use, intrinsic, namespace :: env => iso_fortran_env

call utils%savetxt("a.txt", x)          ! members are reached with "%"
y = np%sin(np%pi)
real(env%real64) :: z
type(np%ndarray_t) :: a
```

`use, namespace :: M` puts exactly one name into the scope: the namespace
`M`, or the local name given with `L => M`. Every public entity of the module
is then reachable as `M%name`. No member name is made accessible without the
qualifier, so the scope's namespace is not polluted. This fills the one gap
in Fortran's module import facilities compared to Python:

| Python                 | Fortran today            | With this feature           |
|------------------------|--------------------------|-----------------------------|
| `from A import x`      | `use A, only: x`         | (unchanged)                 |
| `from A import x as y` | `use A, only: y => x`    | (unchanged)                 |
| `from A import *`      | `use A`                  | (unchanged)                 |
| `import A`             | N/A                      | `use, namespace :: A`       |
| `import A as B`        | N/A                      | `use, namespace :: B => A`  |

## History and sources

The design is based on everything in the issue discussion and on the
committee papers:

* J3/19-246 and J3/20-108 (Čertík): the original proposal,
  `use, namespace :: A` / `use, namespace :: B => A`, accessed with `%`.
* J3 plenary, 2020-02-26 (summary in comment 26 of the issue). Feedback
  included: "no big difference between `module%foo` and `module_foo`",
  "prefers rename on use", "a solution in search of a problem", "use
  Haskell a lot which has this feature and it is useful", "this should work
  for derived types and other entities", "the main attraction is for the
  compiler to track where an entity comes from", "this promotes
  collaboration", a dislike of `%` with `:` suggested (colon then rejected
  as unworkable), and a question about direct qualification if nesting
  were allowed.
* The issue discussion (2019–2025): syntax alternatives (`use, namespace`,
  `use A, only:`, `use A, as: B`, `use namespace A`, `import`, `with`,
  `decorate`/`prefix(ed)`, `namespace(B)`), accessors (`%`, `::`, others),
  transitivity (`b%a%x`), submodules, operators, generics.
* J3/23-196r1 (Cohen): "remote access" with `%%`, where module names would
  flow through use and host association "for the purpose of `%%` only". It
  asks whether remote access should bypass `ONLY` ("current thinking is
  that it should not be allowed") and suggests
  `USE modulename, NAMESPACE:` for a namespace-only import.
* J3/25-119r1 (Cohen, data subgroup): recommends not doing US13 for
  Fortran 2028. The main objection is that "module names are not passed
  through subsequent use or host association, which means it would only
  work in a scope that directly uses the module. That would make the feature
  useless." Changing that for the module names themselves would be
  incompatible. The paper also notes that `module1%%var` is no shorter than
  a rename to `module1_var`. A `namespace ... end namespace` construct was
  floated in the 2025 discussion (comment 105).

The prototype exists to answer the question raised in comments 54 and 92:
"I think we need to implement this in a compiler and actually use it."

## Decisions

| #  | Question | Decision |
|----|----------|----------|
| D1 | Syntax of a namespace-only import | `use, namespace :: [L =>] M` |
| D2 | Does a plain `use M` also make `M%x` available? | No |
| D3 | Accessor | `%` |
| D4 | Do namespaces pass through USE association? | **Open**, see the D4 section below |
| D5 | Name of a parent component when extending `ns%t` | The type's name in its module (`t`) |
| D6 | May `IMPORT` in an interface body name a namespace? | Yes |
| D7 | Two namespace imports of the same module under the same local name | Allowed (redundant) |

### D1: syntax — `use, namespace :: [L =>] M`

The alternatives, from the discussion:

* **`use, namespace :: A` / `use, namespace :: B => A`** (chosen). It
  follows the existing `use, intrinsic :: iso_fortran_env` form and combines
  with it: `use, intrinsic, namespace :: env => iso_fortran_env`. Because of
  the commas and the double colon it is unambiguous in fixed form (comment
  55). Renaming uses the familiar `=>` in the familiar direction.
* `use A, only:`, the Chapel style (comments 65, 66, 93). The statement is
  already valid Fortran 2003 and means "import nothing", so it would not be
  obvious to a reader that it now does something (comments 67, 68).
  Renaming would need new syntax (`use B => A, only:`).
* `use A, as: B` / `use A, as is` (comments 59, 64, 88). Reads like
  Python's `import numpy as np`. It would be a third form of the USE
  statement, and `as is` can be confused with "as given" (comment 99).
* `use, namespace(B) :: A`, `use, prefixed(B) :: A`, `use, decorate(name=B)
  :: A` (comments 79, 97, 98). The alias sits in an attribute argument
  instead of using `=>`.
* `use namespace A` / `use module A` (comment 54). Ambiguous in fixed form:
  `USENAMESPACEMYLIB` (comment 55).
* `import A` / `with A` / a new statement keyword (comments 49, 52, 74).
  `import` already has a different meaning in Fortran, and `with` does not
  suggest the meaning to a new reader.

### D2: a plain `use M` does not create a namespace

Klausler's model (comments 89–93): every USE statement would also make the
module name a qualifier, so `use A, only: x` would give `x` plus `A%...`.
That would be backward compatible for the scope that has the USE, because
F2023 19.3.1 already forbids a local identifier that equals a global
identifier used in the same scope (`use m; integer :: m` is invalid today).
The prototype keeps the conservative choice, so a plain USE behaves exactly
as today. The only way to create a namespace is the new syntax, which makes
the feature purely additive and the easiest to argue for. The model can be
added later without breaking anything.

### D3: accessor `%`

* `%` (chosen). It reads like a component reference and IDEs already
  understand it. A namespace is a local identifier of the same class as a
  variable, so a namespace and a variable with the same name can never both
  be accessible in one scope: `ns%x` is never ambiguous (see
  Name resolution below).
* `%%` (J3/23-196r1). Keeps module access visually distinct and allows a
  variable and a module with the same name to coexist. That matters only if
  module names flow implicitly into scopes that did not ask for them, which
  D2 and D4 avoid.
* `::` (comments 80, 86, 87). Ambiguous with array sections `a(1::2)` and
  with specification statements (J3/23-196r1). Rejected by the committee.

### D5: parent component name

`type, extends(ns%t) :: u` gives `u` a parent component named `t` (the name
of the type in the module that defines it), because the qualified name
`ns%t` is not a name. Test: `namespace_modules_05`.

### D6: `IMPORT` of a namespace

An interface body does not access its host by host association, so a
namespace of the host is not accessible there. `import :: ns` (and
`import, all`) makes it accessible, just like any other host entity. A
`use, namespace` statement inside the interface body also works. Tests:
`namespace_modules_15` (valid) and `errors/namespace_modules_26` (missing
import).

### D7: repeated namespace imports

`use, namespace :: s => m` may appear twice for the same module, just as the
same USE statement may be repeated. Giving the same local name to two
*different* modules is an error (`errors/namespace_modules_10`).

## Syntax

The USE statement (F2023 R1409) gets a third form:

```
use-stmt  is  USE [[, module-nature] ::] module-name [, rename-list]
          or  USE [[, module-nature] ::] module-name , ONLY : [only-list]
          or  USE , namespace-modifier-list :: [local-namespace-name =>] module-name

namespace-modifier  is  NAMESPACE
                    or  module-nature          ! INTRINSIC or NON_INTRINSIC
```

Constraints:

* C1: `NAMESPACE` appears exactly once in a *namespace-modifier-list*, and
  at most one *module-nature* appears. They may be in either order
  (`use, intrinsic, namespace ::` and `use, namespace, intrinsic ::`).
  (`errors/namespace_modules_15`)
* C2: The `::` is required (as it is whenever a *module-nature* is given).
  (`errors/namespace_modules_16`)
* C3: A namespace import has neither a *rename-list* nor `ONLY`. A USE
  statement that is not a namespace import cannot rename the module
  (`use :: b => a` is invalid). Entities cannot be renamed "inside" a
  namespace (comments 19–25: like the components of a derived type, the
  members of a namespace keep their names). (`errors/namespace_modules_02`,
  `_03`, `_17`)
* C4: A module shall not import itself as a namespace (existing rule for
  USE). (`errors/namespace_modules_22`)

The namespace-qualified name (the accessor) is:

```
namespace-qualified-name  is  namespace-name % name
```

If D4 allows namespaces to be members of namespaces, `name` may itself be
a namespace, giving chains such as `std%linalg%solve`.

A namespace-qualified name may be followed by the usual part references,
section subscripts, substring ranges, component references and actual
argument lists, exactly as the member's unqualified name could be. Blanks
are allowed around `%` and names are case insensitive, as for component
references (`namespace_modules_22`). In fixed form, blanks are
insignificant as usual (`namespace_modules_21`).

## Semantics

### The namespace entity

`use, namespace :: L => M` declares `L` (or `M` if there is no rename) as a
**namespace**, a new kind of class (1) local entity in the scoping unit
(F2023 19.3.1). It identifies the module `M`. A namespace is not a data
object, procedure, type or generic. It can only appear:

* as the leftmost part of a namespace-qualified name `L%member`;
* in an `IMPORT` statement;
* in accessibility statements and in USE statements of *other* scopes, if
  D4 allows it.

In particular it cannot be referenced on its own (`print *, L`), used as an
actual argument, assigned to, or subscripted
(`errors/namespace_modules_06`, `_07`, `_08`).

Like any class (1) local identifier, it must not be the same as another
local identifier of the scope (`errors/namespace_modules_09`). If a
namespace and a use-associated entity have the same local name, the name
must not be referenced (`errors/namespace_modules_11`), the same rule as
for two use-associated entities with the same name (F2023 14.2.2).

The module name `M` is still a global identifier used in the scope, even
when the namespace is renamed. By the existing F2023 19.3.1 rule, the scope
cannot declare a local entity named `M` (`errors/namespace_modules_23`).

### What `L%name` designates

`L%name` designates the entity that `use M, only: name` would make
accessible: any public entity of `M`, including entities that `M` itself
accesses by use association and makes public. It designates *the same
entity* as any other route to it (ordinary USE, another namespace for the
same module, host association). Every attribute of the entity applies
unchanged. For example, `PROTECTED` still forbids modifying it outside its
module (`errors/namespace_modules_12`), a named constant is still a
constant (`errors/namespace_modules_27`), and a private entity is not
accessible (`errors/namespace_modules_05`). A name that is not an entity of
the module is an error (`errors/namespace_modules_04`).

A namespace import makes none of the member names accessible without
qualification (`errors/namespace_modules_01`). Local entities may therefore
have the same names as members, and the two stay distinct
(`namespace_modules_01`, `_16`).

### Where `L%name` may appear

Anywhere the member's name could be *referenced*:

| Kind of member | Allowed uses (with tests) |
|---|---|
| Variable | Expressions; assignment targets; part references `L%a(i)`, `L%a(2:4)`, `L%s(1:3)`, `L%obj%comp`; actual arguments; `allocate`/`deallocate`; both sides of pointer assignment; `nullify`; `associated`, `allocated`, `size`, ...; input/output items, unit and format specifiers, implied-DO lists; ASSOCIATE and SELECT TYPE selectors (`_01`, `_03`, `_17`, `_18`, `_20`) |
| Named constant, enumerator | All constant expressions: kind selectors `real(L%dp)`, array bounds, character lengths, `parameter` initialization, `case` selectors, kind arguments of intrinsics (`_06`, `_07`) |
| Procedure | `call L%s(...)`, function references, keyword and optional arguments, actual arguments, procedure pointer targets, `associated(p, L%f)` (`_01`, `_09`, `_19`) |
| Generic interface | Generic resolution over the module's specifics, including a generic with the same name as a type (`_08`) |
| Abstract interface | `procedure(L%iface)` declarations of dummy procedures and procedure pointers (`_09`) |
| Derived type | `type(L%t)`, `class(L%t)`, structure constructors `L%t(...)`, array constructor type-spec `[L%t :: ...]`, `allocate(L%t :: x)`, `type is (L%t)`, `class is (L%t)`, `extends(L%t)` (`_04`, `_05`, `_14`, `_20`) |
| Namespace (only if D4 allows it) | `L%inner%x` (`export_01`, `export_03`) |

Type-bound procedures, type-bound generics, type-bound operators and
type-bound assignment belong to the type. They work on objects whose type
was accessed through a namespace, with no extra import
(`namespace_modules_04`, `_14`).

Not allowed:

* Declaring an entity with a qualified name, e.g. `integer :: L%y`
  (`errors/namespace_modules_25`).
* A qualified name as a DO variable. The syntax requires a variable name,
  as it does for `do obj%i = ...` today (`errors/namespace_modules_19`).
* A qualified name as the kind parameter of a literal constant, e.g.
  `1.5_L%dp`. The syntax requires a digit string or a named-constant name.
  Write `real(1.5, L%dp)` instead (`errors/namespace_modules_20`).
* A derived type used as if it were a data object, e.g. `L%t%x`
  (`errors/namespace_modules_24`).

### Operators, assignment and defined input/output

Non-type-bound defined operators (`operator(.dot.)`, `operator(+)`),
defined assignment (`assignment(=)`) and non-type-bound defined
input/output (`write(formatted)`) have no name that could be qualified
(comment 7 asked whether `x .cross_product. y` would become
`x foo::operator(.cross_product.) y`). A namespace import does **not** make
them accessible (`errors/namespace_modules_18`). They are imported
explicitly next to the namespace:

```fortran
use, namespace :: v => vectors
use vectors, only: operator(.dot.), assignment(=)
```

(`namespace_modules_14`). Type-bound versions need no import (see above).

### Generic interfaces are not extended

With an ordinary USE, a local `interface gen` extends the use-associated
generic `gen`. `L%gen` is not a local name, so a local generic with the
same name is a separate generic and does not extend `L%gen`
(`namespace_modules_23`, `errors/namespace_modules_30`). Comment 7 asked
about this interaction.

### Name resolution

The first name of `a%b` is resolved like any local identifier (local
entity, then host-associated entity, and so on). If it resolves to a
namespace, `a%b` is a namespace-qualified name. Otherwise it is a
component reference or type-bound procedure reference, as today. A
namespace and a variable with the same name cannot both be accessible in a
scope (see above), so this is unambiguous.

### Scoping: host association, BLOCK, submodules, interface bodies

A namespace is a local entity, so it is accessible by host association like
any other:

* in internal procedures and module procedures, and in BLOCK constructs,
  of the scope that imported it (`namespace_modules_11`);
* in submodules of a module that imported it (`namespace_modules_13`;
  comments 60, 62, 72, 73). A submodule can also import its own
  namespaces. Submodules are never visible through a namespace;
  `M%submod%...` does not exist (comment 73);
* in interface bodies, only through `IMPORT` (D6, `namespace_modules_15`,
  `errors/namespace_modules_26`).

A local entity of an inner scope with the same name hides the host's
namespace, as for any host-associated entity (`namespace_modules_11`,
`errors/namespace_modules_21`). A `use, namespace` statement is allowed
wherever a USE statement is allowed: in a program, module, submodule,
subroutine, function, BLOCK construct or interface body
(`namespace_modules_12`, `_15`).

This addresses the host-association half of the J3/25-119r1 objection:
namespace names are class (1) local identifiers, so they are host
associated.

### Combining with ordinary USE statements

A scope may freely mix a namespace import of `M` with ordinary USE
statements of `M`. The same module may also be imported under several
namespace names. All routes designate the same entities
(`namespace_modules_10`; comments 22–23):

```fortran
use, namespace :: sm1 => some_module
use, namespace :: sm2 => some_module
use some_module, only: t1 => thing1   ! t1, sm1%thing1 and sm2%thing1 are the same
```

Renames in an ordinary USE do not affect the namespace: above, `sm1%t1`
does not exist (comments 18–21).

### Intrinsic modules

Intrinsic modules can be imported as namespaces
(`use, intrinsic, namespace :: env => iso_fortran_env`, with
`env%real64`, `c%c_loc`, `c%c_ptr`, ...). See `namespace_modules_07`.

## D4: namespaces and USE association (open)

This is the one decision that is still open. Consider:

```fortran
module a
    integer :: x = 1
end module

module b
    use, namespace :: a        ! the namespace "a" is a local entity of b
    integer :: y = 2
end module
```

What can users of `b` do with `a`? Host association is not in question:
inside `b`, its procedures and its submodules, `a%x` always works. The
options:

### Option A: a namespace is an ordinary entity (public by default)

A namespace is a class (1) local entity of the module like any other, so it
follows the usual rules:

* It has the module's default accessibility and can be named in
  `public`/`private` statements.
* `use b` makes `a` accessible, so `a%x` works. `use b, only: a` and
  `use b, only: aa => a` work too.
* With `use, namespace :: b`, `b%a%x` works (a namespace is a member like
  any other).

```fortran
program p1
    use b                   ! brings y and the namespace a
    print *, a%x, y
end program

program p2
    use, namespace :: nb => b
    print *, nb%a%x, nb%y   ! chains of namespaces
end program

program p3
    use b, only: aa => a    ! namespaces can be listed and renamed
    print *, aa%x
end program
```

Pros:

* No new rules: a namespace behaves like every other local entity, and
  existing rules cover accessibility, ONLY, renaming and conflicts.
* It answers the J3/25-119r1 objection directly. The *namespace name* (not
  the module name) passes through use association, so the feature is not
  limited to the scope that has the USE statement. There is no
  incompatibility, because only the new syntax creates namespaces (D2). The
  incompatibility that Everythingfunctional raised in comment 107
  (`module b; use a; end` then `use b; integer a`) cannot happen: a plain
  `use a` in `b` creates nothing.
* Facade modules become possible: a library can export
  `std%linalg%solve`, `std%stats%mean` (`namespace_modules_export_01`).
  Comments 45–47 (Klausler: "would it be transitive?"; Čertík: yes;
  septcolor showed that Julia behaves this way) and comment 71 (Haskell
  can export module aliases, which "lets one compose a new module using
  names imported from others") ask for exactly this. Comment 110 gives
  `b%a%x` as the example.

Cons:

* `use b` now also brings in `a`, which can clash with the user's own
  names. The feature exists to reduce that kind of pollution. Library
  authors must write `private :: a` to avoid exporting namespaces they only
  use internally, and most modules that import namespaces probably want
  that.
* The .mod file must record namespaces.

Tests: `export_01`, `export_02`, `export_03` and `export_04` are valid;
`errors/namespace_modules_28` and `_29` are errors.

### Option A2: an ordinary entity, but private unless declared PUBLIC

As option A, except that a namespace is exported only if it is explicitly
named in a `public` statement, whatever the module's default accessibility.

```fortran
module b
    use, namespace :: a
    public :: a              ! required to export the namespace
end module
```

Pros: everything in option A, without the accidental pollution. A module
that uses a namespace internally exports nothing new. Exporting is
explicit, like building a facade.

Cons: a new special case in the accessibility rules (default accessibility
does not apply to namespaces). Plain `public` at module level does not
export namespaces, which may surprise.

Tests: `export_03` and `export_04` are valid; `export_01` and `export_02`
are errors (no `public :: linalg`, `public :: a`).
`errors/namespace_modules_28` and `_29` are errors.

### Option B: namespaces are members, but not use associated

`use, namespace :: b` gives `b%a%x`, but `use b` does not make `a`
accessible, and `a` cannot appear in an ONLY list.

```fortran
use, namespace :: b
print *, b%a%x      ! OK
use b
print *, a%x        ! error: a is not accessible
```

Pros: `use b` never pulls namespaces into the user's scope, yet facades
still work through qualification (`std%linalg%solve`). This matches the
narrow reading of comments 45–47 and 110.

Cons: namespaces become a second kind of module member with their own
rules (a member of `b` that USE cannot import). Accessibility statements
still need to apply to them (`private :: a` should hide `b%a`). A module
cannot re-export a namespace for unqualified use. The standard would need
more new text than for option A.

Tests: `export_01` is valid; `export_02`, `export_03` and `export_04` are
errors; `errors/namespace_modules_28` is an error.

### Option C: namespaces are never exported

A namespace exists only in the scope with the `use, namespace` statement,
plus host association. `b%a%x` and `use b` followed by `a%x` are errors.

Pros: simplest to specify and implement, with no .mod file changes. It is
the smallest possible proposal, and a later revision could relax it to A,
A2 or B without breaking code.

Cons: this is exactly the limitation that J3/25-119r1 calls "would make
the feature useless" (for module names). Facades are not possible, and
every scope that wants `a%x` must import `a` itself (host association
still helps within one program unit).

Tests: all four `export_*` tests are errors; `errors/namespace_modules_28`
is an error.

### Option D (for completeness): module names flow implicitly (J3/23-196r1)

In the `%%` paper, module names pass through use and host association "for
the purpose of `%%` only", even without any new USE syntax. This requires
the D2 "yes" model and a separate accessor. J3/25-119r1 called it "quite
inconsistent with how the scoping rules work". Rejected by D2 and D3.

### Test matrix for D4

| Test | A | A2 | B | C |
|---|---|---|---|---|
| `namespace_modules_export_01` (facade, `std%linalg%solve2`, no accessibility statements) | valid | error | valid | error |
| `namespace_modules_export_02` (`use b` then `a%x`, default public) | valid | error | error | error |
| `namespace_modules_export_03` (`private` default + `public :: a`; ONLY and rename of a namespace; `b%a%x`) | valid | valid | error | error |
| `namespace_modules_export_04` (same namespace through two modules) | valid | valid | error | error |
| `errors/namespace_modules_28` (`private :: a`, then `b%a%x`) | error | error | error | error |
| `errors/namespace_modules_29` (two different namespaces named `a`) | error | error | n/a | n/a |

When D4 is decided, the `export_*` tests that are errors under the chosen
option move to `tests/errors/`.

## Out of scope

* Renaming or selecting members inside a namespace
  (`use, namespace :: sm => m, only: t1 => thing1`, comments 17–25).
  Combine a namespace import with an ordinary `use m, only: ...` instead.
* Implicit namespace access without a USE statement (`%a%something`,
  comment 112; the "use the module implicitly" idea in J3/25-119r1). It
  would complicate dependency analysis.
* Nested modules (proposal #86) and packages. Modules with the same name
  in different libraries (comments 113–115) are a separate problem.
* A `namespace ... end namespace` construct (comment 105).
* `use A, only: *` together with a `-Wimplicit-use` warning (comment 92).
  It is orthogonal and could be added separately.

## Answers to the committee's objections

* *"`module1%var` is no shorter than `module1_var`"* (J3/25-119r1, 2020
  plenary). A rename needs one entry in the USE statement per entity, so
  every new entity used requires editing the USE statement, and the name
  used in the code is disconnected from its definition. A namespace import
  is one line for the whole module, `lapack%dgesv` stays greppable, and
  local names never clash with module members (see the lapack example in
  J3/20-108 and `namespace_modules_01`, `_02`).
* *"Module names are not passed through use or host association"*
  (J3/25-119r1). The namespace is a new class (1) local identifier, not the
  module name. It is host associated (`namespace_modules_11`, `_13`), and
  depending on D4 it is also use associated. Old code cannot be affected,
  because only the new syntax creates namespaces.
* *"Use rename or GENERIC instead"* (2020 plenary). Renaming works per
  entity and pollutes the local scope. Generics cannot resolve two
  procedures with the same interface (`np%sin` vs `math%sin`,
  `namespace_modules_02`), and they do not apply to variables, constants or
  types.
* *"Should work for derived types and other entities"* (2020 plenary). It
  does: types, constants, variables, generics, abstract interfaces and
  enumerators (`namespace_modules_04`–`_09`, `_20`). The
  `select type ... type is (object_m%object)` case from comment 103 is
  covered by `namespace_modules_20`.
* *"The compiler can track where an entity comes from"* (2020 plenary). It
  can, and so can readers: every qualified reference names its module.

## Prototype implementation in LFortran (plan)

* **Parser/AST**: add `namespace` to `use_modifier`. Allow
  `[local =>] module` after the modifiers when `namespace` is present. The
  AST `Use` node gets the modifier and an optional local name. Allow
  `id % id` where the grammar currently accepts only a type or interface
  name: `type(...)`, `class(...)`, `extends(...)`, `type is (...)`,
  `class is (...)`, `procedure(...)`, and the array-constructor and
  `allocate` type-specs. Expressions, calls and assignments already parse
  (`a%b` is a member access). `lfortran fmt` must print the new form back.
* **Semantics (AST → ASR)**: `use, namespace` loads the module like a USE
  statement but adds only a namespace symbol to the current symbol table.
  When the first part of `a%b...` resolves to a namespace, resolve `b` in
  the module's symbol table (checking public access). Then create, or reuse,
  an `ExternalSymbol` in the current scope with a compiler-generated name
  that cannot clash with user identifiers, and continue with the rest of
  the reference as for an ordinary use-associated entity. The same applies
  to type-spec positions. All checks (PROTECTED, PARAMETER, generic
  resolution) then come for free.
* **ASR**: after semantics, qualified references are ordinary
  `ExternalSymbol`s, so the passes and backends need no changes. The
  namespace symbol itself needs an ASR representation to be host
  associated and, depending on D4, stored in .mod files. One possibility is
  an `ExternalSymbol` whose target is the `Module` symbol itself.
* **Diagnostics**: the error tests in `tests/errors/namespace_modules_*`
  define the expected errors. The messages must be lowercase and must not
  mention ASR node names, for example "`m` is a namespace; it can only be
  used as `m%name`", "module `m` has no public entity `nosuch`", or "`only`
  cannot be used with a namespace import".

## Tests

The integration tests (`integration_tests/namespace_modules_*.f90`) were
checked with GFortran through a mechanical translation to standard Fortran
(`use, namespace :: L => M` becomes `use M, only: L__x => x, ...` and
`L%x` becomes `L__x`). They will be registered in
`integration_tests/CMakeLists.txt` with the `llvm` label once the prototype
compiles them. They cannot carry the `gfortran` label, since GFortran does
not support the syntax. The error tests (`tests/errors/namespace_modules_*.f90`)
will be registered in `tests/tests.toml` at the same time.

| Test | Covers |
|---|---|
| `namespace_modules_01` | Variables, subroutines, functions; keyword and optional arguments; local names equal to member names |
| `namespace_modules_02` | Renamed namespaces; several modules exporting the same names (J3/20-108 example); intrinsic `sin` unaffected |
| `namespace_modules_03` | Arrays, sections, allocatable arrays (including reallocation on assignment), pointers, `nullify`, `associated` |
| `namespace_modules_04` | Derived types: declarations, constructors, components, type-bound procedures, array constructor type-spec, `allocate` type-spec, `select type` |
| `namespace_modules_05` | `extends(ns%t)`, abstract types with deferred bindings, parent component name (D5) |
| `namespace_modules_06` | Constant expressions: kinds, bounds, lengths, `parameter`, `case`, enumerators |
| `namespace_modules_07` | Intrinsic modules; modifier order; two names for one module |
| `namespace_modules_08` | Generic interfaces; generic with a type's name; specifics |
| `namespace_modules_09` | Procedures as actual arguments, procedure pointers, `procedure(ns%iface)` |
| `namespace_modules_10` | Mixing with ordinary USE; several namespaces of one module; repeated import (D7) |
| `namespace_modules_11` | Host association (internal and module procedures, BLOCK); shadowing |
| `namespace_modules_12` | Namespace imports local to procedures, BLOCK, external procedures |
| `namespace_modules_13` | Submodules |
| `namespace_modules_14` | Type-bound operators and assignment; explicit import of a defined operator |
| `namespace_modules_15` | Interface bodies: `IMPORT` of a namespace (D6) and local namespace import |
| `namespace_modules_16` | Members named like intrinsics, like the namespace, like another namespace |
| `namespace_modules_17` | Character variables, substrings, deferred length |
| `namespace_modules_18` | Input/output statements |
| `namespace_modules_19` | Elemental and pure procedures |
| `namespace_modules_20` | ASSOCIATE and SELECT TYPE selectors; `type is (ns%t)` |
| `namespace_modules_21` | Fixed-form source |
| `namespace_modules_22` | Case insensitivity and blanks around `%` |
| `namespace_modules_23` | Local generic with the same name does not extend `ns%gen` |
| `namespace_modules_export_01`–`_04` | D4, see the matrix above |

| Error test | Error |
|---|---|
| `namespace_modules_01` | Member used without qualification |
| `namespace_modules_02` | `only` with a namespace import |
| `namespace_modules_03` | Rename list with a namespace import |
| `namespace_modules_04` | No such member |
| `namespace_modules_05` | Private member |
| `namespace_modules_06` | Namespace used as a value |
| `namespace_modules_07` | Namespace as an actual argument |
| `namespace_modules_08` | Assignment to a namespace |
| `namespace_modules_09` | Namespace name clashes with a local entity |
| `namespace_modules_10` | One local name for two different modules |
| `namespace_modules_11` | Ambiguous reference: namespace vs use-associated entity |
| `namespace_modules_12` | Modifying a PROTECTED variable |
| `namespace_modules_13` | Qualifier with a module that was only used with plain USE (D2) |
| `namespace_modules_14` | Qualifier with a module that was not used |
| `namespace_modules_15` | `namespace` modifier repeated |
| `namespace_modules_16` | Missing `::` |
| `namespace_modules_17` | Module renamed in an ordinary USE |
| `namespace_modules_18` | Defined operator not imported by a namespace import |
| `namespace_modules_19` | Qualified name as a DO variable |
| `namespace_modules_20` | Qualified name as a literal kind parameter |
| `namespace_modules_21` | Namespace hidden by a local variable in an internal procedure |
| `namespace_modules_22` | Module imports itself |
| `namespace_modules_23` | Local entity named like the renamed module |
| `namespace_modules_24` | Type used as a data object |
| `namespace_modules_25` | Declaring a qualified name |
| `namespace_modules_26` | Interface body without `IMPORT` |
| `namespace_modules_27` | Assignment to a named constant |
| `namespace_modules_28` | Private namespace of a module (D4, all options) |
| `namespace_modules_29` | Ambiguous exported namespaces (D4 options A, A2) |
| `namespace_modules_30` | Local generic does not extend `ns%gen` |
