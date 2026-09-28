# Namespaces for Modules (`use, namespace`)

Status: **design draft for an LFortran prototype**. This is an LFortran
extension that implements
[j3-fortran/fortran_proposals#1](https://github.com/j3-fortran/fortran_proposals/issues/1).
It is not (yet) part of any Fortran standard.

## Summary

```fortran
use, namespace :: utils                 ! import the module as a module entity
use, namespace :: np => numpy           ! ... under a different local name
use, intrinsic, namespace :: env => iso_fortran_env

call utils%savetxt("a.txt", x)          ! members are reached with "%"
y = np%sin(np%pi)
real(env%real64) :: z
type(np%ndarray_t) :: a
x = std%linalg%solve(A, b)              ! module entities can be members too
```

`use, namespace :: M` puts exactly one name into the scope: a **module
entity** named `M`, or named `L` with `L => M`. The module entity designates
the module `M`. Every public entity of the module is then reachable as
`M%name`. No member name is made accessible without the qualifier, so the
scope is not polluted.

A module entity is an ordinary class (1) local entity. It is host
associated, it is public or private like any other entity of a module, and
it is use associated by users of that module. So `std%linalg%solve` works,
where `std` imported `linalg` as a module entity. This is exactly Python's
model:

| Python                 | Fortran today            | With this feature           |
|------------------------|--------------------------|-----------------------------|
| `from A import x`      | `use A, only: x`         | (unchanged)                 |
| `from A import x as y` | `use A, only: y => x`    | (unchanged)                 |
| `from A import *`      | `use A`                  | (unchanged)                 |
| `import A`             | N/A                      | `use, namespace :: A`       |
| `import A as B`        | N/A                      | `use, namespace :: B => A`  |
| `B.A.x` (`B` did `import A`) | N/A                | `B%A%x` (`B` did `use, namespace :: A`) |

## History and sources

The design is based on the whole issue discussion (115 comments,
2019–2025), and on the committee papers and minutes:

* J3/19-246 and J3/20-108 (Čertík): the original proposal,
  `use, namespace :: A` / `use, namespace :: B => A`, accessed with `%`.
  These papers did not say whether the namespace passes through use
  association.
* J3 meeting 221, 2020-02-26: tutorial and discussion of 20-108, with no
  vote. The minutes record: "Malcolm says not compelling, can be done with
  rename. Van says it can be done with use, only. Tom likes the clarity of
  expressing where a variable came from. Vipul noted that the tutorial
  demonstrated a need for namespace management, and collaboration is
  necessary for evolution of the language." Comment 26 of the issue has a
  longer summary.
* J3/23-196r1 (Cohen), passed at meeting 230 (June 2023). The feature was
  recast as "remote access to module entities", qualified by **the module
  name itself** after an ordinary USE statement, with a new `%%` operator.
  Module names would flow through use and host association "for the purpose
  of `%%` only". This became WG5 work item **US13** "Add namespace-like
  access to module entities" (N2222, N2234).
* J3/25-119r1 (Cohen, data subgroup), passed at meeting 235 (February 2025):
  stop developing US13 for Fortran 2028 and investigate "more advanced forms
  of namespace control" instead. In the issue (comment 105), a J3 member
  described the direction as a `namespace ... end namespace` construct that
  would make "a namespace ... a first class entity".
* The issue, April 2025 (comments 106–111): the design adopted here (a
  module entity is use associated, `b%a%x`) was proposed after 25-119r1.
  The reply was that it "seems potentially workable, if possibly tricky to
  define in the standard".
* J3/26-121 (Cohen), passed at meeting 239 (February 2026): "those
  investigations have not yet borne fruit", so not in Fortran 2028. The
  minutes note "us13 - recommend to withdraw (25-119r1 explains why hard)".
* WG5 meeting, June 2026 (N2259, N2261): US13 was removed from the work
  list for the next standard, together with six other items. No reason
  specific to US13 is recorded.

In short, the committee analysed and rejected one design: remote access by
module name, with implicit flow of module names. It never evaluated the
design in this document. The section "Objections and answers" below goes
through every objection raised.

The prototype exists to answer the request in comments 54 and 92: "I think
we need to implement this in a compiler and actually use it."

## Decisions

| #  | Question | Decision |
|----|----------|----------|
| D1 | Syntax of a namespace import | `use, namespace :: [L =>] M` |
| D2 | Does a plain `use M` also make `M%x` available? | No |
| D3 | Accessor | `%` |
| D4 | What is `L`, and does it pass through association? | A **module entity**: an ordinary class (1) entity, host and use associated (Python's model) |
| D5 | Name of a parent component when extending `L%t` | The type's name in its module (`t`) |
| D6 | May `IMPORT` in an interface body name a module entity? | Yes |
| D7 | Two namespace imports of the same module under the same local name | Allowed (redundant) |
| D8 | Identity | A module entity designates the module itself; module entities for the same module are never in conflict |
| D9 | Accessibility in the statement | `use, namespace, private :: L => M` and `use, namespace, public :: ...` (module specification part only) |

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

### D2: a plain `use M` does not create a module entity

Klausler's model (comments 89–93): every USE statement would also make the
module name a qualifier, so `use A, only: x` would give `x` plus `A%...`.
The prototype keeps the conservative choice, so a plain USE behaves exactly
as today. The only way to create a module entity is the new syntax, which
makes the feature purely additive. No existing program, and no existing
module, contains a module entity.

### D3: accessor `%`

* `%` (chosen). It reads like a component reference and IDEs already
  understand it. A module entity is a class (1) local identifier, like a
  variable, so a module entity and a variable with the same name can never
  both be accessible unqualified in one scope: `L%x` is never ambiguous (see
  Name resolution below).
* `%%` (J3/23-196r1). Keeps module access visually distinct and would let a
  variable and a module with the same name coexist. That is needed only if
  module *names* flow implicitly into scopes that did not ask for them,
  which this design never does.
* `::` (comments 80, 86, 87). Ambiguous with array sections `a(1::2)` and
  with specification statements (J3/23-196r1).

### D4: module entities

See the section "Module entities" below.

### D5: parent component name

`type, extends(L%t) :: u` gives `u` a parent component named `t` (the name
of the type in the module that defines it), because the qualified name
`L%t` is not a name. Test: `namespace_modules_05`.

### D6: `IMPORT` of a module entity

An interface body does not access its host by host association, so a
module entity of the host is not accessible there. `import :: L` (and
`import, all`) makes it accessible, just like any other host entity. A
`use, namespace` statement inside the interface body also works. Test:
`namespace_modules_15`. A missing `import` is not diagnosed yet (see the
known limitations below).

### D7: repeated namespace imports

`use, namespace :: s => m` may appear twice for the same module, just as the
same USE statement may be repeated. Giving the same local name to two
*different* modules is an error (`cc_10`).

### D8: identity

A module entity designates the module itself, just as in Python
`b.a is a`. So `a%x`, `b%a%x` and `c%a%x` all designate the same variable
`x`. Two module entities that arrive in a scope under the same local name
are not in conflict if they designate the same module. This covers a direct
`use, namespace :: a` together with a `use b` that exports `a`, and two
modules that both export `a` (`namespace_modules_27`, `_28`). If they
designate different modules, the name must not be referenced, the usual
rule for use-associated entities (`cc_29`).

### D9: accessibility in the namespace import

In the specification part of a module, the accessibility of a module entity
can be given in the statement itself:

```fortran
use, namespace, private :: helper => internal_helpers   ! used only inside
use, namespace, public :: linalg                          ! part of the API
```

This is equivalent to the namespace import followed by
`private :: helper` or `public :: linalg`. Separate `public`/`private`
statements naming the module entity work too. Outside a module, or with
more than one *access-spec*, the statement is an error
(`namespace_modules_28`, `cc_31`, `cc_32`, `cc_33`).

## Syntax

The USE statement (F2023 R1409) gets a third form:

```
use-stmt  is  USE [[, module-nature] ::] module-name [, rename-list]
          or  USE [[, module-nature] ::] module-name , ONLY : [only-list]
          or  USE , namespace-modifier-list :: [module-entity-name =>] module-name

namespace-modifier  is  NAMESPACE
                    or  module-nature          ! INTRINSIC or NON_INTRINSIC
                    or  access-spec            ! PUBLIC or PRIVATE
```

Constraints:

* C1: `NAMESPACE` appears exactly once in a *namespace-modifier-list*, at
  most one *module-nature* appears, and at most one *access-spec* appears.
  They may be in any order (`use, intrinsic, namespace ::` and
  `use, namespace, intrinsic ::`). (`cc_15`, `cc_32`)
* C2: An *access-spec* is allowed only in the specification part of a
  module (like an *access-stmt*). (`cc_31`)
* C3: The `::` is required (as it is whenever a *module-nature* is given).
  (`errors/namespace_modules_16`)
* C4: A namespace import has neither a *rename-list* nor `ONLY`. A USE
  statement that is not a namespace import cannot rename the module
  (`use :: b => a` is invalid). Members cannot be renamed "inside" a module
  entity (comments 19–25: like the components of a derived type, the
  members of a module keep their names). (`cc_02`,
  `errors/namespace_modules_03`, `cc_17`)
* C5: A module shall not import itself (existing rule for USE).
  (`cc_22`)

A module entity also appears in ordinary USE statements of *other* modules,
like any other entity: `use b, only: a` and `use b, only: aa => a`, where
`a` is a module entity of `b`.

The qualified name (the accessor) is:

```
module-qualified-name  is  module-entity-name % name
```

If `name` is itself a module entity of that module, the qualification
continues: `std%linalg%solve`. A module-qualified name may be followed by
the usual part references, section subscripts, substring ranges, component
references and actual argument lists, exactly as the member's unqualified
name could be. Blanks are allowed around `%` and names are case
insensitive, as for component references (`namespace_modules_22`). In fixed
form, blanks are insignificant as usual (`namespace_modules_21`).

## Module entities

`use, namespace :: L => M` declares `L` (or `M` if there is no rename) as a
**module entity**: a new kind of named entity that designates the module
`M`. It is a class (1) local identifier (F2023 19.3.1), in the same
category as a generic name, a namelist group name or a derived type name.

**A module entity is not a data type and not a data object.** It has no
value, no storage and no run-time representation. Everything about it is
resolved at compile time. Fortran gains no new type. A module entity names
a module, the way a procedure name names a procedure.

### Examples

```fortran
module a
    integer :: x = 1
end module

module b
    use, namespace :: a        ! "a" is a module entity of b (public by default)
    integer :: y = 2
end module

program p1
    use, namespace :: b        ! Python: import b
    print *, b%y, b%a%x        ! Python: b.y, b.a.x
end program

program p2
    use b                      ! Python: from b import *    (brings y and a)
    print *, y, a%x
end program

program p3
    use b, only: aa => a       ! Python: from b import a as aa
    print *, aa%x
end program
```

A library can build a facade (`namespace_modules_24`):

```fortran
module std
    use, namespace :: linalg => std_linalg
    use, namespace :: stats => std_stats
end module

program main
    use, namespace :: std
    x = std%linalg%solve(A, b)
    m = std%stats%mean(x)
end program
```

A module that uses a module entity only internally keeps it private, as for
any other entity it does not want to export (`namespace_modules_28`,
`cc_28`, `cc_33`):

```fortran
module b
    use, namespace, private :: helper => b_internal_helpers
    ...
end module
```

### Rules

A module entity follows the rules of every other class (1) local entity:

* **Host association**: it is accessible in internal procedures, module
  procedures, BLOCK constructs and submodules of the scope that declares it
  (`namespace_modules_11`, `_13`). In interface bodies it is accessible only
  through `IMPORT` (D6). A local entity of an inner scope with the same name
  hides it (`namespace_modules_11`, `cc_21`).
* **Use association**: a public module entity of a module `b` is accessible
  to `use b`, can be listed in `use b, only: a`, and can be renamed with
  `use b, only: aa => a` (`namespace_modules_25`, `_26`, `_27`, `_28`).
* **Accessibility**: default accessibility, `public`/`private` statements,
  and D9 apply. A private module entity is not accessible outside its
  module, neither by USE nor as `b%a` (`cc_28`, `cc_33`).
* **Members**: a public module entity of `b` is a member of `b`, so
  `b%a%x` works (`namespace_modules_24`, `_26`, `_28`, `_29`).
* **Conflicts**: it must not have the same name as another local entity of
  the scope (`cc_09`). If it has the same local name
  as a use-associated entity, the name must not be referenced
  (`cc_11`), unless both designate the same module
  (D8).

It can only appear:

* as the leftmost part of a module-qualified name `L%member` (or inside a
  chain `b%L%member`);
* in `IMPORT`, `PUBLIC` and `PRIVATE` statements;
* in the only-list or rename-list of a USE statement of a module that
  exports it.

It cannot be referenced on its own (`print *, L`), used as an actual
argument, assigned to, or subscripted (`cc_06`, `cc_07`,
`cc_08`). These positions are deliberately left free for the future
extensions described below.

The module name `M` is still a global identifier used in the scope, even
when the module entity is renamed. By the existing F2023 19.3.1 rule, the
scope cannot declare a local entity named `M`. This is not diagnosed yet
(see the known limitations below).

### Compatibility

The feature is purely additive. Module entities are created only by the
new `use, namespace` syntax (D2), so no existing program or module contains
one. Every existing USE statement means exactly what it meant before,
including USE statements of existing modules. The use association of module
entities (`use b` brings in `a`) can only happen for a module `b` that
itself uses the new syntax. The author of `b` then chose to make `a` part
of `b`'s interface, and can write `private` to keep it internal, exactly as
for any other entity.

### Doors left open

Because a module entity designates a module, later revisions can add
operations on modules without changing anything in this design. Python's
`ModuleType` shows what is useful. In Fortran these would be compile-time
inquiry intrinsics that take a module entity as their argument. For
example:

| Python | Possible Fortran analogue |
|---|---|
| `m.__name__` | `module_name(m)`, a constant `"numpy"` |
| `dir(m)`, `m.__dict__` | an inquiry returning the names of the public members of `m` |
| `m.__doc__` | a documentation string, once Fortran has docstrings |
| `m.__file__`, `m.__path__` | processor-dependent information about where the module came from |
| `hasattr(m, "x")` | an inquiry whether `m` has a public member `x` |

Other extensions this design leaves room for:

* A module path in a USE statement, `use std%linalg, only: solve` or
  `use, namespace :: la => std%linalg`, once `std%linalg` designates a
  module.
* Nested modules (proposal #86) and a `namespace ... end namespace`
  construct (comment 105). Both would create entities of this same kind.
* Accessibility on ordinary USE statements (`use, private :: m`),
  generalizing D9.

None of these is part of the prototype. They are listed so that nothing in
the prototype closes them off.

## Semantics of member access

### What `L%name` designates

`L%name` designates the entity that `use M, only: name` would make
accessible: any public entity of `M`, including entities that `M` itself
accesses by use association and makes public, and including public module
entities of `M`. It designates *the same entity* as any other route to it
(ordinary USE, another module entity for the same module, host
association). Every attribute of the entity applies unchanged. For
example, `PROTECTED` still forbids modifying it outside its module
(`cc_12`), a named constant is still a constant
(`cc_27`), and a private entity is not accessible
(`cc_05`). A name that is not an entity of the module
is an error (`cc_04`).

`L%name` does not rename anything and makes no name accessible. A namespace
import makes none of the member names accessible without qualification
(`cc_01`). Local entities may therefore have the same
names as members, and the two stay distinct (`namespace_modules_01`, `_16`).

### Where `L%name` may appear

Anywhere the member's name could be *referenced*:

| Kind of member | Allowed uses (with tests) |
|---|---|
| Variable | Expressions; assignment targets; part references `L%a(i)`, `L%a(2:4)`, `L%s(1:3)`, `L%obj%comp`; actual arguments; `allocate`/`deallocate`; both sides of pointer assignment; `nullify`; `associated`, `allocated`, `size`, ...; input/output items, unit and format specifiers, implied-DO lists; ASSOCIATE and SELECT TYPE selectors (`_01`, `_03`, `_17`, `_18`, `_20`) |
| Named constant, enumerator | All constant expressions: kind selectors `real(L%dp)`, array bounds, character lengths, `parameter` initialization, `case` selectors, kind arguments of intrinsics (`_06`, `_07`, `_29`) |
| Procedure | `call L%s(...)`, function references, keyword and optional arguments, actual arguments, procedure pointer targets, `associated(p, L%f)` (`_01`, `_09`, `_19`) |
| Generic interface | Generic resolution over the module's specifics, including a generic with the same name as a type (`_08`, `_29`) |
| Abstract interface | `procedure(L%iface)` declarations of dummy procedures and procedure pointers (`_09`) |
| Derived type | `type(L%t)`, `class(L%t)`, structure constructors `L%t(...)`, array constructor type-spec `[L%t :: ...]`, `allocate(L%t :: x)`, `type is (L%t)`, `class is (L%t)`, `extends(L%t)` (`_04`, `_05`, `_14`, `_20`, `_29`) |
| Module entity | Further qualification `L%inner%x`, in all the positions above (`_24`, `_26`, `_28`, `_29`) |

Type-bound procedures, type-bound generics, type-bound operators and
type-bound assignment belong to the type. They work on objects whose type
was accessed through a module entity, with no extra import
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
  (`cc_24`).

### Operators, assignment and defined input/output

Non-type-bound defined operators (`operator(.dot.)`, `operator(+)`),
defined assignment (`assignment(=)`) and non-type-bound defined
input/output (`write(formatted)`) have no name that could be qualified
(comment 7 asked whether `x .cross_product. y` would become
`x foo::operator(.cross_product.) y`). A namespace import does **not** make
them accessible (`cc_18`). They are imported
explicitly next to the namespace import:

```fortran
use, namespace :: v => vectors
use vectors, only: operator(.dot.), assignment(=)
```

(`namespace_modules_14`). Type-bound versions need no import (see above).

### Generic interfaces are not extended

With an ordinary USE, a local `interface gen` extends the use-associated
generic `gen`. `L%gen` is not a local name, so a local generic with the
same name is a separate generic and does not extend `L%gen`
(`namespace_modules_23`, `cc_30`). Comment 7 asked
about this interaction.

### Name resolution

The first name of `a%b` is resolved like any local identifier (local
entity, then host-associated entity, and so on). If it resolves to a module
entity, `a%b` is a module-qualified name, and `b` is looked up among the
public entities of that module. If `b` is itself a module entity, a further
`%c` is resolved in the same way. Otherwise `a%b` is a component reference
or type-bound procedure reference, as today. A module entity and a variable
with the same name cannot both be accessible in a scope (see Rules above),
so this is unambiguous.

### Scoping details

* A `use, namespace` statement is allowed wherever a USE statement is
  allowed: in a program, module, submodule, subroutine, function, BLOCK
  construct or interface body (`namespace_modules_12`, `_15`). A module
  entity declared in a procedure or BLOCK is local to it; only module
  entities in the specification part of a module can be use associated.
* Submodules see the module entities of their ancestors by host
  association, and can declare their own (`namespace_modules_13`; comments
  60, 62, 72, 73). Submodules themselves are never visible through a
  module entity: `M%submod%...` does not exist (comment 73).

### Combining with ordinary USE statements

A scope may freely mix a namespace import of `M` with ordinary USE
statements of `M`. The same module may also be imported under several
module entity names. All routes designate the same entities
(`namespace_modules_10`; comments 22–23):

```fortran
use, namespace :: sm1 => some_module
use, namespace :: sm2 => some_module
use some_module, only: t1 => thing1   ! t1, sm1%thing1 and sm2%thing1 are the same
```

Renames in an ordinary USE do not affect member access: above, `sm1%t1`
does not exist and `sm1%thing1` does (comments 18–21). Conversely, `L%name`
is not a rename, so it does not hide the unqualified `name` that another
USE statement of the same module makes accessible (`namespace_modules_28`).

### Intrinsic modules

Intrinsic modules can be imported as module entities
(`use, intrinsic, namespace :: env => iso_fortran_env`, with
`env%real64`, `c%c_loc`, `c%c_ptr`, ...). See `namespace_modules_07`.

## Objections and answers

These are all the objections and concerns raised in the issue, at the J3
meetings and in the J3 papers, with their answers for the design in this
document. For each, the source is given.

### "It is not worth it"

1. **"Not compelling, can be done with rename"** (Cohen, 2020 minutes;
   J3/25-119r1 §2 and §3: "`module1%%var` is still not any shorter than
   `module1_var`", "available since Fortran 90"). **"No big difference
   between `module%foo` and `module_foo`"**, **"prefers rename on use"**
   (2020 plenary).
   Renaming works per entity. Every entity used needs its own entry in the
   USE statement, the entry has to be kept in sync with the code, and the
   renamed name is disconnected from its module. A namespace import is one
   line for the whole module. It never pollutes the scope, and every
   reference names its module (the lapack example of J3/20-108,
   `namespace_modules_01`, `_02`). Renaming also cannot express a facade
   module (`std%linalg%solve`, `namespace_modules_24`). The committee's own
   comparison was with the remote-access design, where the qualifier had to
   be written in full each time. Here the module entity can be renamed once
   (`use, namespace :: la => std_linalg`).
2. **"Can be done with use, only"** (Snyder, 2020 minutes). `use, only`
   imports names unqualified, so it is the opposite of what is asked for.
   It is what one uses today, with the costs described in item 1.
3. **"Use the GENERIC facility: make conflicting functions generic and
   rename on use"** (2020 plenary). Generic resolution cannot choose
   between two procedures with the same interface (`np%sin` and
   `math%sin`, `namespace_modules_02`), and it does not apply to variables,
   named constants or types.
4. **"A solution in search of a problem"**, **"hard to come up with a
   compelling use case; more relevant in the Python world where packages
   come from disparate places"** (2020 plenary). Fortran now has a package
   ecosystem (fpm, stdlib) where modules from different authors meet in one
   program. Name clashes between them, and long `use, only` lists, are
   everyday problems (comment 9, J3/20-108). Other languages provide
   exactly this: Python, Julia, Haskell (said at the 2020 plenary to be
   "useful"), Chapel, Modula (2020 plenary).
5. **"This change is a convenience, not a fix or an enabling feature. We
   need to prioritize"** (Klausler, quoted in comment 12); "low priority"
   (comment 12); "I don't see the feature as necessary" (comment 9). A
   convenience that affects every USE statement in every program is worth
   prioritizing. The facade case (item 1) is also enabling: it cannot be
   done today. An implementation in LFortran provides the usage experience
   needed to judge it.
6. **"Benefit/cost ratio would not only be small, but likely be negative
   (viz cure worse than the disease)"** (J3/25-119r1 §6). That judgement
   was made about the remote-access design, and its costs were items 7–11
   below. None of them apply here. The remaining cost is new text in the
   standard (item 16).

### Scoping and compatibility

7. **"Module names are not passed through subsequent use or host
   association, which means it would only work in a scope that directly
   uses the module. That would make the feature useless. Passing module
   names through use or host association … cannot be changed without
   introducing an incompatibility"** (J3/25-119r1 §2; also J3/23-196r1:
   "`%` only gives one-level-back of access, which is not good enough").
   The qualifier here is not the module name. It is a module entity, a new
   class (1) local entity created by new syntax. It passes through host
   association and use association by the existing rules for class (1)
   entities, so the feature works in any scope. No incompatibility is
   possible, because no existing code contains a module entity (D2). The
   paper says the same about renaming: "renaming produces a normal class
   one name that is already passed through use and host association". A
   module entity is such a name.
8. **The incompatibility example** (comment 107): `module b; use a; end`,
   then `use b; integer a` is valid today and must stay valid. It does:
   a plain `use a` in `b` creates no module entity, so `use b` brings no
   `a`.
9. **"Quite inconsistent with how the scoping rules work"** (J3/25-119r1
   §3, about module names flowing through association "for the purpose of
   `%%` only"). No special scoping rule exists here. Module entities
   follow the ordinary rules for class (1) entities.
10. **"Allow `module-name%%whatever` to use the module implicitly … would
    complicate module dependency analysis"** (J3/25-119r1 §3; also
    `%a%something`, comment 112). Here a module entity always comes from a
    `use, namespace` statement, so every module dependency is explicit in a
    USE statement, as today.
11. **Should access bypass ONLY? "Additional complexity, potential
    confusion, and rendering some deliberate namespace controls
    ineffective"** (J3/23-196r1, J3/25-119r1 §4). Nothing is bypassed. A
    plain `use m, only: x` creates no module entity (D2). A namespace import
    is an explicit request for the public entities of the module, and the
    module's own `private` statements stay in control.
12. **"In relation to nesting of module entities, will direct qualification
    be possible? If you allow nesting there may be more than one unique way
    to get to the object you want"** (2020 plenary). Yes, `b%a%x` and a
    direct `a%x` both reach `x`. They designate the same entity (D8), just
    as one entity can already reach a scope through several USE paths
    without conflict (F2023 14.2.2).
13. **A variable and a module with the same name** (the `mfarthest` example
    in J3/23-196r1). A module entity and a variable are both class (1)
    names. If both reach a scope through use association, the existing
    rule applies: the name must not be referenced. One of them can be
    renamed on USE (`use mnear, only: v => mfarthest`).
14. **"`use b` now brings in `a`, which pollutes the user's scope"**. `use b`
    already brings in every public entity of `b`, including everything `b`
    itself uses from other modules. A module entity is one more public
    entity, and it exists only because the author of `b` wrote
    `use, namespace`. The author hides it with `private` (D9), the same
    control as for anything else. A user who wants no pollution writes
    `use, namespace :: b`, the whole point of the feature.

### It adds a new kind of thing

15. **"It adds a new type to Fortran."** It does not. A module entity is a
    named entity, not a data type or a data object. It has no values, no
    storage and no run-time representation, and nothing about types
    changes. The language already has many kinds of named entities that are
    not data objects (generic names, namelist groups, construct names,
    abstract interfaces). Fortran 2023 added enumeration types, and Fortran
    202Y is adding templates and requirements. In the committee's own
    discussion, a namespace "as a first class entity" was suggested
    (comment 105).
16. **"Possibly tricky to define in the standard"** (comment 109). This is
    the real cost. It needs a new form of the USE statement (clause 14), a
    new kind of class (1) entity with its access rules (clause 19), and
    module-qualified names in the syntax rules for designators and
    type-specs. Most of the rules are the existing ones for class (1)
    entities, by design. This prototype and its tests are meant to show
    exactly how much is needed.
17. **"Handle it as part of a formal namespace concept, not piecemeal"**
    (comments 79, 95). A module entity is that concept for modules. A later
    `namespace ... end namespace` construct or nested modules (#86) would
    create entities of the same kind (see "Doors left open").

### Syntax and the accessor

18. **Dislike of `%`; "some compilers use colon in error messages"** (2020
    plenary); **`%` should be distinct from component access, as `()` for
    both arrays and functions is confusing** (comment 80). `%` already
    means "member of", and a module entity is used exactly like a
    structure whose components are the module's public entities, which is
    how users think of it (comments 9, 25, 28). Resolution is unambiguous
    (Name resolution above). The alternatives are worse: `::` is ambiguous
    (J3/23-196r1), `.` conflicts with operators, and backquote or `#` use up
    a special character (J3/23-196r1).
19. **`use namespace A` is ambiguous in fixed form** (comment 55). The chosen
    syntax has commas and `::` (`namespace_modules_21`).
20. **`use A, only:` is not clear to a reader** (comments 67, 68). Not used.
21. **The keyword `namespace` is long, and means something else in C++**
    (comment 77); **reserve it for a future namespace concept** (comments
    79, 95). The keyword describes what the statement does (it imports a
    module as a namespace). If a namespace construct is added later, it
    would produce entities of the same kind (item 17).
22. **`use, namespace :: np => numpy` invites a list,
    `np => numpy, lpk => lapack`** (comment 80). One module per statement,
    as for every USE statement.
23. **People will try `use, namespace :: sm => m, only: t1 => thing1`**
    (comments 17–25). It is an error with a clear message
    (`cc_02`, `errors/namespace_modules_03`). Members keep their names, as
    components do. Combine with an ordinary `use m, only: t1 => thing1`.

### Semantic questions

24. **Operators, named operators, generic interfaces and their extension,
    submodules, interface bodies, BLOCK** (Ian Harvey via comment 7).
    Answered in the sections above: operators and assignment (not imported;
    type-bound ones work), generics (resolved normally, not extended by
    local generics), submodules and BLOCK (host association), interface
    bodies (`IMPORT`).
25. **Submodules** (comments 60, 72). Submodules see their ancestors' module
    entities, and are themselves never reachable through a module entity
    (comment 73).
26. **Do two module entities for one module share saved variables?**
    (comment 24). Yes: they designate the same module and the same
    entities (D8, `namespace_modules_10`).
27. **"Should work for derived types and other entities"** (2020 plenary).
    It does: types, constants, variables, generics, abstract interfaces,
    enumerators and module entities (`namespace_modules_04`–`_09`, `_20`,
    `_29`). The `type is (object_m%object)` case of comment 103 is covered
    by `namespace_modules_20`.
28. **Workaround with a derived type whose type-bound procedures wrap the
    module's procedures** (comment 28). It cannot cover variables,
    constants or types, it needs a wrapper for every procedure, and the
    module and the object need different names (comments 28–31).

## Out of scope

* Renaming or selecting members inside a module entity
  (`use, namespace :: sm => m, only: t1 => thing1`, comments 17–25).
  Combine a namespace import with an ordinary `use m, only: ...` instead.
* Implicit module access without a USE statement (`%a%something`,
  comment 112; the "use the module implicitly" idea in J3/25-119r1).
* Modules with the same name in different libraries (comments 113–115).
* `use A, only: *` together with a `-Wimplicit-use` warning (comment 92).
  It is orthogonal and could be added separately.
* The future extensions listed under "Doors left open".

## Prototype implementation in LFortran

* **Parser and AST**: `namespace`, `public` and `private` are USE modifiers
  (`SimpleAttribute`), and the `Use` node has an optional `local_name` for
  `L => M`. In the positions that otherwise take only a type or interface
  name (`type(...)`, `class(...)`, `extends(...)`, `type is (...)`,
  `class is (...)`, `procedure(...)`, the type-spec of an array constructor
  and a type-bound `procedure(...)`), `a%b%c` is parsed into the name `c`
  and a separate list `qualifier`, `[a, b]` (a field of `AttrType`,
  `AttrExtends`, `TypeStmtName`, `ClassStmt`, `ArrayInitializer` and
  `DerivedTypeProc`). Expressions, calls, assignments and the type-spec of
  ALLOCATE already parse, since `a%b` is a member access. `lfortran fmt`
  prints the new forms back.
* **ASR**: a new symbol, [ModuleReference](asr/asr_nodes/symbol_nodes/ModuleReference.md)
  `(parent_symtab, name, module_name, access)`, is the module entity. It is
  host associated like any symbol, use associated through an
  `ExternalSymbol` that points to it, and saved in .mod files, so chains
  `b%a%x` also work across separate compilation.
* **Semantics (AST to ASR)**: `use, namespace` loads the module like any USE
  statement but declares only a `ModuleReference`. The AST is not modified.
  A module-qualified name is resolved where names are resolved: in the
  visitors of names, function references and array elements, subroutine
  calls and coindexed objects, and wherever a type-spec, a namelist group
  name or another name is looked up on its own. If the first part of a
  designator is a module entity (a `ModuleReference`, or an `ExternalSymbol`
  for one), one helper (`resolve_module_qualified`) follows the module
  entities of the chain, checks that each member is a public entity of its
  module and diagnoses ambiguous and invalid references. The member is made
  accessible in the scope of the reference as a private `ExternalSymbol`,
  created by the same code as `use M, only: x`, so generics, type-bound
  procedures, constructors, `PROTECTED`, named constants and intrinsic
  modules behave exactly as for use association. It is stored under the
  generated name `x~of_M` (see `generated_symbol_name`), which no
  identifier can spell, so it cannot clash with a user entity and is not
  listed as a document symbol. These symbols are recorded as such when they
  are created (`ModuleEntityState`, shared by the symbol table and body
  visitors), so that a module does not export them. The designator is then
  analysed as if it named that symbol. The rest of semantics, the ASR passes
  and the LLVM backend see only ordinary use-associated entities.
* **Names in diagnostics**: semantic diagnostics never show a generated
  name. The ones reported while a designator is resolved show it as
  written there (`a%f`). Everywhere else a derived type or an enumeration
  type is shown by its own name, as for a variable declared with that type:
  the type of `a%t(1)` in a type mismatch reported for the enclosing
  statement is `t`. This is the name of the type in its module, not the
  local name of the symbol that refers to it (`libasr`'s
  `type_to_str_fortran_symbol`), so it does not depend on the scope, on
  the local name of the module entity, or on whether the symbol was
  declared in this file, in a `.mod` file or in the parent of a submodule
  (`errors/namespace_separate_component.f90`,
  `errors/namespace_separate_submodule.f90`). The same holds for a type
  renamed by an ordinary USE (`use m, only: tt => t`, `cc_55`). Only the
  messages of internal compiler errors (failed assertions) can still
  contain a generated name.
* **Portability**: with `--std=f23` (or `--std=legacy`), every
  `use, namespace` statement gets a warning that it is an LFortran extension
  (`tests/warnings/namespace_modules_std_01`).
* **Diagnostics**: the semantic errors are all reported with
  `--continue-compilation` by one file,
  `tests/errors/namespace_continue_compilation.f90` (cases `cc_NN`); the
  syntax errors are in `tests/errors/namespace_modules_*`. Their messages
  are in `tests/reference/`.

Known limitations of the prototype:

* `type, private :: t` is not enforced: `L%t` accepts a derived type that
  is private in its module, exactly as `use M, only: t` does in LFortran
  today, since the derived type records only the default accessibility of
  the module where it is defined (lfortran/lfortran#13729). Derived types
  made public by `public :: t` or `type, public :: t` in a module whose
  default accessibility is `PRIVATE` work (`namespace_modules_32`).
* The Fortran dependency scanner of CMake does not recognize
  `use, namespace`, so a file whose only USE of a module is a namespace
  import is not ordered after the file that defines the module.
* A parameterized derived type accessed through a module entity cannot be
  given type parameters in a type-spec: `type(L%pdt(8))`,
  `type(L%pdt(k=8))` and `class(L%pdt(8))` are syntax errors, because the
  parser accepts a qualified name in `type(...)` and `class(...)` only
  without a type-parameter list. `type(L%pdt)`, with the default values
  of the parameters, works. Until this is supported, access the type with
  `use M, only: pdt` and write `type(pdt(8))`.
* LFortran reports an ASR verification error instead of a semantic error
  for `call f()` where `f` is a function (lfortran/lfortran#13804), also
  for `call L%f()` and for a generic `call L%gen()` whose specific is a
  function. The error names the procedure itself (`f`, or the specific
  `gen_int`), not the symbol that refers to it.
* Two errors are not diagnosed yet, because LFortran does not enforce the
  underlying rules for ordinary USE statements either. Error tests for them
  will be added when those issues are fixed.
  - A local entity named like a module used in the scope, even when the
    module entity is renamed (lfortran/lfortran#12850). This program is
    accepted:

    ```fortran
    module mod_a
        implicit none
        integer :: x = 1
    end module

    program local_named_like_module
        use, namespace :: m => mod_a
        implicit none
        integer :: mod_a
        mod_a = 2
        print *, m%x, mod_a
    end program
    ```

  - A module entity of the host referenced in an interface body without
    `IMPORT` (lfortran/lfortran#13799). This program is accepted:

    ```fortran
    module mod_t
        implicit none
        type :: t
            integer :: x = 1
        end type
    end module

    program interface_without_import
        use, namespace :: m => mod_t
        implicit none
        interface
            subroutine ext(a)
                type(m%t), intent(in) :: a
            end subroutine
        end interface
    end program
    ```

## Tests

The integration tests (`integration_tests/namespace_modules_*.f90`) were
checked with GFortran through a mechanical translation to standard Fortran
(`use, namespace :: L => M` becomes `use M, only: L__x => x, ...` and
`L%x` becomes `L__x`, with chains translated by hand). They are
registered in `integration_tests/CMakeLists.txt` with the `llvm` label. They
cannot carry the `gfortran` label, since GFortran does not support the
syntax. The error tests are registered in `tests/tests.toml`: the
semantic errors are the cases `cc_NN` (subroutines or modules, each with a
comment) of `tests/errors/namespace_continue_compilation.f90`, which is
checked with `--continue-compilation` so that every case is reported, and
the syntax errors are separate files `tests/errors/namespace_modules_NN.f90`,
since parsing stops at the first one. Also registered are the `--std`
warning test (`tests/warnings/namespace_modules_std_01.f90`), `ast_f90`
round trips of
`namespace_modules_05`, `_07`, `_20` and `_28` and the ASR of
`namespace_modules_01`.

| Test | Covers |
|---|---|
| `namespace_modules_01` | Variables, subroutines, functions; keyword and optional arguments; local names equal to member names |
| `namespace_modules_02` | Renamed module entities; several modules exporting the same names (J3/20-108 example); intrinsic `sin` unaffected |
| `namespace_modules_03` | Arrays, sections, allocatable arrays (including reallocation on assignment), pointers, `nullify`, `associated` |
| `namespace_modules_04` | Derived types: declarations, constructors, components, type-bound procedures, array constructor type-spec, `allocate` type-spec, `select type` |
| `namespace_modules_05` | `extends(L%t)`, abstract types with deferred bindings, parent component name (D5) |
| `namespace_modules_06` | Constant expressions: kinds, bounds, lengths, `parameter`, `case`, enumerators |
| `namespace_modules_07` | Intrinsic modules, including `c%c_f_pointer` and `c%c_f_procpointer`; modifier order; two names for one module |
| `namespace_modules_08` | Generic interfaces; generic with a type's name; specifics |
| `namespace_modules_09` | Procedures as actual arguments, procedure pointers, `procedure(L%iface)` |
| `namespace_modules_10` | Mixing with ordinary USE; several module entities for one module; repeated import (D7) |
| `namespace_modules_11` | Host association (internal and module procedures, BLOCK); shadowing |
| `namespace_modules_12` | Namespace imports local to procedures, BLOCK, external procedures |
| `namespace_modules_13` | Submodules |
| `namespace_modules_14` | Type-bound operators and assignment; explicit import of a defined operator |
| `namespace_modules_15` | Interface bodies: `IMPORT` of a module entity (D6) and local namespace import |
| `namespace_modules_16` | Members named like intrinsics, like the module entity, like another module entity |
| `namespace_modules_17` | Character variables, substrings, deferred length |
| `namespace_modules_18` | Input/output statements |
| `namespace_modules_19` | Elemental and pure procedures |
| `namespace_modules_20` | ASSOCIATE and SELECT TYPE selectors; `type is (L%t)` |
| `namespace_modules_21` | Fixed-form source |
| `namespace_modules_22` | Case insensitivity and blanks around `%` |
| `namespace_modules_23` | Local generic with the same name does not extend `L%gen` |
| `namespace_modules_24` | Facade module: chains `std%linalg%solve2` |
| `namespace_modules_25` | `use b` brings in `b`'s public module entity `a` |
| `namespace_modules_26` | `private` module + `public :: a`; `use b, only: a`; `only: other => a`; `b%a%x` |
| `namespace_modules_27` | The same module entity through two modules (D8) |
| `namespace_modules_28` | `use, namespace, private/public` (D9); identity of `a` and `b%a` (D8); `L%name` is not a rename |
| `namespace_modules_29` | Chains in type-specs, constant expressions, generic calls, constructors, `type is` |
| `namespace_modules_30` | Deferred type-bound procedure with `procedure(L%iface)`; `type(L%t)` in a BLOCK; `real(L%dp) function f()` |
| `namespace_modules_31` | A module entity named like a module that the host accesses through another module entity |
| `namespace_modules_32` | Types of a module with default `PRIVATE`: `public :: t`, `type, public :: u`, a generic constructor named like its type |
| `namespace_modules_33` | Separate compilation (`.mod` files): `use b` of a module with public module entities, `a%x`, `b%a%x`, `b%env%real64`; parent component `v%t` of a type that extends `a%t` in that module, which also has a variable `t` |

In the table of error tests, `cc_NN` is a case of
`errors/namespace_continue_compilation.f90`, and the other tests are
separate files in `errors/` (`namespace_modules_NN.f90`, and
`namespace_separate_*.f90`, whose modules are in
`namespace_separate_types.f90` and `namespace_separate_holder.f90`).

| Error test | Error |
|---|---|
| `cc_01` | Member used without qualification |
| `cc_02` | `only` with a namespace import |
| `namespace_modules_03` | Rename list with a namespace import |
| `cc_04` | No such member |
| `cc_05` | Private member |
| `cc_06` | Module entity used as a value |
| `cc_07` | Module entity as an actual argument |
| `cc_08` | Assignment to a module entity |
| `cc_09` | Module entity name clashes with a local entity |
| `cc_10` | One local name for two different modules |
| `cc_11` | Ambiguous reference: module entity vs use-associated variable |
| `cc_12` | Modifying a PROTECTED variable |
| `cc_13` | Qualifier with a module that was only used with plain USE (D2) |
| `cc_14` | Qualifier with a module that was not used |
| `cc_15` | `namespace` modifier repeated |
| `namespace_modules_16` | Missing `::` |
| `cc_17` | Module renamed in an ordinary USE |
| `cc_18` | Defined operator not imported by a namespace import |
| `namespace_modules_19` | Qualified name as a DO variable |
| `namespace_modules_20` | Qualified name as a literal kind parameter |
| `cc_21` | Module entity hidden by a local variable in an internal procedure |
| `cc_22` | Module imports itself |
| `cc_24` | Type used as a data object |
| `namespace_modules_25` | Declaring a qualified name |
| `cc_27` | Assignment to a named constant |
| `cc_28` | Private module entity of a module, accessed as `b%a%x` |
| `cc_29` | Two module entities named `a` for different modules, referenced |
| `cc_30` | Local generic does not extend `L%gen` |
| `cc_31` | Access-spec in a namespace import outside a module |
| `cc_32` | Two access-specs in a namespace import |
| `cc_33` | `use, namespace, private` entity imported with ONLY |
| `cc_34` | Module name as a qualifier in a type-spec, after a module entity for the same module was used |
| `cc_35` | Ambiguous module entity in a type-spec, named like a module whose type the host used |
| `cc_36` | Module entity as a type (`type(m)`) |
| `cc_37` | Variable as a type (`type(m%x)`) |
| `cc_38` | Module entity as a type (`class(m)`) |
| `cc_39` | Module entity as an interface (`procedure(m)`) |
| `cc_40` | Variable called as a subroutine (`call m%x()`) |
| `cc_41` | SAVE statement for a module entity |
| `cc_42` | PARAMETER statement for a module entity |
| `cc_43` | Generic function `g%gen(2)` in an array bound of a module variable (not a constant expression) |
| `cc_44` | Module entity as a parent type (`extends(m)`) |
| `cc_45` | Function as a parent type (`extends(g%gen_int)`) |
| `cc_46` | Structure constructor `l%u(1)` passed for a dummy argument of another type (the type is shown as `u`) |
| `cc_47` | Array constructor `[l%u :: ...]` passed for a scalar dummy argument of another type |
| `cc_48` | Structure constructor `l%u(1)` assigned to a variable of another type |
| `cc_49` | Module entity as the type-spec of ALLOCATE (`allocate(m :: y)`) |
| `cc_50` | Variable as the type-spec of ALLOCATE (`allocate(m%x :: y)`) |
| `cc_51` | Function as the type-spec of ALLOCATE (`allocate(g%gen_int :: y)`) |
| `cc_52` | Type mismatch in a procedure that imports the module under another local name than its sibling (the type is shown as `u`) |
| `cc_53` | Type mismatch where `l%u` in a sibling procedure designates the expected type (the type is shown as `u`) |
| `cc_54` | Component declared as `type(l%u)` in another module passed for a dummy argument of another type |
| `cc_55` | Structure constructor of a type renamed by an ordinary USE (`uu => u`) passed for a dummy argument of another type (the type is shown as `u`, as for a module entity) |
| `cc_56` | ALLOCATE of a component declared as `type(l%u)` in another module that is neither allocatable nor a pointer (the type is shown as `u`) |
| `cc_57` | DEALLOCATE of the structure constructor `l%u(1)` (the type is shown as `u`) |
| `namespace_separate_component` | As `cc_54`, with the modules compiled separately (`.mod` files) |
| `namespace_separate_submodule` | Structure constructor `l%u(1)` passed for a dummy argument of another type in a submodule whose separately compiled parent declared the symbol for `l%u` |
