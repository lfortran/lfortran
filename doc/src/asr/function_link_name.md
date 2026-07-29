# Function.link_name

## Problem

A specific procedure that shares its generic interface name cannot occupy the
same symbol-table key as the `GenericProcedure`. LFortran stores the specific
under `"<name>~genericprocedure"`, so `Function.name` is the **disambiguated
key**, not necessarily the **external linkage name**.

For module procedures this is fine: definition and call both use the mangled
internal name. For **external interface bodies**, calls must link to the real
external symbol (`"<name>"`). The backend must not invent that name and must not
recover it by string-stripping a frontend convention.

## Design

`Function` gains an optional field:

```text
string? link_name
```

| Field | Meaning |
| --- | --- |
| `Function.name` | ASR / symbol-table identity (may include `~genericprocedure`) |
| `FunctionType.bindc_name` | BindC export name only (`bind(C, name=...)`) |
| `Function.link_name` | Object-file / external linkage name when it **differs** from `name` |

**Null `link_name`** means: use `name` (historical behaviour).

**Non-null `link_name`** means: for external interface (and BindC-without-`name=`)
lowering, emit/call this name instead of `name`.

Semantics sets `link_name` only when creating an interface-body Function that is
stored under `"<name>~genericprocedure"`. It does **not** set it for module
procedure implementations that share a generic name (both ends use the mangled
`name`).

## Verification

If `FunctionType.deftype == Interface`, `module == false`, and `name` ends with
`~genericprocedure`, then `link_name` must be present and equal to `name`
without that suffix.

## Backend contract

```text
if BindC and bindc_name set → use bindc_name
else if link_name set         → use link_name
else                          → use name (+ normal mangling rules)
```
