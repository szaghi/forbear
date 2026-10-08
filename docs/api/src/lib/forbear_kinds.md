---
title: forbear_kinds
---

# forbear_kinds

**Source**: `src/lib/forbear_kinds.F90`

## Contents

- [ucs4_string](#ucs4-string)

## Variables

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `ASCII` | integer | parameter |  |
| `UCS4` | integer | parameter |  |

## Functions

### ucs4_string

**Attributes**: pure

**Returns**: character(kind=[UCS4](/api/src/lib/forbear_kinds), len=:)

```fortran
function ucs4_string(input) result(output)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `input` | class(*) | in |  |  |

**Call graph**

```mermaid
flowchart TD
  build_frame["build_frame"] --> ucs4_string["ucs4_string"]
  complete["complete"] --> ucs4_string["ucs4_string"]
  create_spinner["create_spinner"] --> ucs4_string["ucs4_string"]
  draw["draw"] --> ucs4_string["ucs4_string"]
  initialize["initialize"] --> ucs4_string["ucs4_string"]
  update["update"] --> ucs4_string["ucs4_string"]
  write_message["write_message"] --> ucs4_string["ucs4_string"]
  style ucs4_string fill:#3e63dd,stroke:#99b,stroke-width:2px
```
