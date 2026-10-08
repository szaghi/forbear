---
title: forbear_element_object
---

# forbear_element_object

**Source**: `src/lib/forbear_element_object.F90`

**Dependencies**

```mermaid
graph LR
  forbear_element_object["forbear_element_object"] --> face["face"]
  forbear_element_object["forbear_element_object"] --> forbear_kinds["forbear_kinds"]
```

## Contents

- [element_object](#element-object)
- [destroy](#destroy)
- [initialize](#initialize)
- [assign_element](#assign-element)
- [output](#output)

## Derived Types

### element_object

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `string` | character(kind=[UCS4](/api/src/third_party/FACE/src/lib/face), len=:) | allocatable |  |
| `color_fg` | character(len=:) | allocatable |  |
| `color_bg` | character(len=:) | allocatable |  |
| `style` | character(len=:) | allocatable |  |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `destroy` | pass(self) |  |
| `initialize` | pass(self) |  |
| `output` | pass(self) |  |
| `assignment(=)` |  |  |
| `assign_element` | pass(lhs) |  |

## Subroutines

### destroy

**Attributes**: pure

```fortran
subroutine destroy(self)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([element_object](/api/src/lib/forbear_element_object#element-object)) | inout |  |  |

**Call graph**

```mermaid
flowchart TD
  destroy["destroy"] --> destroy["destroy"]
  initialize["initialize"] --> destroy["destroy"]
  initialize["initialize"] --> destroy["destroy"]
  style destroy fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### initialize

**Attributes**: pure

```fortran
subroutine initialize(self, string, color_fg, color_bg, style)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([element_object](/api/src/lib/forbear_element_object#element-object)) | inout |  |  |
| `string` | class(*) | in | optional |  |
| `color_fg` | character(len=*) | in | optional |  |
| `color_bg` | character(len=*) | in | optional |  |
| `style` | character(len=*) | in | optional |  |

**Call graph**

```mermaid
flowchart TD
  create_spinner["create_spinner"] --> initialize["initialize"]
  initialize["initialize"] --> initialize["initialize"]
  initialize["initialize"] --> destroy["destroy"]
  style initialize fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### assign_element

**Attributes**: pure

```fortran
subroutine assign_element(lhs, rhs)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `lhs` | class([element_object](/api/src/lib/forbear_element_object#element-object)) | inout |  |  |
| `rhs` | type([element_object](/api/src/lib/forbear_element_object#element-object)) | in |  |  |

## Functions

### output

**Attributes**: pure

**Returns**: character(kind=[UCS4](/api/src/third_party/FACE/src/lib/face), len=:)

```fortran
function output(self)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([element_object](/api/src/lib/forbear_element_object#element-object)) | in |  |  |

**Call graph**

```mermaid
flowchart TD
  render["render"] --> output["output"]
  output["output"] --> colorize["colorize"]
  style output fill:#3e63dd,stroke:#99b,stroke-width:2px
```
