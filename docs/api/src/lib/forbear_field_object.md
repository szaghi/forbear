---
title: forbear_field_object
---

# forbear_field_object

**Source**: `src/lib/forbear_field_object.F90`

**Dependencies**

```mermaid
graph LR
  forbear_field_object["forbear_field_object"] --> iso_fortran_env["iso_fortran_env"]
```

## Contents

- [progress_object](#progress-object)
- [field_object](#field-object)

## Derived Types

### progress_object

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `current` | real(kind=R8P) |  |  |
| `min_value` | real(kind=R8P) |  |  |
| `max_value` | real(kind=R8P) |  |  |
| `fraction` | real(kind=R8P) |  |  |
| `percent` | integer(kind=I4P) |  |  |
| `rate` | real(kind=R8P) |  |  |
| `elapsed` | real(kind=R8P) |  |  |
| `eta` | real(kind=R8P) |  |  |

### field_object

**Attributes**: abstract

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `render` | pass(self) |  |
