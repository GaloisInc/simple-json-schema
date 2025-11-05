# Overview

`simple-json-schema` is a tool for specifying JSON schemas, and validating that
JSON documents conform to the schema.   It is similar to other tools,
such as `JSON Schema`, but it aims for a more readable schema specification
language, based on TypeScript types.

# Usage

The most common way to use the tool are as follows:

```
# Check if `my-schema.ts` is a valid schema specification
simple-json-schema my-schema.ts

# Check if `my-doc.json` conforms to the schema described in `my-schema.ts`
simple-json-schema my-schema.ts --validate=my-doc.json
```

By default, the tool will validate against the first definition the schema
file.  To specify a different definition, you may use the `--entry` flag:

```
simple-json-schema my-schema.ts --validate=my-doc.json --entry=SomeDef
```

# Writing Schemas

A schema file is very similar to a TypeScript file containing a collection
of type synonyms.  The following grammar specifies the format of schemas,
describes the intended semantics, and points out deviations from TypeScript.


```
SCHEMA := IMPORT* TYPE_DEF*
    // The schema may contain comments as in TypeScript.
    // These are generally ignored, except line comments starting with
    // 3 slashes, which are doc-comments and may appear only before
    // definitions or field constraints as described below.

IMPORT :=

    import * as IDENT from STRING
    // Use all definitions from file STRING, with names qualified by IDENT.
    // For example, if "Other.ts" defines `x`, then
    // `import * as O from "Other.ts"` will introduce `O.x` in scope.
    // NOTE: This is different from TypeScript in that it does not refer
    // to the default export.
    
  | import { IDENT (,IDENT)* } from STRING
    // Use only the listed definitions from file STRING, with unqualified names.
    // For example, `import { a, b } from "Other.ts"` will introduce
    // definitions `a` and `b` defined in "Other.ts" to the current scope.


TYPE_DEF :=

    type IDENT = TYPE
    // Define a named type.
    // A definition may be preceded by some line doc-comments
    // (i.e., lines starting with `///`).
    // Definitions may be recursive, but only under a value constructor.
    // For example, `type X = [X] | boolean` is OK, but `type X = X | boolean`
    // is not.
    // NOTE: A deviation form TypeScript is that all definitions are considered
    // to be exported, so there is no need to write an explicit `export`.


TYPE :=

    IDENT | IDENT.IDENT
    // Match the values accepted by this named type.
 
  | LITERAL
    // Match a specific JSON value.

  | TYPE "|" TYPE
    // Match a value accepted by either type.
    // For example `"A" | "B"` matches either the JSON value "A" or "B".

  | boolean
    // Match any boolean JSON value.
    // Equivalent to `true | false`.
  
  | number
    // Match any numeric JSON value.
    
  | string
    // Match any string JSON value.

  | any
    // Match any JSON value.

  | {} | { FIELD (,FIELD)* }
    // Match a JSON object, whose fields match the given FIELDs.
    // The order of the fields is not important, and fields cannot
    // be repeated.

  | TYPE[]
    // Matches a JSON array, whose values match TYPE.
    // For example, `number[]` matches arrays with number elements.

  | "(" TYPE ")"
    // Parens may be used for grouping, as usual.
    // For example `("a" | "b")[]` matches arrays
    // whose elements are `"a"` or `"b"`

  | [] | [ TYPE (,TYPE)* ]
    // Matches JSON arrays of a fixed length, with elements matching types
    // in the corresponding positions.
    // For example `[boolean,string]` matches 2 element arrays, where the
    // first element should be a boolean, and the second one should be a string.

  | TYPE?
    // Equivalent to `TYPE | null`.
    // NOTE: this is not a valid TypeScript type.

LITERAL :=

    null
    // Matches the value `null`

  | false
    // Matches the value `false`

  | true
    Matches the value `true`

  | STRING
    // Matches this specific string literal in JSON format

  | NUMBER
    // Matches this specific numeric literal in JSON format

  
  FIELD :=

    FIELD_NAME : TYPE
    // The object must have a field with the given name and matching TYPE
    // A field may be preceded by some line doc-comments
    // (i.e., lines starting with `///`).

  | FIELD_NAME? : TYPE
    // The object is not required to have this field, but if it does,
    // then it should match TYPE
    // A field may be preceded by some line doc-comments
    // (i.e., lines starting with `///`).

  | ...
    // Matches any fields that are not matched by another FIELD in the
    // object specification.
    // For example `{ x: boolean, ... }` matches objects that must have
    // a boolean field name `x` and may have any other fields.
    // In contrast, `{ x: boolean }` matches objects that have exactly one
    // boolean field named `x`.
    // NOTE: This is not a valid TypeScript type.

FIELD_NAME := IDENT | STRING
    // Quotes in field name are optional if the field is just an identifier.
```