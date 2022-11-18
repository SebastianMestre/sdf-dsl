# Project purpose

This project implements an deep-embedded DSL in Haskell.

The language allows one to naturally express 3d shapes, and get an SDF for that
shape out as a result. In particular, we can currently compile shape descriptions
into SDF definitions in either GLSL or ECMAScript.

The ECMAScript SDFs we generate depend upon a runtime library, which is not
included within this project.

# Project structure

## Data structures

The project uses multiple intermediate languages that range across the low-level
to high-level spectrum, to represent shapes and SDFs.

The first language it uses, called `Shape` within the code, is meant to be
directly manipulated by users. This language has concepts that are familiar to 3D
digital artists, such as points, extrusions, translations, unions and more.

The second language, called `Form` (short for 'formula') is used for symbolic
manipulation of SDFs, and it's what the bulk of the project focuses on. This
language includes concepts that are familiar to programmers and computer
scientists, such as function application and let-bindings. This language is meant
for internal usage only.

The third language, called `Tac` (short for 'Three Address Code') is an SSA form
language, suitable for translating into our target languages.

## Passes

The compilation process is split into several passes, some of which convert
expressions from one intermediate language to the next. What follows is a list of
the major passes, and a short description of each one.

- Expand: Converts `Shape` expressions into a `Form` expressions. This pass
defines what the SDF of each primitive is.
- Typecheck: Decorates `Form` expressions with type annotations, which are used
during code generation.
- Lower: Converts `Form` expressions into sequences of equivalent `Tac` programs.
- Codegen: Converts `Tac` programs to equivalent GLSL or ECMAScript programs.
