[![scalafix-migrate-zio-macros Scala version support](https://index.scala-lang.org/virtuslabrnd/scalafix-migrate-zio-macros/scalafix-migrate-zio-macros/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/virtuslabrnd/scalafix-migrate-zio-macros/scalafix-migrate-zio-macros)

# Scalafix rules for ZIO macro annotations migration

This project contains set of rules allowing to migrate Scala 2 macro-annotations based utilities of ZIO ecosystem. 

## Available code generation rules: 

### `ZIOAccessibleCodeGen`
Rule detects usages of `zio.macros.accessible` annotations and generates accessors for annotated traits based on original macro-annonations algorithm.
Support: 
 - [x] - `zio.ZIO` (and it's main aliases) 
 - [x] - `zio.ZManaged` (and it's main aliases) 
 - [x] - `zio.ZStream`, `zio.ZSink` (and it's main aliases) 
 - [x] - accessors for normal methods
 - [x] - `@accessible` - supported
 - [ ] - `@accessibleM[_]` - not supported yet
 - [ ] - `@accessibleMM[_, _]`  - not supported yet
 - [ ] - `@throwing` - not supported yet

### `ZIOMockableCodeGen`
Rule detects usages of `zio.macros.mockable` annotations and generates mocks for annotated traits based on original macro-annonations algorithm.
Support: 
 - [x] - `zio.ZIO` (and it's main aliases) 
 - [x] - `zio.ZManaged` (and it's main aliases) 
 - [x] - `zio.ZStream`, `zio.ZSink` (and it's main aliases) 
 - [x] - overloaded methods
 - [x] - generic, type parametrized methods


## Usage
All rules require compilation with SemanticDB enabled and are targeting Scalafix 0.12.x
For information on how to use this projects refer to [Scalafix user guide](https://scalacenter.github.io/scalafix/docs/users/installation.html)

