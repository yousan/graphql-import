import * as fs from 'fs'
import {
  DefinitionNode,
  parse,
  print,
  ObjectTypeDefinitionNode,
  DocumentNode,
  Kind,
  DirectiveDefinitionNode,
  TypeDefinitionNode,
  isObjectType,
  isInterfaceType,
  isInputObjectType,
} from 'graphql'
import { flatten } from 'lodash'
import * as path from 'path'
import * as resolveFrom from 'resolve-from'

export type ValidDefinitionNode = DirectiveDefinitionNode | TypeDefinitionNode

/**
 * Describes the imports of a particular file.
 */
export type RawModule = {
  from: string
  imports:
    | ImportAll
    | (
        | DirectiveImport
        | ScalarImport
        | ObjectImport
        | InterfaceImport
        | EnumImport
        | UnionImport
        | InputImport)[]
}
export type ImportAll = '*'
export type DirectiveImport = string
export type ScalarImport = string
export type ObjectImport = string | { type: string; field: string }
export type InterfaceImport = string | { type: string; field: string }
export type EnumImport = string
export type UnionImport = string
export type InputImport = string

/**
 *
 * Imports schema and resolves imports from adjacent schemas.
 * Returns a bundled schema.
 *
 * Merges Query, Mutation and Subscription fields in adjacent schemas
 * into one inside the imported schema if particular root type is present
 * in the imported schema.
 * Other types follow the paradigm of last imported and are not merged.
 *
 * @param schema The imported schema.
 * @param schemas A collection of schemas.
 * @param path The path of the imports.
 */
export function importSchema(
  schema: string,
  schemas: { [key: string]: string } = {},
  processedImports: string[] = [],
): string {
  /* Parse the imported schema */

  const sdl = read(schema, schemas) || schema
  const document = getDocumentFromSDL(sdl)

  /* Process imported schema */

  const typeDefinitions = filterTypeDefinitions(document.definitions)
  const rootTypeDefinitions = filterRootTypeDefinitions(typeDefinitions)
  const otherTypeDefinitions = filterNonRootTypeDefinitions(typeDefinitions)

  /* Collect definitions from the imports */

  const rawModules: RawModule[] = parseSDL(sdl)
  const modules: ValidDefinitionNode[][] = rawModules.map(rawModule =>
    importModule(rawModule, schema, schemas),
  )

  const importedTypeDefinitions: ValidDefinitionNode[] = flatten(modules)

  const importedRootTypeDefinitions = filterRootTypeDefinitions(
    importedTypeDefinitions,
  )
  const importedOtherTypeDefinitions = filterNonRootTypeDefinitions(
    importedTypeDefinitions,
  )

  /* Merge root types */

  debugger

  const rootTypes = [
    ...rootTypeDefinitions,
    ...importedRootTypeDefinitions,
  ].reduce<ObjectTypeDefinitionNode[]>((acc, definition) => {
    /**
     * Find existing definition (bottom case from the imported schema)
     * and merge it with the imported one.
     */
    const existingDefinition = acc.find(existingDefinition =>
      isDefinition(existingDefinition, definition),
    )

    if (existingDefinition) {
      /* Merge definitions */
      ;(existingDefinition as any) /* Overrides readonly */.fields = [
        ...existingDefinition.fields,
        ...definition.fields,
      ]

      return acc
    } else {
      return [...acc, definition]
    }
  }, [])

  /* Merge other imported types */

  const otherTypes = [
    ...otherTypeDefinitions,
    ...importedOtherTypeDefinitions,
  ].reduceRight<ValidDefinitionNode[]>((acc, definition) => {
    const existingDefinition = acc.find(existingDefinition =>
      isDefinition(existingDefinition, definition),
    )

    if (existingDefinition) {
      return acc
      // if (
      //   definition.kind === 'DirectiveDefinition' ||
      //   definition.kind === 'ScalarTypeDefinition' ||
      //   definition.kind === 'EnumTypeDefinition' ||
      //   definition.kind === 'UnionTypeDefinition' ||
      //   /* Because TypeScript is made by idiots */
      //   existingDefinition.kind === 'DirectiveDefinition' ||
      //   existingDefinition.kind === 'ScalarTypeDefinition' ||
      //   existingDefinition.kind === 'EnumTypeDefinition' ||
      //   existingDefinition.kind === 'UnionTypeDefinition'
      // ) {
      //   return acc
      // }

      // /* Merge definitions */
      // ;(existingDefinition as any) /* Overrides readonly */.fields = [
      //   ...existingDefinition.fields,
      //   ...definition.fields,
      // ]
    }

    return [...acc, definition]
  }, [])

  /* Bundles document */

  const bundledDocuments = {
    ...document,
    definitions: [...rootTypes, ...otherTypes],
  }

  return print(bundledDocuments)

  /**
   * Helper functions which are only available in the context
   * of this function and serve the execution of the imports.
   */

  /**
   *
   * Reads a schema.
   *
   * @param schema
   * @param schemas
   */
  function read(schema: string, schemas?: { [key: string]: string }) {
    if (isFile(schema)) {
      return fs.readFileSync(schema, { encoding: 'utf8' })
    }
    return schemas ? schemas[schema] : schema
  }

  /**
   *
   * Determines whether a path points to a file.
   *
   * @param f
   */
  function isFile(f: string): boolean {
    return f.endsWith('.graphql')
  }

  /**
   * Check if a schema contains any type definitions at all.
   *
   * @param sdl Schema to parse
   * @returns True if SDL only contains comments and/or whitespaces
   */
  function isEmptySDL(sdl: string): boolean {
    return (
      sdl
        .split('\n')
        .map(l => l.trim())
        .filter(l => !(l.length === 0 || l.startsWith('#'))).length === 0
    )
  }

  /**
   * Parses a schema into a graphql DocumentNode.
   * If the schema is empty a DocumentNode with empty definitions will be created.
   *
   * @param sdl Schema to parse
   * @returns A graphql DocumentNode with definitions of the parsed sdl.
   */
  function getDocumentFromSDL(sdl: string): DocumentNode {
    if (isEmptySDL(sdl)) {
      return {
        kind: Kind.DOCUMENT,
        definitions: [],
      }
    } else {
      return parse(sdl, { noLocation: true })
    }
  }

  /**
   * Resolve the path of an import.
   * First it will try to find a file relative from the file the import is in,
   * if that fails it will try to resolve it as a module
   * so imports from packages work correctly.
   *
   * @param filePath Path the import was made from
   * @param importFrom Path given for the import
   * @returns Full resolved path to a file
   */
  function resolveModuleFilePath(filePath: string, importFrom: string): string {
    const dirname = path.dirname(filePath)
    if (isFile(filePath) && isFile(importFrom)) {
      try {
        return fs.realpathSync(path.join(dirname, importFrom))
      } catch (e) {
        if (e.code === 'ENOENT') {
          return resolveFrom(dirname, importFrom)
        }
      }
    }

    return importFrom
  }

  /**
   *
   * Determines whether two definitions define the same thing.
   *
   * @param a
   * @param b
   */
  function isDefinition<
    X extends ValidDefinitionNode,
    Y extends ValidDefinitionNode
  >(a: X, b: Y): b is Y {
    return a.name.value === b.name.value
  }

  /**
   * Filter relevant definitions from schema
   *
   * @param definitions All definitions from a schema
   * @returns Relevant type definitions
   */
  function filterTypeDefinitions(
    definitions: ReadonlyArray<DefinitionNode>,
  ): ValidDefinitionNode[] {
    const validKinds = [
      'DirectiveDefinition',
      'ScalarTypeDefinition',
      'ObjectTypeDefinition',
      'InterfaceTypeDefinition',
      'EnumTypeDefinition',
      'UnionTypeDefinition',
      'InputObjectTypeDefinition',
    ]
    return definitions
      .filter(d => validKinds.includes(d.kind))
      .map(d => d as ValidDefinitionNode)
  }

  /**
   * Returns rootTypes.
   */
  function filterRootTypeDefinitions(
    definitions: ValidDefinitionNode[],
  ): ObjectTypeDefinitionNode[] {
    const rootTypes = ['Query', 'Mutation', 'Subscription']

    return definitions
      .filter(
        d =>
          d.kind === 'ObjectTypeDefinition' && rootTypes.includes(d.name.value),
      )
      .map(d => d as ObjectTypeDefinitionNode)
  }

  /**
   * Returns rootTypes.
   */
  function filterNonRootTypeDefinitions(
    definitions: ValidDefinitionNode[],
  ): ValidDefinitionNode[] {
    const rootTypes = ['Query', 'Mutation', 'Subscription']

    return definitions.filter(d => !rootTypes.includes(d.name.value))
  }

  function importModule(
    rawModule: RawModule,
    filePath: string,
    schemas?: { [key: string]: string },
  ): ValidDefinitionNode[] {
    const key = isFile(schema) ? path.resolve(schema) : schema

    /* Break circular dependencies */

    if (processedImports.includes(key)) {
      return []
    }

    const moduleFilePath = resolveModuleFilePath(filePath, rawModule.from)

    /* Import schema */

    const importedSchema = importSchema(moduleFilePath, schemas, [
      ...processedImports,
      key,
    ])
    const importedDocument = getDocumentFromSDL(importedSchema)
    const allDefinitions = filterTypeDefinitions(importedDocument.definitions)

    /* Filter imports */

    const imports = rawModule.imports

    if (imports === '*') {
      return allDefinitions
    }

    const importedDefinitions = imports.reduce<ValidDefinitionNode[]>(
      (acc, _import) => {
        /* Handle import */

        switch (typeof _import) {
          case 'object': {
            const parsedImport = `${_import.type}.${_import.field}`

            const definition = allDefinitions.find(
              definition => definition.name.value === _import.type,
            )

            if (!definition) {
              throw new Error(
                `Couldn't find ${_import.type} in ${moduleFilePath}.`,
              )
            }

            const validKinds = [
              'ObjectTypeDefinition',
              'InterfaceTypeDefinition',
              'InputObjectTypeDefinition',
            ]

            if (!validKinds.includes(definition.kind)) {
              throw new Error(`Couldn't import ${parsedImport}.`)
            }

            const field = (definition as any).fields.find(
              field => field.name.value === _import.field,
            )

            if (!field) {
              throw new Error(
                `Couldn't find ${parsedImport} in ${moduleFilePath}.`,
              )
            }

            const newDefinition = {
              ...definition,
              fields: [field],
            }

            return [...acc, newDefinition]
          }
          case 'string': {
            const definition = allDefinitions.find(
              definition => definition.name.value === _import,
            )

            if (!definition) {
              throw new Error(`Couldn't find ${_import} in ${moduleFilePath}.`)
            }

            return [...acc, definition]
          }
        }
      },
      [],
    )

    return importedDefinitions
  }
}

/**
 * Parse a single import line and extract imported types and schema filename
 *
 * @param importLine Import line
 * @returns Processed import line
 */
export function parseImportLine(importLine: string): RawModule {
  // Apply regex to import line
  const matches = importLine.match(/^import (\*|(.*)) from ('|")(.*)('|");?$/)
  if (!matches || matches.length !== 6 || !matches[4]) {
    throw new Error(`Too few regex matches: ${matches}`)
  }

  // Extract matches into named variables
  const [, wildcard, importsString, , from] = matches

  /* Wildcard */

  if (wildcard === '*') return { imports: '*', from }

  /* Selectors */

  const imports = importsString.split(',').map(d => d.trim())
  const parsedImports = imports.map(i => {
    /**
     * Parses particular import. The only two options are
     * either importing a type/directive... "name" (ex. "Query")
     * or a specific field  "name.field" (ex. "Query.hello")
     */
    const iMatches = i.match(/(^\w+$)|(?:^(\w+)\.(\w+)$)/)
    const [, type, sType, sField] = iMatches

    if (!type && (!sType || !sField)) {
      throw new Error(`Faulty import ${i}.`)
    }

    if (type) return type

    return { type: sType, field: sField }
  })

  return { imports: parsedImports, from }
}

/**
 * Parse a schema and analyze all import lines
 *
 * @param sdl Schema to parse
 * @returns Array with collection of imports per import line (file)
 */
export function parseSDL(sdl: string): RawModule[] {
  return sdl
    .split('\n')
    .map(l => l.trim())
    .filter(l => l.startsWith('# import ') || l.startsWith('#import '))
    .map(l => l.replace('#', '').trim())
    .map(parseImportLine)
}
