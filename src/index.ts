import * as fs from 'fs'
import {
  DefinitionNode,
  parse,
  print,
  ObjectTypeDefinitionNode,
  DocumentNode,
  Kind,
  TypeDefinitionNode,
} from 'graphql'
import { flatten, groupBy, includes, keyBy, isEqual } from 'lodash'
import * as path from 'path'
import * as resolveFrom from 'resolve-from'
import { completeDefinitionPool, ValidDefinitionNode } from './definition'

/**
 * Describes the imports of a particular file.
 */
export interface RawModule {
  imports: string[]
  from: string
}

export interface Module {
  imports: string[]
  from: string
  definitions: ValidDefinitionNode[]
}

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
  schemas?: { [key: string]: string },
  processedImports?: string[],
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
  const modules: Module[] = rawModules.map(rawModule =>
    importModule(rawModule, schema, schemas),
  )

  const importedTypeDefinitions: ValidDefinitionNode[] = modules.reduce(
    (acc, module) => {
      return acc
    },
    [],
  )

  const importedRootTypeDefinitions = filterRootTypeDefinitions(
    importedTypeDefinitions,
  )
  const importedOtherTypeDefinitions = filterNonRootTypeDefinitions(
    importedTypeDefinitions,
  )

  /* Merge root types */

  const rootTypes = importedRootTypeDefinitions.reduce((acc, definition) => {
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
        ...(existingDefinition as ObjectTypeDefinitionNode).fields,
        ...definition.fields,
      ]
    }

    return acc
  }, rootTypeDefinitions)

  /* Bundles document */

  const bundledDocuments = {
    ...document,
    definitions: [
      ...rootTypes,
      ...otherTypeDefinitions,
      ...importedOtherTypeDefinitions,
    ],
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
  function isDefinition(
    a: ValidDefinitionNode,
    b: ValidDefinitionNode,
  ): boolean {
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
  ): Module {
    const key = isFile(schema) ? path.resolve(schema) : schema
    const moduleFilePath = resolveModuleFilePath(filePath, rawModule.from)

    /* Import schema */

    const importedSchema = importSchema(filePath, schemas)
    const importedSdl = read(importedSchema)
    const importedDocument = getDocumentFromSDL(importedSdl)
    const allDefinitions = filterTypeDefinitions(importedDocument.definitions)

    /* Filter imports */

    const importedDefinitions = []

    return {
      from: rawModule.from,
      imports: rawModule.imports,
      definitions: importedDefinitions,
    }
  }
}

/**
 * Filter the types loaded from a schema, first by relevant types,
 * then by the types specified in the import statement.
 *
 * @param imports Types specified in the import statement
 * @param typeDefinitions All definitions from a schema
 * @returns Filtered collection of type definitions
 */
function filterImportedDefinitions(
  imports: string[],
  typeDefinitions: ReadonlyArray<DefinitionNode>,
  allDefinitions: ValidDefinitionNode[][] = [],
): ValidDefinitionNode[] {
  // This should do something smart with fields

  const filteredDefinitions = filterTypeDefinitions(typeDefinitions)

  if (includes(imports, '*')) {
    if (
      imports.length === 1 &&
      imports[0] === '*' &&
      allDefinitions.length > 1
    ) {
      const previousTypeDefinitions: { [key: string]: DefinitionNode } = keyBy(
        flatten(allDefinitions.slice(0, allDefinitions.length - 1)).filter(
          def => !includes(rootFields, def.name.value),
        ),
        def => def.name.value,
      )
      return typeDefinitions.filter(
        typeDef =>
          typeDef.kind === 'ObjectTypeDefinition' &&
          previousTypeDefinitions[typeDef.name.value],
      ) as ObjectTypeDefinitionNode[]
    }
    return filteredDefinitions
  } else {
    const result = filteredDefinitions.filter(d =>
      includes(imports.map(i => i.split('.')[0]), d.name.value),
    )
    const fieldImports = imports.filter(i => i.split('.').length > 1)
    const groupedFieldImports = groupBy(fieldImports, x => x.split('.')[0])

    for (const rootType in groupedFieldImports) {
      const fields = groupedFieldImports[rootType].map(x => x.split('.')[1])
      ;(filteredDefinitions.find(
        def => def.name.value === rootType,
      ) as any).fields = (filteredDefinitions.find(
        def => def.name.value === rootType,
      ) as ObjectTypeDefinitionNode).fields.filter(
        f => includes(fields, f.name.value) || includes(fields, '*'),
      )
    }

    return result
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

  // Extract imported types
  const imports =
    wildcard === '*' ? ['*'] : importsString.split(',').map(d => d.trim())

  // Return information about the import line
  return { imports, from }
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
