/*
 * Copyright (C) 2022 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package parse

import (
	"bufio"
	"io"
	"log"
	"os"
	"regexp"
	"strings"
)

// Assumes that we have just consumed the open brace from the lexer.
// This consumes all lexemes in the entire struct, including the outer end brace.
// It does not consume anything after the end brace.
func parseStructBody(lex *lexer) []Node {
	var members []Node
	append := func(node Node) {
		members = append(members, node)
	}
	parseMethod := func(name, returns, args item, isTemplate bool) *MethodNode {
		item := lex.nextItem()
		for item.typ == itemConst || item.typ == itemNoexcept {
			item = lex.nextItem()
		}
		method := &MethodNode{
			NodeType:   NodeMethod,
			Line:       Line(item.line),
			Name:       name.val,
			ReturnType: returns.val,
			Arguments:  args.val,
			Body:       "",
			IsTemplate: isTemplate,
		}
		switch item.typ {
		case itemMethodBody:
			method.Body = item.val
		case itemSemicolon:
		default:
			panic(lex, item)
			return nil
		}
		return method
	}
	for item := lex.nextItem(); item.typ != itemCloseBrace; item = lex.nextItem() {
		switch {
		case item.val == "enum":
			append(parseEnum(lex))
		case item.val == "struct":
			append(parseStruct(lex))
		case item.val == "class":
			append(parseClass(lex))
		case item.val == "namespace":
			append(parseNamespace(lex))
		case item.val == "using":
			append(parseUsing(lex))
		case item.val == "public", item.val == "private", item.val == "protected":
			expect(lex, itemColon)
			append(&AccessSpecifierNode{
				NodeType: NodeAccessSpecifier,
				Line:     Line(item.line),
				Access:   item.val,
			})
		case item.typ == itemCloseBrace:
			break
		case item.typ == itemSimpleType:
			name := expect(lex, itemIdentifier)
			nextItem := lex.nextItem()
			switch nextItem.typ {
			case itemMethodArgs:
				parseMethod(name, item, nextItem, false)
			case itemSemicolon:
				append(&FieldNode{
					NodeType:  NodeField,
					Line:      Line(item.line),
					Name:      name.val,
					FieldType: item.val,
					Rhs:       "",
				})
			case itemEquals:
				rhs := expect(lex, itemDefaultValue)
				expect(lex, itemSemicolon)
				append(&FieldNode{
					NodeType:  NodeField,
					Line:      Line(item.line),
					Name:      name.val,
					FieldType: item.val,
					Rhs:       rhs.val,
				})
			}
		case item.typ == itemTemplate:
			expect(lex, itemTemplateArgs)
			returns := expect(lex, itemSimpleType)
			name := expect(lex, itemIdentifier)
			args := expect(lex, itemMethodArgs)
			append(parseMethod(name, returns, args, true))
		default:
			panic(lex, item)
		}
	}
	return members
}

// Assumes that we have just consumed the "class" keyword from the lexer.
// Consumes everything up to (and including) the trailing semicolon.
func parseClass(lex *lexer) *ClassNode {
	name := expect(lex, itemIdentifier)
	item := lex.nextItem()
	if item.typ == itemSemicolon {
		// We don't have an AST node for forward declarations, just skip it.
		return nil
	}
	if item.typ == itemColon {
		// Only one base class is allowed.
		expect(lex, itemIdentifier)
		item = lex.nextItem()
	}
	if item.typ != itemOpenBrace {
		panic(lex, item)
	}
	members := parseStructBody(lex)
	expect(lex, itemCloseBrace)
	expect(lex, itemSemicolon)
	return &ClassNode{
		NodeType: NodeClass,
		Line:     Line(name.line),
		Name:     name.val,
		Members:  members,
	}
}

// Assumes that we have just consumed the "struct" keyword from the lexer.
// Consumes everything up to (and including) the trailing semicolon.
func parseStruct(lex *lexer) *StructNode {
	name := expect(lex, itemIdentifier)
	item := lex.nextItem()
	if item.typ == itemSemicolon {
		// We don't have an AST node for forward declarations, just skip it.
		return nil
	}
	if item.typ != itemOpenBrace {
		panic(lex, item)
	}
	members := parseStructBody(lex)
	expect(lex, itemSemicolon)
	return &StructNode{
		NodeType: NodeStruct,
		Line:     Line(name.line),
		Name:     name.val,
		Members:  members,
	}
}

// Assumes that we have just consumed the "enum" keyword from the lexer.
// Consumes everything up to (and including) the trailing semicolon.
func parseEnum(lex *lexer) *EnumNode {
	expect(lex, itemClass)
	name := expect(lex, itemIdentifier)
	item := lex.nextItem()
	if item.typ == itemColon {
		expect(lex, itemSimpleType)
		item = lex.nextItem()
	}
	if item.typ != itemOpenBrace {
		panic(lex, item)
	}
	firstVal := expect(lex, itemIdentifier)
	node := &EnumNode{
		Name:       name.val,
		NodeType:   NodeEnum,
		Line:       Line(name.line),
		Values:     []string{firstVal.val},
		ValueLines: []Line{Line(firstVal.line)},
	}
	for item = lex.nextItem(); item.typ != itemCloseBrace; {
		if item.typ != itemComma {
			panic(lex, item)
		}
		item = lex.nextItem()
		if item.typ == itemCloseBrace {
			break
		}
		if item.typ != itemIdentifier {
			panic(lex, item)
		}
		node.Values = append(node.Values, item.val)
		node.ValueLines = append(node.ValueLines, Line(item.line))
		item = lex.nextItem()
	}
	expect(lex, itemSemicolon)
	return node
}

// Assumes that we have just consumed the "using" keyword from the lexer.
// Consumes everything up to (and including) the trailing semicolon.
func parseUsing(lex *lexer) *UsingNode {
	name := expect(lex, itemIdentifier)
	expect(lex, itemEquals)
	rhs := expect(lex, itemSimpleType)
	expect(lex, itemSemicolon)
	return &UsingNode{NodeUsing, Line(name.line), name.val, rhs.val}
}

// Assumes that we have just consumed the "namespace" keyword from the lexer.
// Consumes everything up to (and including) the closing brace.
func parseNamespace(lex *lexer) *NamespaceNode {
	name := expect(lex, itemIdentifier)
	expect(lex, itemOpenBrace)
	ns := &NamespaceNode{NodeNamespace, Line(name.line), name.val, nil}
	item := lex.nextItem()

	// Filter out nil nodes (e.g. forward declarations)
	// Note that checking for nil is tricky due to a classic Go gotcha.
	append := func(child Node) {
		switch concrete := child.(type) {
		case *StructNode:
			if concrete == nil {
				return
			}
		case *ClassNode:
			if concrete == nil {
				return
			}
		}
		ns.Children = append(ns.Children, child)
	}

	for ; item.typ != itemCloseBrace; item = lex.nextItem() {
		switch item.typ {
		case itemClass:
			append(parseClass(lex))
		case itemStruct:
			append(parseStruct(lex))
		case itemEnum:
			append(parseEnum(lex))
		case itemUsing:
			append(parseUsing(lex))
		case itemNamespace:
			append(parseNamespace(lex))
		default:
			panic(lex, item)
		}
	}
	return ns
}

func parseRoot(lex *lexer) *RootNode {
	expect(lex, itemNamespace)
	ns := parseNamespace(lex)
	return &RootNode{NodeRoot, 0, ns}
}

func expect(lex *lexer, expectedType itemType) item {
	item := lex.nextItem()
	if item.typ != expectedType {
		panic(lex, item)
	}
	return item
}

func panic(lex *lexer, unexpected item) {
	lex.drain()
	// Very useful local hack: change this to Panicf to see a call stack.
	log.Fatalf("%d: parser sees unexpected lexeme %s", unexpected.line, unexpected.String())
}

func Parse(sourcePath string) []TypeDefinition {
	data, err := os.ReadFile(sourcePath)
	if err != nil {
		log.Fatal(err)
	}
	contents := string(data)
	lexer := createLexer(contents)
	root := parseRoot(lexer)

	// Gather all block-style comments.
	context := parserContext{}
	context.compileRegexps()

	// Line numbers are 1-based in beamsplitter, so we add an extra newline at the top.
	context.codelines = strings.Split("\n"+contents, "\n")
	context.commentBlocks = gatherCommentBlocks(strings.NewReader(contents))

	// Recurse into the AST using pre-order traversal, gathering type information along the way.
	context.gatherTypeInfo(root.Child, "", nil)

	context.addTypeQualifiers()

	return context.definitions
}

type TypeDefinition interface {
	BaseName() string
	QualifiedName() string
	Parent() TypeDefinition
}

type StructField struct {
	TypeString   string
	Name         string
	DefaultValue string
	Description  string
	EmitterFlags map[string]struct{}
	CustomType   TypeDefinition
}

type StructDefinition struct {
	name        string
	qualifier   string
	Fields      []StructField
	Description string
	parent      TypeDefinition
}

func (defn StructDefinition) BaseName() string       { return defn.name }
func (defn StructDefinition) QualifiedName() string  { return defn.qualifier + defn.name }
func (defn StructDefinition) Parent() TypeDefinition { return defn.parent }

type EnumValue struct {
	Description string
	Name        string
}

type EnumDefinition struct {
	name        string
	qualifier   string
	Values      []EnumValue
	Description string
	parent      TypeDefinition
}

func (defn EnumDefinition) BaseName() string       { return defn.name }
func (defn EnumDefinition) QualifiedName() string  { return defn.qualifier + defn.name }
func (defn EnumDefinition) Parent() TypeDefinition { return defn.parent }

type Documented interface{ GetDoc() string }

func (defn EnumDefinition) GetDoc() string   { return defn.Description }
func (defn StructDefinition) GetDoc() string { return defn.Description }
func (field StructField) GetDoc() string     { return field.Description }
func (value EnumValue) GetDoc() string       { return value.Description }

type generalScope struct{}

type scope interface {
	BaseName() string
	QualifiedName() string
	Parent() TypeDefinition
}

func (defn generalScope) BaseName() string       { return "" }
func (defn generalScope) QualifiedName() string  { return "" }
func (defn generalScope) Parent() TypeDefinition { return nil }

type parserContext struct {
	definitions      []TypeDefinition
	stack            []scope
	commentBlocks    map[int]string
	codelines        []string
	floatMatcher     *regexp.Regexp
	vectorMatcher    *regexp.Regexp
	fieldDocParser   *regexp.Regexp
	customFlagFinder *regexp.Regexp
}

// https://github.com/google/re2/wiki/Syntax
func (context *parserContext) compileRegexps() {
	context.floatMatcher = regexp.MustCompile(`(\-?[0-9]+\.[0-9]*)f?`)
	context.vectorMatcher = regexp.MustCompile(`\{(\s*\-?[0-9\.]+\s*(,\s*\-?[0-9\.]+\s*){1,})\}`)
	context.customFlagFinder = regexp.MustCompile(`\s*\%codegen_([a-zA-Z0-9_]+)\%\s*`)
	context.fieldDocParser = regexp.MustCompile(`(?://\s*\!\<\s*(.*))`)
}

// Creates a mapping from line numbers to strings, where the strings are entire block comments
// and the line numbers correspond to the last line of each block comment.
func gatherCommentBlocks(sourceFile io.Reader) map[int]string {
	comments := make(map[int]string)
	scanner := bufio.NewScanner(sourceFile)
	var comment = ""
	var indention = 0
	for lineNumber := 1; scanner.Scan(); lineNumber++ {
		codeline := scanner.Text()
		if strings.Contains(codeline, `/**`) {
			indention = strings.Index(codeline, `/**`)
			if strings.Contains(codeline, `*/`) {
				comments[lineNumber] = codeline[indention:] + "\n"
				continue
			}
			comment = codeline[indention:] + "\n"
			continue
		}
		if comment != "" {
			if len(codeline) > indention {
				codeline = codeline[indention:]
			}
			comment += codeline + "\n"
			if strings.Contains(codeline, `*/`) {
				comments[lineNumber] = comment
				comment = ""
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	return comments
}

// Annotates struct fields that have custom types (i.e. enums or structs).
func (context parserContext) addTypeQualifiers() {
	typeMap := make(map[string]TypeDefinition)
	for _, defn := range context.definitions {
		typeMap[defn.QualifiedName()] = defn
	}
	for _, defn := range context.definitions {
		structDefn, isStruct := defn.(*StructDefinition)
		if !isStruct {
			continue
		}
		for fieldIndex, field := range structDefn.Fields {
			// Extract the namespace prefix (if any) explicitly specified for this field type.
			var namespace string
			localTypeName := field.TypeString
			if index := strings.LastIndex(field.TypeString, "::"); index > -1 {
				namespace = field.TypeString[:index]
				localTypeName = field.TypeString[index+2:]
			}

			// Prepend additional qualifiers to the type string by searching upward through
			// the current namespace hierarchy, and looking for a match.
			mutable := &structDefn.Fields[fieldIndex]
			for ancestor := defn; ; ancestor = ancestor.Parent() {
				var qualified string
				if namespace != "" && strings.HasSuffix(ancestor.QualifiedName(), namespace) {
					qualified = ancestor.QualifiedName() + "::" + localTypeName
				} else {
					qualified = ancestor.QualifiedName() + "::" + field.TypeString
				}
				if fieldType, found := typeMap[qualified]; found {
					mutable.TypeString = qualified
					mutable.CustomType = fieldType
					break
				}
				if ancestor.Parent() == nil {
					if fieldType, found := typeMap[field.TypeString]; found {
						mutable.CustomType = fieldType
					}
					break
				}
			}

			if mutable.CustomType == nil {
				continue
			}

			// Prepend additional qualifiers to the value string if it is a known enum.
			var fieldType TypeDefinition = structDefn.Fields[fieldIndex].CustomType
			enumDefn, isEnum := fieldType.(*EnumDefinition)
			if isEnum {
				selectedEnum := field.DefaultValue
				if index := strings.LastIndex(field.DefaultValue, "::"); index > -1 {
					selectedEnum = field.DefaultValue[index+2:]
				}
				mutable.DefaultValue = enumDefn.QualifiedName() + "::" + selectedEnum
			}
		}
	}
}

// Validates and transforms the RHS of an assignment.
// For vectors, this converts curly braces into square brackets.
func (context parserContext) distillValue(cppvalue string, lineNumber int) string {
	cppvalue = strings.TrimSpace(cppvalue)

	// Remove trailing "f" from floats, which isn't allowed in JavaScript.
	if context.floatMatcher.MatchString(cppvalue) {
		cppvalue = context.floatMatcher.ReplaceAllString(cppvalue, "$1")
	}

	// There are many ways to declare vector values (multi-arg constructor, single-arg
	// constructor, curly braces with type, curly braces without type), so just poop out if
	// the syntax is anything other than "curly braces without type".
	if strings.Contains(cppvalue, "math::") || strings.Contains(cppvalue, "Color") {
		log.Fatalf("%d: vectors must have the form {x, y ...}", lineNumber)
	}

	// Assume it's a vector if there's a curly brace.
	if strings.Contains(cppvalue, "{") {
		if context.vectorMatcher.MatchString(cppvalue) {
			cppvalue = context.vectorMatcher.ReplaceAllString(cppvalue, "[$1]")
		} else {
			log.Fatalf("%d: vectors must have the form {x, y ...}", lineNumber)
		}
	}
	return cppvalue
}

func (context *parserContext) getDescription(line Line) string {
	desc := context.commentBlocks[int(line)-1]
	if desc == "" {
		codeline := context.codelines[int(line)]
		if matches := context.fieldDocParser.FindStringSubmatch(codeline); matches != nil {
			desc = matches[1]
		}
	}
	return context.customFlagFinder.ReplaceAllString(desc, "")
}

func (context *parserContext) getEmitterFlags(line Line) map[string]struct{} {
	codeline := context.codelines[int(line)]
	if matches := context.customFlagFinder.FindAllStringSubmatch(codeline, -1); matches != nil {
		result := make(map[string]struct{}, len(matches))
		for _, flag := range matches {
			result[flag[1]] = struct{}{}
		}
		return result
	}
	return nil
}

// Search for all enums and structs and gather them into a flat list of type definitions.
func (context *parserContext) gatherTypeInfo(node Node, prefix string, parent TypeDefinition) {
	switch concrete := node.(type) {
	case *NamespaceNode:
		// HACK: filament namespace is a special case, remove it from the type database.
		if concrete.Name != "filament" {
			prefix = prefix + concrete.Name + "::"
		}
		for _, child := range concrete.Children {
			context.gatherTypeInfo(child, prefix, parent)
		}
	case *EnumNode:
		defn := &EnumDefinition{
			name:        concrete.Name,
			qualifier:   prefix,
			Values:      make([]EnumValue, len(concrete.Values)),
			Description: context.getDescription(concrete.Line),
			parent:      parent,
		}
		for i, val := range concrete.Values {
			defn.Values[i] = EnumValue{
				Name:        val,
				Description: context.getDescription(concrete.ValueLines[i]),
			}
		}
		context.definitions = append(context.definitions, defn)
	case *StructNode:
		defn := &StructDefinition{
			name:        concrete.Name,
			qualifier:   prefix,
			Fields:      make([]StructField, 0),
			Description: context.getDescription(concrete.Line),
			parent:      parent,
		}
		prefix = prefix + concrete.Name + "::"
		for _, child := range concrete.Members {
			switch member := child.(type) {
			case *StructNode, *ClassNode, *EnumNode, *NamespaceNode:
				context.gatherTypeInfo(child, prefix, defn)
			case *FieldNode:
				defn.Fields = append(defn.Fields, StructField{
					TypeString:   member.FieldType,
					Name:         member.Name,
					DefaultValue: context.distillValue(member.Rhs, int(member.Line)),
					Description:  context.getDescription(member.Line),
					EmitterFlags: context.getEmitterFlags(member.Line),
				})
			}
		}
		context.definitions = append(context.definitions, defn)
	case *ClassNode:
		prefix = prefix + concrete.Name + "::"
		for _, child := range concrete.Members {
			switch child.(type) {
			case *StructNode, *ClassNode, *EnumNode, *NamespaceNode:
				context.gatherTypeInfo(child, prefix, parent)
			}
		}
	}
}
