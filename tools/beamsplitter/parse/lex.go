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
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// item represents a token or text string returned from the scanner.
type item struct {
	typ  itemType // The type of this item.
	pos  int      // The starting position, in bytes, of this item in the input string.
	val  string   // The value of this item.
	line int      // The line number at the start of this item.
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.typ > itemKeyword:
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError                  itemType = iota // error occurred; value is text of error
	itemBlockCommentGroupBegin                 // starts with `/**`, ends with `*/`, contains `@{`
	itemBlockCommentGroupEnd                   // starts with `/**`, ends with `*/`, contains `@}`
	itemSimpleType                             // examples: `Texture* const`, `uint8_t`, `BlendMode`
	itemMethodBody                             // blob with the entire contents of an inlined method
	itemMethodArgs                             // unparsed blob, includes outermost with `()`
	itemTemplateArgs                           // unparsed blob, includes outermost with `<>`
	itemDefaultValue                           // an unparsed RHS expression
	itemIdentifier                             // legal C++ identifier
	itemEOF

	itemSymbol // unused enum separator
	itemOpenBrace
	itemCloseBrace
	itemSemicolon
	itemColon
	itemEquals

	itemKeyword // unused enum separator
	itemNamespace
	itemClass
	itemConst
	itemNoexcept
	itemStruct
	itemEnum
	itemTemplate
	itemPublic
	itemProtected
	itemPrivate
	itemUsing
)

const eof = -1

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	name              string    // the name of the input; used only for error reports
	input             string    // the entire contents of the file being scanned
	items             chan item // channel of scanned items
	line              int       // 1+number of newlines seen
	startLine         int       // start line of this item
	parenDepth        int       // nesting depth of () expressions
	braceDepth        int       // nesting depth of {} expressions
	angleBracketDepth int       // nesting depth of <> expressions
	pos               int       // current position in the input
	start             int       // start position of this item
	atEOF             bool      // we have hit the end of input
}

// next returns the next rune in the input.
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.atEOF = true
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += w
	if r == '\n' {
		l.line++
	}
	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// backup steps back one rune.
func (l *lexer) backup() {
	if !l.atEOF && l.pos > 0 {
		r, w := utf8.DecodeLastRuneInString(l.input[:l.pos])
		l.pos -= w
		// Correct newline count.
		if r == '\n' {
			l.line--
		}
	}
}

func (lex *lexer) backupMultiple(count int) {
	for i := 0; i < count; i++ {
		lex.backup()
	}
}

// emit passes an item back to the client.
func (l *lexer) emit(t itemType) {
	l.items <- item{t, l.start, l.input[l.start:l.pos], l.startLine}
	l.start = l.pos
	l.startLine = l.line
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.line += strings.Count(l.input[l.start:l.pos], "\n")
	l.start = l.pos
	l.startLine = l.line
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

func (lex *lexer) acceptAlphaNumeric() bool {
	next := lex.next()
	if isAlphaNumeric(next) {
		return true
	}
	lex.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
}

func (lex *lexer) acceptSpace() bool {
	return lex.accept(" \t\n")
}

func (l *lexer) acceptRune(expected rune) bool {
	if l.next() == expected {
		return true
	}
	l.backup()
	return false
}

func (lex *lexer) acceptString(expectedString string) bool {
	for i, c := range expectedString {
		if lex.next() != c {
			lex.backupMultiple(i)
			return false
		}
	}
	return true
}

func (lex *lexer) acceptIdentifier() bool {
	next := lex.next()
	if next != '_' && !unicode.IsLetter(next) {
		lex.backup()
		return false
	}
	for isAlphaNumeric(lex.next()) {
	}
	lex.backup()
	return true
}

func (lex *lexer) acceptKeyword(keyword string) bool {
	start := lex.pos
	if !lex.acceptString(keyword) {
		return false
	}
	if lex.eof() {
		return true
	}
	if lex.acceptAlphaNumeric() {
		lex.backupMultiple(lex.pos - start)
		return false
	}
	return true
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...any) stateFn {
	l.items <- item{itemError, l.start, fmt.Sprintf(format, args...), l.startLine}
	return nil
}

// nextItem returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) nextItem() item {
	return <-l.items
}

// drain drains the output so the lexing goroutine will exit.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) drain() {
	for range l.items {
	}
}

func (lex *lexer) eof() bool {
	return lex.pos >= len(lex.input)
}

// lex creates a new scanner for the input string.
func lex(name, input string) *lexer {
	l := &lexer{
		name:      name,
		input:     input,
		items:     make(chan item),
		line:      1,
		startLine: 1,
	}
	go l.run()
	return l
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexRootFn; state != nil; {
		state = state(l)
	}
	close(l.items)
}

func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// state functions

func lexRootFn(lex *lexer) stateFn {
	if lex.eof() {
		return nil
	}
	if lex.acceptSpace() {
		lex.acceptRun(" \n\t")
		return lexRootFn
	}
	if lex.acceptString("/*") {
		return lexBlockCommentFn(lex)
	}
	if lex.acceptString("//") {
		return lexLineCommentFn(lex)
	}
	if lex.acceptRune('#') {
		return lexEatLineFn(lex)
	}
	if lex.acceptKeyword("namespace") {
		lex.emit(itemNamespace)
		return lexNamespaceFn(lex)
	}
	return lex.errorf("Expected namespace")
}

// Upon entry we are just past the namespace keyword.
func lexNamespaceFn(lex *lexer) stateFn {
	if lex.acceptRune('{') {
		lex.emit(itemOpenBrace)
		return lexBlockFn(lex)
	}
	if lex.acceptIdentifier() {
		lex.emit(itemIdentifier)
		if lex.acceptRune('{') {
			lex.emit(itemOpenBrace)
			return lexBlockFn(lex)
		}
	}
	return lex.errorf("Badly formed namespace")
}

func lexBlockFn(lex *lexer) stateFn {
	if lex.acceptKeyword("namespace") {
		lex.emit(itemNamespace)
		return lexNamespaceFn(lex)
	}
	if lex.acceptKeyword("struct") {
		lex.emit(itemStruct)
		if lex.acceptIdentifier() {
			lex.emit(itemIdentifier)
		}
		if !lex.acceptRune('{') {
			return lex.errorf("Badly formed struct")
		}
		lex.emit(itemOpenBrace)
		return lexStruct(lex)
	}
	if lex.acceptKeyword("class") {
		lex.emit(itemClass)
		if !lex.acceptIdentifier() {
			return lex.errorf("Anonymous classes are illegal.")
		}
		lex.emit(itemIdentifier)
		if !lex.acceptRune('{') {
			return lex.errorf("Badly formed class")
		}
		lex.emit(itemOpenBrace)
		return lexClass(lex)
	}
	if lex.acceptKeyword("enum") {
		lex.emit(itemEnum)
		if !lex.acceptIdentifier() {
			return lex.errorf("Anonymous enums are illegal.")
		}
		lex.emit(itemIdentifier)
		if !lex.acceptRune('{') {
			return lex.errorf("Badly formed enum")
		}
		lex.emit(itemOpenBrace)
		return lexEnum(lex)
	}
	return lex.errorf("Expected namespace, struct, class, or enum.")
}

func lexLineCommentFn(lex *lexer) stateFn {
	if lex.eof() {
		return nil
	}
	if lex.accept("\n") {
		return lexRootFn
	}
	return lexLineCommentFn
}

func lexEatLineFn(lex *lexer) stateFn {
	if lex.eof() {
		return nil
	}
	if lex.accept("\n") {
		return lexRootFn
	}
	return lexEatLineFn
}

func lexBlockCommentFn(lex *lexer) stateFn {
	if lex.eof() {
		return lex.errorf("Unexpected EOF")
	}
	if lex.accept("*/") {
		return lexRootFn
	}
	return lexBlockCommentFn
}

func lexSymbolFn(lex *lexer) stateFn {
	switch lex.input[lex.pos] {
	case '{':
		lex.pos++
		lex.emit(itemOpenBrace)
		return lexRootFn
	case '}':
		lex.pos++
		lex.emit(itemCloseBrace)
		return lexRootFn
	case ';':
		lex.pos++
		lex.emit(itemSemicolon)
		return lexRootFn
	case ':':
		lex.pos++
		lex.emit(itemColon)
		return lexRootFn
	case '=':
		lex.pos++
		lex.emit(itemEquals)
		return lexRootFn
	}
	return lex.errorf("Unexpected input")
}

func lexSpaceFn(lex *lexer) stateFn {
	for unicode.IsSpace(rune(lex.input[lex.pos])) && !lex.eof() {
		lex.pos++
	}
	lex.ignore()
	return lexRootFn
}
