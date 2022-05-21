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

type Node interface {
	Type() NodeType
}

type NodeType int
type Line int

func (t NodeType) Type() NodeType {
	return t
}

const (
	NodeRoot NodeType = iota
	NodeNamespace
	NodeClass
	NodeStruct
	NodeEnum
	NodeUsing
	NodeAccessSpecifier
	NodeGroupingDelimiter
	NodeMethod
	NodeField
)

type RootNode struct {
	NodeType
	Line
	Child *NamespaceNode
}

type NamespaceNode struct {
	NodeType
	Line
	Name     string
	Children []Node
}

type ClassNode struct {
	NodeType
	Line
	Name    string
	Members []Node
}

type StructNode struct {
	NodeType
	Line
	Name    string
	Members []Node
}

type EnumNode struct {
	NodeType
	Line
	Name       string
	Values     []string
	ValueLines []Line // used to find docstring for each enum value
}

type UsingNode struct {
	NodeType
	Line
	Name string
	Rhs  string
}

type AccessSpecifierNode struct {
	NodeType
	Line
	Access string
}

type GroupingDelimiterNode struct {
	NodeType
	Line
	DocString        string
	OpeningDelimiter bool
	ClosingDelimiter bool
}

type MethodNode struct {
	NodeType
	Line
	Name       string
	ReturnType string
	Arguments  string
	Body       string
	IsTemplate bool
}

type FieldNode struct {
	NodeType
	Line
	Name      string
	FieldType string
	Rhs       string
}
