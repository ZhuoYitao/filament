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

package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"runtime"

	"beamsplitter/parse"
)

const kCodelineMarker = "The remainder of this file is generated by beamsplitter"

func findFilamentRoot() string {
	var (
		_, b, _, _  = runtime.Caller(0)
		thisFolder  = filepath.Dir(b)
		toolsFolder = filepath.Dir(thisFolder)
	)
	return filepath.Dir(toolsFolder)
}

func main() {
	const sourceFilename = "Options.h"
	log.SetFlags(0)
	log.SetPrefix(sourceFilename + ":")
	root := findFilamentRoot()
	sourcePath := filepath.Join(root, "filament", "include", "filament", sourceFilename)
	definitions := parse.Parse(sourcePath)

	// For diagnostic purposes, this dumps out the database that was gathered from
	// the parsing phase.
	if len(os.Args) > 1 && os.Args[1] == "--verbose" {
		for _, defn := range definitions {
			switch concrete := defn.(type) {
			case *parse.StructDefinition:
				fmt.Println("STRUCT:", concrete.QualifiedName())
				for _, field := range concrete.Fields {
					fmt.Println("\t", field.TypeString, "...", field.Name, "...", field.DefaultValue)
				}
			case *parse.EnumDefinition:
				fmt.Println("  ENUM:", concrete.QualifiedName())
				for _, value := range concrete.Values {
					fmt.Println("\t", value)
				}
			}
		}
	}

	emitSerializer(definitions, filepath.Join(root, "libs", "viewer", "src"))

	jsfolder := filepath.Join(root, "web", "filament-js")
	emitJavaScript(definitions, "View", jsfolder)
	editTypeScript(definitions, "View", jsfolder)

	javafolder := filepath.FromSlash("com/google/android/filament")
	javafolder = filepath.Join(root, "android/filament-android/src/main/java", javafolder)
	editJava(definitions, "View", javafolder)

	fmt.Print(`
Note that this tool does not generate bindings for setter methods on
filament::View. If you added or renamed one of the setter methods, you
will likely need to manually modify the following files:

 - web/filament-js/extensions.js
 - web/filament-js/jsbindings.cpp
 - web/filament-js/filament.d.ts
 - filament/include/filament/View.h
 - filament/src/View.cpp
 - filament/src/details/View.cpp
 - android/filament-android/src/main/java/.../View.java
 - android/filament-android/src/main/cpp/View.cpp
`)
}
