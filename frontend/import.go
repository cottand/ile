package frontend

import "golang.org/x/tools/go/packages"

var goImportConfig = &packages.Config{
	Mode: packages.NeedName | packages.NeedFiles | packages.NeedImports | packages.NeedDeps | packages.NeedSyntax,
}

func tmp() {
	// packages.Load(goImportConfig)
}
