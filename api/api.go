package api

import "io/fs"

type Options struct {
	IleSrc  fs.ReadFileFS
	DestDir string
}

func AssembleTranspiledGo(ops *Options) error {
	// fronted will need to return an ile package

	// then backend can take that and return a proper go tree

	// (optional) then we can validate it by loading the go package

	return nil
}
