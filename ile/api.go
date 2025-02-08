package ile

import "io/fs"

type Options struct {
	IleSrc  fs.ReadFileFS
	DestDir string
}

