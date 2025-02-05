package log

import (
	"context"
	"log/slog"
	"os"
	"slices"
)

var enabledSections = []string{
	//"frontend",
	//"desugar",
	"package",
}

var LoggerOpts = &slog.HandlerOptions{
	AddSource: true,
	Level:     slog.LevelDebug,
	ReplaceAttr: func(groups []string, a slog.Attr) slog.Attr {
		if a.Key == "time" {
			return slog.Attr{}
		}
		return a
	},
}

var DefaultLogger = slog.New(&filteringHandler{underlying: slog.NewTextHandler(os.Stdout, LoggerOpts)})

var _ slog.Handler = &filteringHandler{}

type filteringHandler struct {
	underlying slog.Handler
	sections   []string
}

func (f filteringHandler) Enabled(ctx context.Context, level slog.Level) bool {
	return f.underlying.Enabled(ctx, level)
}

func (f filteringHandler) Handle(ctx context.Context, record slog.Record) error {
	if record.Level >= slog.LevelWarn {
		return f.underlying.Handle(ctx, record)
	}
	// first filter out records which do not match enabledSections
	if !slices.ContainsFunc(f.sections, func(s string) bool {
		return slices.Contains(f.sections, s)
	}) {
		return nil
	}
	return f.underlying.Handle(ctx, record)
}

func (f filteringHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	var newAttrs []slog.Attr
	var sections []string

	// keep the section attribute in filteringHandler
	for _, attr := range attrs {
		if attr.Key == "section" && slices.ContainsFunc(enabledSections, func(section string) bool {
			return section == attr.Value.String()
		}) {
			sections = append(sections, attr.Value.String())
		} else {
			newAttrs = append(newAttrs, attr)
		}
	}
	return &filteringHandler{
		underlying: f.underlying.WithAttrs(newAttrs),
		sections:   sections,
	}
}

func (f filteringHandler) WithGroup(name string) slog.Handler {
	return &filteringHandler{
		underlying: f.underlying.WithGroup(name),
		sections:   f.sections,
	}
}
