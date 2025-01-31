package log

import (
	"context"
	"log/slog"
	"os"
	"slices"
	"strings"
)

var enabledSections = []string{
	//"frontend",
}

var LoggerOpts = &slog.HandlerOptions{
	AddSource: false,
	Level:     slog.LevelDebug,
	ReplaceAttr: func(groups []string, a slog.Attr) slog.Attr {
		if a.Key == "time" {
			return slog.Attr{}
		}
		return a
	},
}

var DefaultLogger = slog.New(&filteringHandler{underlying: slog.NewTextHandler(os.Stderr, LoggerOpts)})

func init() {
	DefaultLogger.Debug("YOLO")
}

var _ slog.Handler = &filteringHandler{}

type filteringHandler struct {
	underlying slog.Handler
}

func (f filteringHandler) Enabled(ctx context.Context, level slog.Level) bool {
	return f.underlying.Enabled(ctx, level)
}

func (f filteringHandler) Handle(ctx context.Context, record slog.Record) error {
	// first filter out records which do not match enabledSections
	wantSection := false
	record.Attrs(func(attr slog.Attr) bool {
		wantSection = wantSection || attr.Key == "section" && slices.ContainsFunc(enabledSections, func(section string) bool {
			return strings.HasPrefix(attr.Value.String(), section)
		})
		// iterate as long as we have not found our section
		return !wantSection
	})
	if !wantSection {
		return nil
	}
	return f.underlying.Handle(ctx, record)
}

func (f filteringHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	return &filteringHandler{f.underlying.WithAttrs(attrs)}
}

func (f filteringHandler) WithGroup(name string) slog.Handler {
	return &filteringHandler{f.underlying.WithGroup(name)}
}
