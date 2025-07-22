package ir

import (
	"context"
	"fmt"
	"log/slog"
)

// slogExpr wraps an Expr as a slog.LogValuer to not render expression strings
// unless they definitely need to be logged
func slogExpr(expr Expr) slog.LogValuer {
	return exprLogValuer{expr}
}
func slogType(t Type) slog.LogValuer { return typeLogValuer{t} }

type exprLogValuer struct{ Expr }
type typeLogValuer struct{ Type }

func (l exprLogValuer) LogValue() slog.Value {
	return slog.GroupValue(
		slog.String("str", ExprString(l)),
		slog.String("hash", fmt.Sprintf("%x", l.Hash())),
		slog.String("pos", RangeOf(l).String()),
		slog.String("name", l.Describe()),
	)
}
func (l typeLogValuer) LogValue() slog.Value { return slog.StringValue(TypeString(l.Type)) }

// IleIRSlogHandler is a slog.Logger capable of lazy-printing expression trees and types
func IleIRSlogHandler(underlying slog.Handler) slog.Handler {
	return &exprLogHandler{underlying: underlying}
}

type exprLogHandler struct {
	underlying slog.Handler
}

func (l *exprLogHandler) Enabled(ctx context.Context, level slog.Level) bool {
	return l.underlying.Enabled(ctx, level)
}

func (l *exprLogHandler) Handle(ctx context.Context, record slog.Record) error {
	newRecord := slog.NewRecord(record.Time, record.Level, record.Message, record.PC)
	// for each attr, add it wrapped in slogExpr if it is an Any and then an Expr
	record.Attrs(func(attr slog.Attr) bool {
		if attr.Value.Kind() == slog.KindAny {
			switch value := attr.Value.Any().(type) {
			case Expr:
				newRecord.Add(attr.Key, slogExpr(value))
				return true
			case Type:
				newRecord.Add(attr.Key, slogType(value))
				return true
			}
		}
		newRecord.Add(attr)
		return true
	})
	return l.underlying.Handle(ctx, newRecord)
}

func (l *exprLogHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	// replace attrs with slogExpr attrs if they are Exprs
	for i, attr := range attrs {
		if attr.Value.Kind() == slog.KindAny {
			switch value := attr.Value.Any().(type) {
			case Expr:
				attr.Value = slog.AnyValue(slogExpr(value))
			case Type:
				attr.Value = slog.AnyValue(slogType(value))
			}
			attrs[i] = attr
		}
	}
	return IleIRSlogHandler(l.underlying.WithAttrs(attrs))
}

func (l *exprLogHandler) WithGroup(name string) slog.Handler {
	return IleIRSlogHandler(l.underlying.WithGroup(name))
}
