package ast

import (
	"context"
	"log/slog"
)

// Slog wraps an Expr as a slog.LogValuer to not render expression strings
// unless they definitely need to be logged
func Slog(expr Expr) slog.LogValuer {
	return exprLogValuer{expr}
}

type exprLogValuer struct{ Expr }

func (l exprLogValuer) LogValue() slog.Value {
	return slog.StringValue(ExprString(l.Expr))
}

// ExprHandler is a slog.Logger capable of lazy-printing expression trees
func ExprHandler(underlying slog.Handler) slog.Handler {
	return &exprLogHandler{underlying: underlying}
}

func ExprLogger(underlying *slog.Logger) *slog.Logger {
	return slog.New(ExprHandler(underlying.Handler()))
}

type exprLogHandler struct {
	underlying slog.Handler
}

func (l *exprLogHandler) Enabled(ctx context.Context, level slog.Level) bool {
	return l.underlying.Enabled(ctx, level)
}

func (l *exprLogHandler) Handle(ctx context.Context, record slog.Record) error {
	newRecord := slog.NewRecord(record.Time, record.Level, record.Message, record.PC)
	// for each attr, add it wrapped in Slog if it is an Any and then an Expr
	record.Attrs(func(attr slog.Attr) bool {
		if attr.Value.Kind() == slog.KindAny {
			if asExpr, isExpr := attr.Value.Any().(Expr); isExpr {
				newRecord.Add(attr.Key, Slog(asExpr))
				return true
			}
		}
		newRecord.Add(attr)
		return true
	})
	return l.underlying.Handle(ctx, newRecord)
}

func (l *exprLogHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	// replace attrs with Slog attrs if they are Exprs
	for i, attr := range attrs {
		if attr.Value.Kind() == slog.KindAny {
			if asExpr, isExpr := attr.Value.Any().(Expr); isExpr {
				attr.Value = slog.AnyValue(Slog(asExpr))
				attrs[i] = attr
			}
		}
	}
	return ExprHandler(l.underlying.WithAttrs(attrs))
}

func (l *exprLogHandler) WithGroup(name string) slog.Handler {
	return ExprHandler(l.underlying.WithGroup(name))
}
