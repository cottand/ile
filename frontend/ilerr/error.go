package ilerr

import (
	"fmt"
	"log/slog"
)

type Errors struct {
	errs []IleError
}

func (r *Errors) With(err ...IleError) *Errors {
	if r == nil {
		return &Errors{errs: err}
	}
	for _, err := range err {
		r.errs = append(r.errs, err)
	}
	return r
}

func (r *Errors) Merge(err *Errors) *Errors {
	if r == nil {
		return err
	}
	if err == nil {
		return r
	}
	if len(err.errs) == 0 {
		return r
	}
	return r.With(err.errs...)
}

func (r *Errors) Errors() []IleError {
	return r.errs
}

func (r *Errors) HasError() bool {
	if r == nil {
		return false
	}
	return len(r.errs) > 0
}

func (r *Errors) LogValue() slog.Value {
	var vals []slog.Attr
	for i, v := range r.errs {
		vals = append(vals, slog.Attr{
			Key: fmt.Sprint("e", i),
			Value: slog.GroupValue(
				slog.Attr{
					Key:   "msg",
					Value: slog.StringValue(FormatWithCode(v)),
				},
			),
		})
	}
	return slog.GroupValue(vals...)
}
