package source

import (
	"context"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sort"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/types/typeutil"
	"golang.org/x/tools/internal/lsp/telemetry/log"
	"golang.org/x/tools/internal/lsp/telemetry/trace"
	"golang.org/x/tools/internal/span"
	errors "golang.org/x/xerrors"
)

// WalkFunc is a package walk function
type WalkFunc func(Package) bool

// SearchFunc is a search global package cache function
type SearchFunc func(WalkFunc)

// Implementation returns a list of references for a given identifier within the packages
// containing i.File. Declarations appear first in the result.
func (i *IdentifierInfo) Implementation(ctx context.Context, search SearchFunc, pos token.Pos) ([]span.Span, error) {
	ctx, done := trace.StartSpan(ctx, "source.Implementation")
	defer done()
	log.Print(ctx, "called i.Implementation")

	file, err := i.File.GetAST(ctx, ParseFull)
	if err != nil {
		return nil, err
	}
	if i.pkg == nil || i.pkg.IsIllTyped() {
		return nil, errors.Errorf("package for %s is ill typed", i.File.URI())
	}

	path, _ := astutil.PathEnclosingInterval(file, pos, pos)
	if path == nil {
		return nil, errors.New("cannot find node enclosing position")
	}

	return i.implements(ctx, search, path)
}

// Adapted from golang.org/x/tools/cmd/guru (Copyright (c) 2013 The Go Authors). All rights
// reserved. See NOTICE for full license.
func (i *IdentifierInfo) implements(ctx context.Context, search SearchFunc, path []ast.Node) ([]span.Span, error) {
	var (
		T      types.Type // selected type (receiver if method != nil)
		method *types.Func
		a      action
	)
	path, a = findInterestingNode(i.pkg, path)
	log.Print(ctx, "called i.implements")

	switch a {
	case actionExpr:
		// method?
		if id, ok := path[0].(*ast.Ident); ok {
			if obj, ok := i.pkg.GetTypesInfo().ObjectOf(id).(*types.Func); ok {
				recv := obj.Type().(*types.Signature).Recv()
				if recv == nil {
					return nil, errors.New("this function is not a method")
				}
				method = obj
				T = recv.Type()
			}
		}

		// If not a method, use the expression's type.
		if T == nil {
			T = i.pkg.GetTypesInfo().TypeOf(path[0].(ast.Expr))
		}

	case actionType:
		T = i.pkg.GetTypesInfo().TypeOf(path[0].(ast.Expr))
	}
	if T == nil {
		return nil, errors.New("not a type, method, or value")
	}

	// Find all named types, even local types (which can have
	// methods due to promotion) and the built-in "error".
	// We ignore aliases 'type M = N' to avoid duplicate
	// reporting of the Named type N.
	var allNamed []*types.Named

	walk := func(p Package) bool {
		log.Print(ctx, fmt.Sprint(p))
		for _, obj := range p.GetTypesInfo().Defs {
			if obj, ok := obj.(*types.TypeName); ok && !isAlias(obj) {
				if named, ok := obj.Type().(*types.Named); ok {
					allNamed = append(allNamed, named)
				}
			}
		}

		return false
	}
	search(walk)

	allNamed = append(allNamed, types.Universe.Lookup("error").Type().(*types.Named))

	var msets typeutil.MethodSetCache

	// Test each named type.
	var to, from, fromPtr []types.Type
	for _, U := range allNamed {
		if isInterface(T) {
			if msets.MethodSet(T).Len() == 0 {
				continue // empty interface
			}
			if isInterface(U) {
				if msets.MethodSet(U).Len() == 0 {
					continue // empty interface
				}

				// T interface, U interface
				if !types.Identical(T, U) {
					if types.AssignableTo(U, T) {
						to = append(to, U)
					}
					if types.AssignableTo(T, U) {
						from = append(from, U)
					}
				}
			} else {
				// T interface, U concrete
				if types.AssignableTo(U, T) {
					to = append(to, U)
				} else if pU := types.NewPointer(U); types.AssignableTo(pU, T) {
					to = append(to, pU)
				}
			}
		} else if isInterface(U) {
			if msets.MethodSet(U).Len() == 0 {
				continue // empty interface
			}

			// T concrete, U interface
			if types.AssignableTo(T, U) {
				from = append(from, U)
			} else if pT := types.NewPointer(T); types.AssignableTo(pT, U) {
				fromPtr = append(fromPtr, U)
			}
		}
	}

	// Sort types (arbitrarily) to ensure test determinism.
	sort.Sort(typesByString(to))
	sort.Sort(typesByString(from))
	sort.Sort(typesByString(fromPtr))

	seen := map[types.Object]struct{}{}
	toSpanFn := func(t types.Type, method *types.Func) *span.Span {
		var obj types.Object
		if method == nil {
			// t is a type
			nt, ok := deref(t).(*types.Named)
			if !ok {
				return nil // t is non-named
			}
			obj = nt.Obj()
		} else {
			// t is a method
			tm := types.NewMethodSet(t).Lookup(method.Pkg(), method.Name())
			if tm == nil {
				return nil // method not found
			}
			obj = tm.Obj()
			if _, seen := seen[obj]; seen {
				return nil // already saw this method, via other embedding path
			}
			seen[obj] = struct{}{}
		}

		loc := toSpan(i.File.FileSet(), obj.Pos(), obj.Name())
		return &loc
	}

	locs := make([]span.Span, 0, len(to)+len(from)+len(fromPtr))
	for _, t := range to {
		loc := toSpanFn(t, method)
		if loc == nil {
			continue
		}
		locs = append(locs, *loc)
	}
	for _, t := range from {
		loc := toSpanFn(t, method)
		if loc == nil {
			continue
		}
		locs = append(locs, *loc)
	}
	for _, t := range fromPtr {
		loc := toSpanFn(t, method)
		if loc == nil {
			continue
		}
		locs = append(locs, *loc)
	}
	return locs, nil
}

type typesByString []types.Type

func (p typesByString) Len() int           { return len(p) }
func (p typesByString) Less(i, j int) bool { return p[i].String() < p[j].String() }
func (p typesByString) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// toSpan converts a token.Pos range into a span.Span.
// end is exclusive.
func toSpan(fset *token.FileSet, pos token.Pos, name string) span.Span {
	start := fset.Position(pos)
	end := fset.Position(pos + token.Pos(len([]byte(name))))
	filename := start.Filename

	return span.New(
		span.FileURI(filename),
		span.NewPoint(start.Line, start.Column, start.Offset),
		span.NewPoint(end.Line, end.Column, end.Offset),
	)
}
