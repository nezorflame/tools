package lsp

import (
	"context"

	"golang.org/x/tools/internal/lsp/protocol"
	"golang.org/x/tools/internal/lsp/source"
	"golang.org/x/tools/internal/span"
)

func (s *Server) implementation(ctx context.Context, params *protocol.TextDocumentPositionParams) ([]protocol.Location, error) {
	var spans []span.Span

	f := func(view source.View) error {
		f, m, err := getGoFile(ctx, view, span.URI(params.TextDocument.URI))
		if err != nil {
			return err
		}

		s, err := m.PointSpan(params.Position)
		if err != nil {
			return err
		}

		rng, err := s.Range(m.Converter)
		if err != nil {
			return err
		}

		// Find all implementations for the identifier at the position.
		ident, err := source.Identifier(ctx, f, rng.Start)
		if err != nil {
			return err
		}

		impls, err := ident.Implementation(ctx, view.Search(), rng.Start)
		if err != nil {
			return err
		}

		spans = append(spans, impls...)
		return nil
	}

	err := walkSession(s.session, f)
	if err != nil {
		return nil, err
	}

	return toProtocolLocations(spans), nil
}

type viewWalkFunc func(v source.View) error

func walkSession(session source.Session, f viewWalkFunc) error {
	for _, view := range session.Views() {
		err := f(view)
		if err != nil {
			return err
		}
	}
	return nil
}

func toProtocolLocations(spans []span.Span) []protocol.Location {
	if len(spans) == 0 {
		return []protocol.Location{}
	}

	var pLocations []protocol.Location
	for _, s := range spans {
		rng := toProtocolRange(s)
		ploc := protocol.Location{
			URI:   string(s.URI()),
			Range: rng,
		}
		pLocations = append(pLocations, ploc)
	}

	return pLocations
}

func toProtocolRange(spn span.Span) protocol.Range {
	var rng protocol.Range

	rng.Start = toProtocolPosition(spn.Start())
	rng.End = toProtocolPosition(spn.End())

	return rng
}

func toProtocolPosition(point span.Point) protocol.Position {
	var pos protocol.Position
	pos.Line = float64(point.Line() - 1)
	pos.Character = float64(point.Column() - 1)

	return pos
}
