# Sample files

Files for manual end-to-end testing in Emacs. Open a file, run `M-x flywrite-mode`, and verify diagnostics appear as expected.

| File | Description |
|------|-------------|
| `duplicate-errors.txt` | Same pronoun case error ("him") in two sentences (tests per-occurrence underlines) |
| `example.txt` | Quick start sample text from the README (affect/effect, then/than, weak phrasing) |
| `latex-complex.tex` | Long LaTeX exam document with heavy markup, lists, and math (stress test for markup suppression) |
| `latex-custom-environment.tex` | Minimal LaTeX exam with a solution block (should handle custom environments) |
| `latex-itemize.tex` | Short LaTeX with an itemize list containing a spelling error |
| `latex-simple.tex` | LaTeX document with multiple paragraphs of errors (should ignore markup) |
| `markdown-code.md` | Markdown with headings, blockquotes, a code block, and errors (should skip code blocks) |
| `markdown-simple.md` | Markdown with multiple paragraphs of errors plus clean prose |
| `text-general-and-academic.txt` | Two paragraphs: general errors (flagged by `prose`), then academic-only errors (flagged by `academic`) |
| `file-local-prose.txt` | `prose` prompt via file-local variable; general errors flagged, academic-only errors not flagged |
