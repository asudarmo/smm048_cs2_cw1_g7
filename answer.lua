-- answer.lua
-- Converts ::: answer blocks into a LaTeX boxed environment

function Div(el)
  if el.classes:includes("answer") then
    return {
      pandoc.RawBlock("latex", "\\begin{center}"),
      pandoc.RawBlock("latex", "\\fbox{\\begin{minipage}{0.95\\linewidth}"),
      pandoc.RawBlock("latex", "\\textbf{Answer}\\\\[0.5em]"),
      el,
      pandoc.RawBlock("latex", "\\end{minipage}}"),
      pandoc.RawBlock("latex", "\\end{center}")
    }
  end
end