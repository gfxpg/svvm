module Svvm.Utils.GraphvizDot where

directedGraph name contents = "digraph " <> name <> " {\n" <> contents <> "}\n"

subgraph name contents = "subgraph " <> name <> " {\nnode [shape=none;]\n" <> contents <> "}\n"

edge nodeFrom nodeTo = nodeFrom <> " -> " <> nodeTo <> "\n"

node name contents = name <> "[label=<" <> contents <> ">];\n"

table contents = "<table border=\"1\" cellborder=\"0\" cellspacing=\"1\">\n" <> contents <> "</table>"

tableRow contents = "<tr>" <> contents <> "</tr>\n"

tableCol colspan contents = "<td colspan=\"" <> show colspan <> "\" align=\"left\">" <> contents <> "</td>"

textBold t = "<b>" <> t <> "</b>"
