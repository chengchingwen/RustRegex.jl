using RustRegex
using Documenter

DocMeta.setdocmeta!(RustRegex, :DocTestSetup, :(using RustRegex); recursive=true)

makedocs(;
    modules=[RustRegex],
    authors="chengchingwen <chengchingwen214@gmail.com> and contributors",
    repo="https://github.com/chengchingwen/RustRegex.jl/blob/{commit}{path}#{line}",
    sitename="RustRegex.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://chengchingwen.github.io/RustRegex.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/chengchingwen/RustRegex.jl",
    devbranch="main",
)
