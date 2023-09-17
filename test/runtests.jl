using RustRegex
using Test

@testset "RustRegex.jl" begin
    snowman = "snowman: \xE2\x98\x83"
    regex1 = rure"\p{So}$"
    @test occursin(regex1, snowman)
    @test findfirst(regex1, snowman) == 10:10

    regex2 = rure".(.*(?P<snowman>\p{So}))$"
    m = match(regex2, snowman)
    @test !isnothing(m)
    @test length(m) == 2
    @test m[:snowman] == "☃"

    haystack = "abc xyz"
    regex3 = rure"\w+(\w)"
    matches = collect(RustRegex.RuIterator(regex3, haystack; capture = false))
    @test length(matches) == 2
    @test matches[1] == "abc"
    @test matches[2] == "xyz"

    matches2 = collect(eachmatch(regex3, haystack))
    @test length(matches) == 2
    @test matches2[1].match == "abc"
    @test matches2[1][1] == "c"
    @test matches2[2].match == "xyz"
    @test matches2[2][1] == "z"

    regex4 = rure"(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})"
    @test RustRegex.capture_names(regex4) == ["year", "month", "day"]

    regex5 = RuRegex(".", 0)
    @test occursin(regex5, "\xff")

    patterns = ["foo", "barfoo", "\\w+", "\\d+", "foobar", "bar"]
    regexes = RustRegex.RuRegexSet(patterns)
    @test length(regexes) == 6
    @test occursin(regexes, "foobar")
    @test !occursin(regexes, "")

end
