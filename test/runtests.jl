using RustRegex
using Test

@testset "RustRegex.jl" begin
    @testset "rust-capi test" begin
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
        GC.gc()
    end

    @testset "julia regex test" begin
        astr = "Hello, world.\n"
        u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
        @test findfirst(rure"x", astr) == nothing
        @test findfirst(rure"H", astr) == 1:1
        @test findnext(rure"H", astr, 2) == nothing
        @test findfirst(rure"l", astr) == 3:3
        @test findnext(rure"l", astr, 4) == 4:4
        @test findnext(rure"l", astr, 5) == 11:11
        @test findnext(rure"l", astr, 12) == nothing
        @test findfirst(rure"\n", astr) == 14:14
        @test findnext(rure"\n", astr, 15) == nothing
        @test findfirst(rure"z", u8str) == nothing
        @test findfirst(rure"∄", u8str) == nothing
        @test findfirst(rure"∀", u8str) == 1:1
        @test findnext(rure"∀", u8str, 4) == nothing
        @test findfirst(rure"∀", u8str) == findfirst(rure"\u2200", u8str)
        @test findnext(rure"∀", u8str, 4) == findnext(rure"\u2200", u8str, 4)
        @test findfirst(rure"∃", u8str) == 13:13
        @test findnext(rure"∃", u8str, 16) == nothing
        @test findfirst(rure"x", u8str) == 26:26
        @test findnext(rure"x", u8str, 27) == 43:43
        @test findnext(rure"x", u8str, 44) == nothing
        @test findfirst(rure"ε", u8str) == 5:5
        @test findnext(rure"ε", u8str, 7) == 54:54
        @test findnext(rure"ε", u8str, 56) == nothing
        for i = 1:lastindex(astr)
            @test findnext(rure"."s, astr, i) == i:i
        end
        for i = 1:lastindex(u8str)
            if isvalid(u8str,i)
                @test findnext(rure"."s, u8str, i) == i:i
            end
        end
        for i = 1:lastindex(astr)
            @test findnext(rure"", astr, i) == i:i-1
        end
        for i = 1:lastindex(u8str)
            if isvalid(u8str,i)
                @test findnext(rure"", u8str, i) == i:i-1
            end
        end
        @test findfirst(rure"xx", "foo,bar,baz") == nothing
        @test findfirst(rure"fo", "foo,bar,baz") == 1:2
        @test findnext(rure"fo", "foo,bar,baz", 3) == nothing
        @test findfirst(rure"oo", "foo,bar,baz") == 2:3
        @test findnext(rure"oo", "foo,bar,baz", 4) == nothing
        @test findfirst(rure"o,", "foo,bar,baz") == 3:4
        @test findnext(rure"o,", "foo,bar,baz", 5) == nothing
        @test findfirst(rure",b", "foo,bar,baz") == 4:5
        @test findnext(rure",b", "foo,bar,baz", 6) == 8:9
        @test findnext(rure",b", "foo,bar,baz", 10) == nothing
        @test findfirst(rure"az", "foo,bar,baz") == 10:11
        @test findnext(rure"az", "foo,bar,baz", 12) == nothing
        for T = (UInt, BigInt)
            for x = (4, 5)
                @test eltype(findnext(rure"l", astr, T(x))) == Int
            end
        end

        @test split("foo,bar,baz", rure",") == ["foo","bar","baz"]
        let str = "a.:.ba..:..cba.:.:.dcba.:."
            @test split(str, rure"\.(:\.)+") == ["a","ba.",".cba","dcba",""]
            @test split(str, rure"\.(:\.)+"; keepempty=false) == ["a","ba.",".cba","dcba"]
            @test split(str, rure"\.+:\.+") == ["a","ba","cba",":.dcba",""]
            @test split(str, rure"\.+:\.+"; keepempty=false) == ["a","ba","cba",":.dcba"]
        end
        @test split("", rure"") == [""]
        @test split("abc", rure"") == ["a","b","c"]
        @test split("abcd", rure"b?") == ["a","c","d"]
        @test split("abcd", rure"b*") == ["a","c","d"]
        @test split("abcd", rure"b+") == ["a","cd"]
        @test split("abcd", rure"b?c?") == ["a","d"]
        @test split("abcd", rure"[bc]?") == ["a","","d"]
        @test split("abcd", rure"a*") == ["","b","c","d"]
        @test split("abcd", rure"a+") == ["","bcd"]
        @test split("abcd", rure"d*") == ["a","b","c",""]
        @test split("abcd", rure"d+") == ["abc",""]
        @test split("abcd", rure"[ad]?") == ["","b","c",""]

        @test replace("abcd", rure"b?" => "^") == "^a^c^d^"
        @test replace("abcd", rure"b+" => "^") == "a^cd"
        @test replace("abcd", rure"b?c?" => "^") == "^a^d^"
        @test replace("abcd", rure"[bc]?" => "^") == "^a^^d^"
        @test replace("foobarfoo", rure"(fo|ba)" => "xx") == "xxoxxrxxo"
        @test replace("foobarfoo", rure"(foo|ba)" => "bar") == "barbarrbar"
        @test replace("äƀçđ", rure"ƀ?" => "π") == "πäπçπđπ"
        @test replace("äƀçđ", rure"ƀ+" => "π") == "äπçđ"
        @test replace("äƀçđ", rure"ƀ?ç?" => "π") == "πäπđπ"
        @test replace("äƀçđ", rure"[ƀç]?" => "π") == "πäππđπ"
        @test replace("foobarfoo", rure"(fo|ba)" => "ẍẍ") == "ẍẍoẍẍrẍẍo"
        @test replace("ḟøøbarḟøø", rure"(ḟø|ba)" => "xx") == "xxøxxrxxø"
        @test replace("ḟøøbarḟøø", rure"(ḟøø|ba)" => "bar") == "barbarrbar"
        @test replace("fooƀäṙfoo", rure"(fo|ƀä)" => "xx") == "xxoxxṙxxo"
        @test replace("fooƀäṙfoo", rure"(foo|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"
        @test replace("ḟøøƀäṙḟøø", rure"(ḟø|ƀä)" => "xx") == "xxøxxṙxxø"
        @test replace("ḟøøƀäṙḟøø", rure"(ḟøø|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"
        @test replace("The fox.", rure"fox(es)?" => s"bus\1") == "The bus."
        @test replace("The foxes.", rure"fox(es)?" => s"bus\1") == "The buses."
        @test replace("The quick fox quickly.", rure"(quick)?\sfox(es)?\s(run)?" => s"\1 bus\2 \3") == "The quick bus quickly."
        @test replace("a", rure"a" => typeof) == "SubString{String}"
        if VERSION >= v"1.7"
            @test replace("foobarbaz", "z" => "m", rure"a.*a" => uppercase) == "foobARBAm"
            @test replace("abcd", rure"b?" => "^", "" => "") == "^a^c^d^"
            @test replace("abcd", rure"b+" => "^", "" => "") == "a^cd"
            @test replace("abcd", rure"b?c?" => "^", "" => "") == "^a^d^"
            @test replace("abcd", rure"[bc]?" => "^", "" => "") == "^a^^d^"
            @test replace("foobarfoo", rure"(fo|ba)" => "xx", "" => "") == "xxoxxrxxo"
            @test replace("foobarfoo", rure"(foo|ba)" => "bar", "" => "") == "barbarrbar"
            @test replace("äƀçđ", rure"ƀ?" => "π", "" => "") == "πäπçπđπ"
            @test replace("äƀçđ", rure"ƀ+" => "π", "" => "") == "äπçđ"
            @test replace("äƀçđ", rure"ƀ?ç?" => "π", "" => "") == "πäπđπ"
            @test replace("äƀçđ", rure"[ƀç]?" => "π", "" => "") == "πäππđπ"
            @test replace("foobarfoo", rure"(fo|ba)" => "ẍẍ", "" => "") == "ẍẍoẍẍrẍẍo"
            @test replace("ḟøøbarḟøø", rure"(ḟø|ba)" => "xx", "" => "") == "xxøxxrxxø"
            @test replace("ḟøøbarḟøø", rure"(ḟøø|ba)" => "bar", "" => "") == "barbarrbar"
            @test replace("fooƀäṙfoo", rure"(fo|ƀä)" => "xx", "" => "") == "xxoxxṙxxo"
            @test replace("fooƀäṙfoo", rure"(foo|ƀä)" => "ƀäṙ", "" => "") == "ƀäṙƀäṙṙƀäṙ"
            @test replace("ḟøøƀäṙḟøø", rure"(ḟø|ƀä)" => "xx", "" => "") == "xxøxxṙxxø"
            @test replace("ḟøøƀäṙḟøø", rure"(ḟøø|ƀä)" => "ƀäṙ", "" => "") == "ƀäṙƀäṙṙƀäṙ"
            let s = "quick quicker quickest"
                @test replace(s, rure"[aeiou]" => "ä", "ui" => "ki", "i" => "I") == "qääck qääckär qääckäst"
                @test replace(s, "i" => "I", "ui" => "ki", rure"[aeiou]" => "ä") == "qkick qkickär qkickäst"
                @test replace(s, rure"[^ ]+" => "word", "quicker " => "X", count=big"99") == "word word word"
                @test replace(s, "quicker " => "X", rure"[^ ]+" => "word", count=big"99") == "word Xword"
                @test replace(s, rure"(quick)(e)" => s"\2-\1", "x" => "X") == "quick e-quickr e-quickst"
                @test replace(s, 'q' => 'Q', rure"u" => 'U') == "QUick QUicker QUickest"
                @test replace(s, rure"q" => "z", islowercase => uppercase, 'r' => 'x') == "zUICK zUICKER zUICKEST"
                @test_throws ErrorException("no capture group 1") replace(s, rure"q" => s"a\1b")
            end
        end
        function collect_eachmatch(re, str)
            [m.match for m in collect(eachmatch(re, str))]
        end
        # This is different from Regex
        @test collect_eachmatch(rure"a?b?", "asbd") == ["a","b",""] ==
            collect_eachmatch(rure"""a?b?""", "asbd")
        @test collect_eachmatch(rure".\s", "x \u2200 x \u2203 y") == ["x ", "∀ ", "x ", "∃ "]
        @test collect_eachmatch(rure"(\w+)(\s*)", "The dark side of the moon") ==
            ["The ", "dark ", "side ", "of ", "the ", "moon"]
        @test collect_eachmatch(rure"", "") == [""]
        @test collect_eachmatch(rure"aa", "aaaa") == ["aa", "aa"]
        @test collect_eachmatch(rure"", "aaa") == ["", "", "", ""]
        @test collect_eachmatch(rure"GCG","GCGCG") == ["GCG"]
        target = """71.163.72.113 - - [30/Jul/2014:16:40:55 -0700] "GET emptymind.org/thevacantwall/wp-content/uploads/2013/02/DSC_006421.jpg HTTP/1.1" 200 492513 "http://images.search.yahoo.com/images/view;_ylt=AwrB8py9gdlTGEwADcSjzbkF;_ylu=X3oDMTI2cGZrZTA5BHNlYwNmcC1leHAEc2xrA2V4cARvaWQDNTA3NTRiMzYzY2E5OTEwNjBiMjc2YWJhMjkxMTEzY2MEZ3BvcwM0BGl0A2Jpbmc-?back=http%3A%2F%2Fus.yhs4.search.yahoo.com%2Fyhs%2Fsearch%3Fei%3DUTF-8%26p%3Dapartheid%2Bwall%2Bin%2Bpalestine%26type%3Dgrvydef%26param1%3D1%26param2%3Dsid%253Db01676f9c26355f014f8a9db87545d61%2526b%253DChrome%2526ip%253D71.163.72.113%2526p%253Dgroovorio%2526x%253DAC811262A746D3CD%2526dt%253DS940%2526f%253D7%2526a%253Dgrv_tuto1_14_30%26hsimp%3Dyhs-fullyhosted_003%26hspart%3Dironsource&w=588&h=387&imgurl=occupiedpalestine.files.wordpress.com%2F2012%2F08%2F5-peeking-through-the-wall.jpg%3Fw%3D588%26h%3D387&rurl=http%3A%2F%2Fwww.stopdebezetting.com%2Fwereldpers%2Fcompare-the-berlin-wall-vs-israel-s-apartheid-wall-in-palestine.html&size=49.0KB&name=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&p=apartheid+wall+in+palestine&oid=50754b363ca991060b276aba291113cc&fr2=&fr=&tt=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&b=0&ni=21&no=4&ts=&tab=organic&sigr=13evdtqdq&sigb=19k7nsjvb&sigi=12o2la1db&sigt=12lia2m0j&sign=12lia2m0j&.crumb=.yUtKgFI6DE&hsimp=yhs-fullyhosted_003&hspart=ironsource" "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"""
        pat = rure"""([\d\.]+) ([\w.-]+) ([\w.-]+) (\[.+\]) "([^"\r\n]*|[^"\r\n\[]*\[.+\][^"]+|[^"\r\n]+.[^"]+)" (\d{3}) (\d+|-) ("(?:[^"]|\")+)"? ("(?:[^"]|\")+)"?"""
        match(pat, target)
        @test occursin(RuRegex("^a\0b\$"), "a\0b")
        msg = "#Hello# from Julia"
        re = rure"#(.+)# from (?<name>\w+)"
        subst = s"FROM: \g<name>\n MESSAGE: \1"
        @test replace(msg, re => subst) == "FROM: Julia\n MESSAGE: Hello"
        @test repr(rure"") == "rure\"\""
        @test repr(rure"\\\"") == raw"rure\"\\\\\\\"\""

        @test findall(rure"\w+", "foo bar") == [1:3, 5:7]
        @test findall(rure"\w+", "foo bar", overlap=true) == [1:3, 2:3, 3:3, 5:7, 6:7, 7:7]
        @test all(findall(rure"\w*", "foo bar") .=== [1:3, 4:3, 5:7, 8:7])
        @test all(findall(rure"\b", "foo bar") .=== [1:0, 4:3, 5:4, 8:7])
        @test findnext(rure"z", "zabcz", 2) == 5:5
        @test_throws BoundsError findnext(rure"z", "zabcz", 7)

        @test count(rure"\w+", "foo bar") == 2
        @test count(rure"\w+", "foo bar", overlap=true) == 6
        @test count(rure"\w*", "foo bar") == 4
        @test count(rure"\b", "foo bar") == 4

        let m = match(rure"(.)(.)(.)", "xyz")
            @test haskey(m, 1)
            @test haskey(m, 2)
            @test haskey(m, 3)
            @test !haskey(m, 44)
            @test (m[1], m[2], m[3]) == ("x", "y", "z")
            @test sprint(show, m) == "RuRegexMatch(\"xyz\", 1=\"x\", 2=\"y\", 3=\"z\")"
        end
        let m = match(rure"(?<a>.)(.)(?<b>.)", "xyz")
            @test haskey(m, :a)
            @test haskey(m, "b")
            @test !haskey(m, "foo")
            @test (m[:a], m[2], m["b"]) == ("x", "y", "z")
            @test sprint(show, m) == "RuRegexMatch(\"xyz\", a=\"x\", 2=\"y\", b=\"z\")"
            @test keys(m) == ["a", 2, "b"]
        end
        let m = match(rure"(?<numéro>\d)[\pZs]*(?<文本>[\p{Han}\p{P}]+)", "1 孔生雪笠，聖裔也。為人蘊藉，工詩。")
            @test haskey(m, :numéro)
            @test haskey(m, "文本")
            @test !haskey(m, "ゑ")
            @test (m[:numéro], m[:文本]) == ("1", "孔生雪笠，聖裔也。為人蘊藉，工詩。")
            @test (m[1], m[2]) == (m[:numéro], m[:文本])
            @test sprint(show, m) == "RuRegexMatch(\"1 孔生雪笠，聖裔也。為人蘊藉，工詩。\", numéro=\"1\", 文本=\"孔生雪笠，聖裔也。為人蘊藉，工詩。\")"
            @test keys(m) == ["numéro", "文本"]
        end
        @test replace("abcde", rure"(..)(?P<byname>d)" => s"\g<byname>xy\\\1") == "adxy\\bce"
        @test_throws ErrorException replace("a", rure"(?P<x>)" => s"\g<y>")
        @test_throws ErrorException replace("s", rure"(?<g1>.)" => s"\gg1>")
        @test  match(rure"∀∀", "∀x∀∀∀").match == "∀∀"
        @test match(rure"\w+", "Düsseldorf").match == "Düsseldorf"

        @test occursin.(rure"Hello", ["Hello", "World"]) == [true, false]
        @test startswith("abc", rure"a")
        @test endswith("abc", rure"c")
        @test !startswith("abc", rure"b")
        @test !startswith("abc", rure"c")
        @test !endswith("abc", rure"a")
        @test !endswith("abc", rure"b")
        @test !startswith("abc", rure"A")
        @test startswith("abc", rure"A"i)
        @test !endswith("abc", rure"C")
        @test endswith("abc", rure"C"i)
        @test endswith((@views "abc"[2:3]), rure"C"i)

        m = match(rure"(.) test (.+)", "a test 123")
        @test first(m) == "a"
        @test collect(m) == ["a", "123"]
        for (i, capture) in enumerate(m)
            i == 1 && @test capture == "a"
            i == 2 && @test capture == "123"
        end
        handle(::Nothing) = "not found"
        handle((capture,)::RuRegexMatch) = "found $capture"
        @test handle(match(rure"a (\d)", "xyz")) == "not found"
        @test handle(match(rure"a (\d)", "a 1")) == "found 1"

    end
end
