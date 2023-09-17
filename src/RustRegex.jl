module RustRegex

include("rure.jl")
using .RuRE

export RuRegex, @rure_str

mutable struct RuObj{F} <: Ref{Cvoid}
    ptr::Ptr{Cvoid}
    free::F
    function RuObj(ptr, free)
        obj = new{typeof(free)}(ptr, free)
        finalizer(obj) do obj
            obj.ptr == C_NULL || obj.free(obj.ptr)
        end
        return obj
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, obj::RuObj) = obj.ptr

Base.show(io::IO, obj::RuObj) = (print(io, "RuObj("); show(io, obj.ptr); print(io, ')'))

"""
    @rure_str -> RuRegex

Construct a rust regex, such as `rure"^[a-z]*\$"`, without interpolation and unescaping (except for
 quotation mark `"` which still has to be escaped). The regex also accepts one or more flags,
 listed after the ending quote:

- case insensitive (i) flag.
- multi-line matching (m) flag. (`^` and `\$` match new line boundaries.)
- any character (s) flag. ("." matches new line.)
- greedy swap (U) flag. (e.g., `+` is ungreedy and `+?` is greedy.)
- ignore whitespace (x) flag.
- Unicode (u) flag. (default flag)


See also: [`RuRegex`](@ref)

"""
macro rure_str(pattern, flags...) RuRegex(pattern, flags...) end

"""
    RuRegex(pattern[, flags]) <: AbstractPattern

A type representing rust regular expression. `RuRegex` support `occursin`, `findnext`, `findfirst`,
 `match`, and `eatchmatch`.

See also: [`@rure_str`](@ref)

"""
struct RuRegex <: AbstractPattern
    pattern::String
    flags::UInt32
    obj::RuObj{typeof(RuRE.rure_free)}
    function RuRegex(pattern::AbstractString, flags = RuRE.RURE_DEFAULT_FLAGS, options = C_NULL)
        len = ncodeunits(pattern)
        err = RuError()
        ptr = RuRE.rure_compile(pattern, len, flags, options, err)
        if ptr == C_NULL
            msg = Base.unsafe_string(RuRE.rure_error_message(err))
            error("RuRE: ", msg)
        end
        return new(pattern, flags, RuObj(ptr, RuRE.rure_free))
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, rure::RuRegex) = Base.cconvert(Ptr{Cvoid}, rure.obj)

function RuRegex(pattern::AbstractString, flags::AbstractString)
    compile_options = RuRE.RURE_DEFAULT_FLAGS
    for f in flags
        compile_options |= f == 'i' ? RuRE.RURE_FLAG_CASEI :
            f == 'm' ? RuRE.RURE_FLAG_MULTI :
            f == 's' ? RuRE.RURE_FLAG_DOTNL :
            f == 'U' ? RuRE.RURE_FLAG_SWAP_GREED :
            f == 'x' ? RuRE.RURE_FLAG_SPACE :
            f == 'u' ? RuRE.RURE_FLAG_UNICODE :
            throw(ArgumentError("unknown regex flag: $f"))
    end
    return RuRegex(pattern, compile_options)
end

function Base.show(io::IO, r::RuRegex)
    flags = r.flags
    print(io, "rure\"")
    Base.escape_raw_string(io, r.pattern)
    print(io, "\"")
    if flags != RuRE.RURE_DEFAULT_FLAGS
        !iszero(flags & RuRE.RURE_FLAG_CASEI) && print(io, 'i')
        !iszero(flags & RuRE.RURE_FLAG_MULTI) && print(io, 'm')
        !iszero(flags & RuRE.RURE_FLAG_DOTNL) && print(io, 's')
        !iszero(flags & RuRE.RURE_FLAG_SWAP_GREED) && print(io, 'U')
        !iszero(flags & RuRE.RURE_FLAG_SPACE) && print(io, 'x')
        !iszero(flags & RuRE.RURE_FLAG_UNICODE) && print(io, 'u')
    end
end

struct RuOptions
    obj::RuObj{typeof(RuRE.rure_options_free)}
    function RuOptions()
        obj = RuObj(rure_options_new(), RuRE.rure_options_free)
        return new(obj)
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, opts::RuOptions) = Base.cconvert(Ptr{Cvoid}, opts.obj)

set_size_limit!(opts::RuOptions, limit::Integer) = RuRE.rure_options_size_limit(opts, limit)
rure_options_dfa_size_limit!(opts::RuOptions, limit::Integer) = RuRE.rure_options_dfa_size_limit(opts, limit)

struct RuError
    obj::RuObj{typeof(RuRE.rure_error_free)}
    function RuError()
        obj = RuObj(RuRE.rure_error_new(), RuRE.rure_error_free)
        return new(obj)
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, err::RuError) = Base.cconvert(Ptr{Cvoid}, err.obj)

struct RuCaptures
    obj::RuObj{typeof(RuRE.rure_captures_free)}
    function RuCaptures(re::RuRegex)
        obj = RuObj(RuRE.rure_captures_new(re), RuRE.rure_captures_free)
        return new(obj)
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, captures::RuCaptures) = Base.cconvert(Ptr{Cvoid}, captures.obj)
Base.length(captures::RuCaptures) = Int(RuRE.rure_captures_len(captures))

function _get_capture(captures::RuCaptures, i::Integer)
    m = Ref{UnitRange{UInt}}(0:0)
    has_capture = RuRE.rure_captures_at(captures, i, m)
    has_capture || return nothing
    _m = m[]
    start = Int(_m.start) + 1
    stop = Int(_m.stop)
    return start:stop
end
function _get_capture(captures::RuCaptures, s::String, i::Integer)
    r = _get_capture(captures, i)
    return isnothing(r) ? nothing : (thisind(s, r.start):thisind(s, r.stop))
end

struct RuCaptureNamesIterator
    obj::RuObj{typeof(RuRE.rure_iter_capture_names_free)}
    function RuCaptureNamesIterator(re::RuRegex)
        obj = RuObj(RuRE.rure_iter_capture_names_new(re), RuRE.rure_iter_capture_names_free)
        return new(obj)
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, itr::RuCaptureNamesIterator) = Base.cconvert(Ptr{Cvoid}, itr.obj)
Base.eltype(::Type{RuCaptureNamesIterator}) = Union{String, Int}
Base.IteratorSize(::Type{RuCaptureNamesIterator}) = Base.SizeUnknown()
function Base.iterate(itr::RuCaptureNamesIterator)
    name_ptr = Ref{Ptr{Cchar}}(C_NULL)
    has_names = RuRE.rure_iter_capture_names_next(itr, name_ptr)
    return has_names ? Base.iterate(itr, 1) : nothing
end
function Base.iterate(itr::RuCaptureNamesIterator, i)
    name_ptr = Ref{Ptr{Cchar}}(C_NULL)
    found = RuRE.rure_iter_capture_names_next(itr, name_ptr)
    found || return nothing
    name = Base.unsafe_string(name_ptr[])
    return (isempty(name) ? i : name, i+1)
end

struct RuIterator{F}
    re::RuRegex
    string::String
    obj::RuObj{typeof(RuRE.rure_iter_free)}
    function RuIterator(re::RuRegex, str::String; capture::Union{Bool, Nothing} = nothing)
        obj = RuObj(RuRE.rure_iter_new(re), RuRE.rure_iter_free)
        isnothing(capture) && return new{RuRE.rure_iter_next_captures}(re, str, obj)
        nextf = capture ? RuRE.rure_iter_next_captures : RuRE.rure_iter_next
        return new{nextf}(re, str, obj)
    end
end
Base.cconvert(::Type{Ptr{Cvoid}}, itr::RuIterator) = Base.cconvert(Ptr{Cvoid}, itr.obj)
Base.eltype(::Type{RuIterator{RuRE.rure_iter_next}}) = SubString{String}
Base.eltype(::Type{RuIterator{RuRE.rure_iter_next_captures}}) = RuRegexMatch
Base.IteratorSize(::Type{<:RuIterator}) = Base.SizeUnknown()
function Base.iterate(itr::RuIterator{F}, _ = nothing) where F
    s = itr.string
    len = ncodeunits(s)
    if F isa typeof(RuRE.rure_iter_next)
        m = Ref{UnitRange{UInt}}(0:0)
        found = RuRE.rure_iter_next(itr, s, len, m)
        found || return nothing
        _m = m[]
        start = thisind(s, Int(_m.start) + 1)
        stop = thisind(s, Int(_m.stop))
        return view(s, start:stop), nothing
    else
        r = itr.re
        caps = RuCaptures(r)
        found = RuRE.rure_iter_next_captures(itr, s, len, caps)
        found || return nothing
        return _captures2match(caps, r, s), nothing
    end
end

const RuRegexMatchIterator = RuIterator{RuRE.rure_iter_next_captures}
RuRegexMatchIterator(r::RuRegex, s::String) = RuIterator(r, s)

function Base.show(io::IO, itr::RuIterator{F}) where F
    if F isa typeof(RuRE.rure_iter_next)
        print(io, "RuIterator{RuRE.rure_iter_next}(")
    else
        print(io, "RuRegexMatchIterator(")
    end
    show(io, itr.re)
    print(io, ", ")
    show(io, itr.string)
    print(io, ')')
end

struct RuRegexSet
    patterns::Vector{String}
    obj::RuObj{typeof(RuRE.rure_set_free)}
    function RuRegexSet(patterns::Vector{String}, flags = RuRE.RURE_DEFAULT_FLAGS, options = C_NULL)
        err = RuError()
        ptr = RuRE.rure_compile_set(patterns, map(length, patterns), length(patterns), flags, options, err)
        if ptr == C_NULL
            msg = Base.unsafe_string(RuRE.rure_error_message(err))
            error("RuRE: ", msg)
        end
        obj = RuObj(ptr, RuRE.rure_set_free)
        return new(patterns, obj)
    end
end
RuRegexSet(patterns::Vector{<:AbstractString}, flags = RuRE.RURE_DEFAULT_FLAGS, options = C_NULL) =
    RuRegexSet(map(String, patterns), flags, options)
Base.cconvert(::Type{Ptr{Cvoid}}, set::RuRegexSet) = Base.cconvert(Ptr{Cvoid}, set.obj)
Base.length(set::RuRegexSet) = RuRE.rure_set_len(set)


"""
    has_captures(re::RuRegex) -> Bool

Return true if there are any capture groups in the regex.
"""
has_captures(re::RuRegex) = RuRE.rure_iter_capture_names_next(RuCaptureNamesIterator(itr), Ref{Ptr{Cchar}}(C_NULL))

"""
    capture_names(re::RuRegex) -> Vector{Union{Int, String}}

Return capture name or index.

# Example

```julia-repl
julia> RustRegex.capture_names(rure"(?P<year>\\d{4})-(?P<month>\\d{2})-(?P<day>\\d{2})")
3-element Vector{Union{Int64, String}}:
 "year"
 "month"
 "day"

julia> RustRegex.capture_names(rure"(?<hour>\\d+):(?<minute>\\d+)(am|pm)?")
3-element Vector{Union{Int64, String}}:
  "hour"
  "minute"
 3

```
"""
capture_names(re::RuRegex) = collect(RuCaptureNamesIterator(re))

"""
    RuRegexMatch

A type representing a single match to a `RuRegex` found in a string, usually created from `match`.

The semantic is the same as `RegexMatch`.
"""
struct RuRegexMatch <: AbstractMatch
    match::SubString{String}
    captures::Union{Nothing, Vector{Union{Nothing, SubString{String}}}}
    offset::Int
    offsets::Union{Nothing, Vector{Int}}
    regex::RuRegex
end
Base.keys(m::RuRegexMatch) = capture_names(m.regex)
Base.length(m::RuRegexMatch) = length(m.captures)
Base.iterate(m::RuRegexMatch, args...) = Base.iterate(m.captures, args...)
Base.eltype(m::RuRegexMatch) = eltype(m.captures)
Base.haskey(m::RuRegexMatch, idx::Integer) = idx in eachindex(m.captures)
function Base.haskey(m::RuRegexMatch, name::Union{AbstractString,Symbol})
    idx = RuRE.rure_capture_name_index(m.regex, name)
    return Int(idx) != -1
end

Base.getindex(m::RuRegexMatch, idx::Integer) = m.captures[idx]
function Base.getindex(m::RuRegexMatch, name::Union{AbstractString,Symbol})
    idx = RuRE.rure_capture_name_index(m.regex, name)
    idx == -1 && error("no capture group named $name found in regex")
    return m[idx]
end

function Base.show(io::IO, m::RuRegexMatch)
    print(io, "RuRegexMatch(")
    show(io, m.match)
    capture_keys = keys(m)
    if !isnothing(m.captures)
        for (i, capture_name) in enumerate(capture_keys)
            print(io, ", ", capture_name, "=")
            show(io, m.captures[i])
        end
    end
    print(io, ")")
end

function _captures2match(caps::RuCaptures, r::RuRegex, s::String)
    n_cap = length(caps) - 1
    m_r = _get_capture(caps, s, 0)
    matched = view(s, m_r)
    offset = m_r.start
    if iszero(n_cap)
        captures = offsets = nothing
    else
        captures = Vector{Union{Nothing, SubString{String}}}(undef, n_cap)
        offsets = Vector{Int}(undef, n_cap)
        for i = 1:n_cap
            cap_r = _get_capture(caps, s, i)
            if isnothing(cap_r)
                captures[i] = nothing
                offsets[i] = 0
            else
                captures[i] = view(s, cap_r)
                offsets[i] = cap_r.start
            end
        end
    end
    return RuRegexMatch(matched, captures, offset, offsets, r)
end

function Base.occursin(r::RuRegex, s::AbstractString; offset::Integer = 0)
    return RuRE.rure_is_match(r, s, ncodeunits(s), offset)
end
function Base.occursin(r::RuRegex, s::SubString{String}; offset::Integer = 0)
    return RuRE.rure_is_match(r, s.string, s.ncodeunits, s.offset + offset)
end

function Base.occursin(rs::RuRegexSet, s::AbstractString; offset::Integer = 0)
    return RuRE.rure_set_is_match(rs, s, ncodeunits(s), offset)
end
function Base.occursin(rs::RuRegexSet, s::SubString{String}; offset::Integer = 0)
    return RuRE.rure_set_is_match(rs, s.string, s.ncodeunits, s.offset + offset)
end

function _findnext(r::RuRegex, s::AbstractString, idx::Integer)
    m = Ref{UnitRange{UInt}}(0:0)
    found = RuRE.rure_find(r, s, ncodeunits(s), idx-1, m)
    found || return nothing
    _m = m[]
    start = thisind(s, Int(_m.start) + 1)
    stop = thisind(s, Int(_m.stop))
    return start:stop
end
function _findnext(r::RuRegex, s::SubString{String}, idx::Integer)
    r = _findnext(r, s.string, s.offset + idx)
    return isnothing(r) ? nothing : (r .- s.offset)
end

function Base.findnext(r::RuRegex, s::AbstractString, idx::Integer)
    if idx > nextind(s, lastindex(s))
        throw(BoundsError(s, idx))
    end
    return _findnext(r, s, idx)
end
Base.findfirst(r::RuRegex, s::AbstractString) = findnext(r, s, 1)

function Base.match(r::RuRegex, s::String, idx::Integer = 1)
    caps = RuCaptures(r)
    found = RuRE.rure_find_captures(r, s, ncodeunits(s), idx-1, caps)
    found || return nothing
    return _captures2match(caps, r, s)
end
function Base.match(r::RuRegex, s::SubString{String}, idx::Integer = 1)
    m = match(r, s.string, s.offset + idx)
    isnothing(m) && return nothing
    offset = m.offset - s.offset
    @. m.offsets = max(m.offsets - s.offset, 0)
    return RuRegexMatch(m.match, m.captures, offset, m.offsets, m.regex)
end

Base.eachmatch(r::RuRegex, s::AbstractString) = RuRegexMatchIterator(r, String(s))


end
