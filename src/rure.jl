module RuRE

using rure_jll

# Modified from the Clang.jl-generated binding over "rure.h". Docs can also be found in "rure.h"
#
# modifications:
#  const converted to corresponding type
#  ignore all custom types
#  all pointer to custom type are converted to `Ptr{Cvoid #= custom type name =#}`
#  comment out some functions
#
const RURE_FLAG_CASEI = UInt32(1 << 0)
const RURE_FLAG_MULTI = UInt32(1 << 1)
const RURE_FLAG_DOTNL = UInt32(1 << 2)
const RURE_FLAG_SWAP_GREED = UInt32(1 << 3)
const RURE_FLAG_SPACE = UInt32(1 << 4)
const RURE_FLAG_UNICODE = UInt32(1 << 5)
const RURE_DEFAULT_FLAGS = UInt32(RURE_FLAG_UNICODE)


# function rure_compile_must(pattern)
#     ccall((:rure_compile_must, librure), Ptr{Cvoid #= rure =#}, (Ptr{Cchar},), pattern)
# end

function rure_compile(pattern, length, flags, options, error)
    ccall((:rure_compile, librure), Ptr{Cvoid #= rure =#}, (Ptr{UInt8}, Csize_t, UInt32, Ptr{Cvoid #= rure_options =#}, Ptr{Cvoid #= rure_error =#}), pattern, length, flags, options, error)
end

function rure_free(re)
    ccall((:rure_free, librure), Cvoid, (Ptr{Cvoid #= rure =#},), re)
end

function rure_is_match(re, haystack, length, start)
    ccall((:rure_is_match, librure), Bool, (Ptr{Cvoid #= rure =#}, Ptr{UInt8}, Csize_t, Csize_t), re, haystack, length, start)
end

function rure_find(re, haystack, length, start, match)
    ccall((:rure_find, librure), Bool, (Ptr{Cvoid #= rure =#}, Ptr{UInt8}, Csize_t, Csize_t, Ptr{Cvoid #= rure_match =#}), re, haystack, length, start, match)
end

function rure_find_captures(re, haystack, length, start, captures)
    ccall((:rure_find_captures, librure), Bool, (Ptr{Cvoid #= rure =#}, Ptr{UInt8}, Csize_t, Csize_t, Ptr{Cvoid #= rure_captures =#}), re, haystack, length, start, captures)
end

# function rure_shortest_match(re, haystack, length, start, _end)
#     ccall((:rure_shortest_match, librure), Bool, (Ptr{Cvoid #= rure =#}, Ptr{UInt8}, Csize_t, Csize_t, Ptr{Csize_t}), re, haystack, length, start, _end)
# end

function rure_capture_name_index(re, name)
    ccall((:rure_capture_name_index, librure), Int32, (Ptr{Cvoid #= rure =#}, Ptr{Cchar}), re, name)
end

function rure_iter_capture_names_new(re)
    ccall((:rure_iter_capture_names_new, librure), Ptr{Cvoid #= rure_iter_capture_names =#}, (Ptr{Cvoid #= rure =#},), re)
end

function rure_iter_capture_names_free(it)
    ccall((:rure_iter_capture_names_free, librure), Cvoid, (Ptr{Cvoid #= rure_iter_capture_names =#},), it)
end

function rure_iter_capture_names_next(it, name)
    ccall((:rure_iter_capture_names_next, librure), Bool, (Ptr{Cvoid #= rure_iter_capture_names =#}, Ptr{Ptr{Cchar}}), it, name)
end

function rure_iter_new(re)
    ccall((:rure_iter_new, librure), Ptr{Cvoid #= rure_iter=#}, (Ptr{Cvoid #= rure =#},), re)
end

function rure_iter_free(it)
    ccall((:rure_iter_free, librure), Cvoid, (Ptr{Cvoid #= rure_iter=#},), it)
end

function rure_iter_next(it, haystack, length, match)
    ccall((:rure_iter_next, librure), Bool, (Ptr{Cvoid #= rure_iter=#}, Ptr{UInt8}, Csize_t, Ptr{Cvoid #= rure_match =#}), it, haystack, length, match)
end

function rure_iter_next_captures(it, haystack, length, captures)
    ccall((:rure_iter_next_captures, librure), Bool, (Ptr{Cvoid #= rure_iter=#}, Ptr{UInt8}, Csize_t, Ptr{Cvoid #= rure_captures =#}), it, haystack, length, captures)
end

function rure_captures_new(re)
    ccall((:rure_captures_new, librure), Ptr{Cvoid #= rure_captures =#}, (Ptr{Cvoid #= rure =#},), re)
end

function rure_captures_free(captures)
    ccall((:rure_captures_free, librure), Cvoid, (Ptr{Cvoid #= rure_captures =#},), captures)
end

function rure_captures_at(captures, i, match)
    ccall((:rure_captures_at, librure), Bool, (Ptr{Cvoid #= rure_captures =#}, Csize_t, Ptr{Cvoid #= rure_match =#}), captures, i, match)
end

function rure_captures_len(captures)
    ccall((:rure_captures_len, librure), Csize_t, (Ptr{Cvoid #= rure_captures =#},), captures)
end

function rure_options_new()
    ccall((:rure_options_new, librure), Ptr{Cvoid #= rure_options =#}, ())
end

function rure_options_free(options)
    ccall((:rure_options_free, librure), Cvoid, (Ptr{Cvoid #= rure_options =#},), options)
end

function rure_options_size_limit(options, limit)
    ccall((:rure_options_size_limit, librure), Cvoid, (Ptr{Cvoid #= rure_options =#}, Csize_t), options, limit)
end

function rure_options_dfa_size_limit(options, limit)
    ccall((:rure_options_dfa_size_limit, librure), Cvoid, (Ptr{Cvoid #= rure_options =#}, Csize_t), options, limit)
end

function rure_compile_set(patterns, patterns_lengths, patterns_count, flags, options, error)
    ccall((:rure_compile_set, librure), Ptr{Cvoid #= rure_set =#}, (Ptr{Ptr{UInt8}}, Ptr{Csize_t}, Csize_t, UInt32, Ptr{Cvoid #= rure_options =#}, Ptr{Cvoid #= rure_error =#}), patterns, patterns_lengths, patterns_count, flags, options, error)
end

function rure_set_free(re)
    ccall((:rure_set_free, librure), Cvoid, (Ptr{Cvoid #= rure_set =#},), re)
end

# function rure_set_is_match(re, haystack, length, start)
#     ccall((:rure_set_is_match, librure), Bool, (Ptr{Cvoid #= rure_set =#}, Ptr{UInt8}, Csize_t, Csize_t), re, haystack, length, start)
# end

function rure_set_matches(re, haystack, length, start, matches)
    ccall((:rure_set_matches, librure), Bool, (Ptr{Cvoid #= rure_set =#}, Ptr{UInt8}, Csize_t, Csize_t, Ptr{Bool}), re, haystack, length, start, matches)
end

function rure_set_len(re)
    ccall((:rure_set_len, librure), Csize_t, (Ptr{Cvoid #= rure_set =#},), re)
end

function rure_error_new()
    ccall((:rure_error_new, librure), Ptr{Cvoid #= rure_error =#}, ())
end

function rure_error_free(err)
    ccall((:rure_error_free, librure), Cvoid, (Ptr{Cvoid #= rure_error =#},), err)
end

function rure_error_message(err)
    ccall((:rure_error_message, librure), Ptr{Cchar}, (Ptr{Cvoid #= rure_error =#},), err)
end

# function rure_escape_must(pattern)
#     ccall((:rure_escape_must, librure), Ptr{Cchar}, (Ptr{Cchar},), pattern)
# end

# function rure_cstring_free(s)
#     ccall((:rure_cstring_free, librure), Cvoid, (Ptr{Cchar},), s)
# end


end
