# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


struct OptionalValue{T,E}
    encoded::E
end


function encode_missing end

encode_missing(::Type{T}) where {T<:Integer} =
    OptionalValue{T,T}(typemax(T))

encode_missing(::Type{T}) where {T<:AbstractFloat} =
    OptionalValue{T,T}(convert(T, NaN))


function unsafe_encode_value(x::T) where {T}
    OptionalValue{T,T}(x)
end

function unsafe_decode_value(x::OptionalValue{T,T}) where {T}
    x.encoded
end



Missings.ismissing(x::OptionalValue{T}) where {T} =
    x == encode_missing(typeof(x))

Missings.ismissing(x::OptionalValue{T}) where {T<:AbstractFloat} =
    isnan(typeof(x))


convert(::Type{OptionalValue{T}}, ::Missing) where T = encode_missing(T)

convert(Union{T,Missing}, x::OptionalValue{T}) =
    ifelse(ismissing(x), missing, x.value)


function convert(::Type{T}, x::OptionalValue) where {T}
    if !ismissing(x)
        convert(T, unsafe_decode_value(x))
    else
        throw(ArgumentError("Missing value $x can't be converted to type $T")
    end
end
