# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).

abstract type _Unsafe end


struct IntegerWithNaN{T<:Integer} <: Integer
    encoded::T

    IntegerWithNaN{T}() where {T<:Integer} = new(encode_nan(T))

    IntegerWithNaN{T}(::Type{_Unsafe}, x) where {T<:Integer} = new(x)
end


Base.isnan(x::IntegerWithNaN{T}) where {T} =
    x.encoded == encode_nan(T)


Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:Integer,U} =
    withnan(promote_type(T,U))

Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:AbstractFloat,U} =
    float(promote_type(T,U))


function Base.convert(TN::Type{IntegerWithNaN{T}}, x::Integer) where {T}
    if typemax(T) > typemax(x)
        TN(_Unsafe, x)
    else
        if x != encode_nan(T)
            TN(_Unsafe, x)
        else
            throw(InexactError())
        end
    end
end

function Base.convert(T::Type{<:Integer}, x::IntegerWithNaN)
    if !isnan(x)
        convert(T, x.encoded)
    else
        throw(InexactError())
    end
end

function Base.convert(TN::Type{IntegerWithNaN{T}}, x::IntegerWithNaN) where {T}
    if !isnan(x)
        TN(convert(T, x.encoded))
    else
        intnan(T)
    end
end

Base.convert(TN::Type{IntegerWithNaN{T}}, x::AbstractFloat) where {T} =
    ifelse(isnan(x), TN(), TN(convert(T, x)))

Base.convert(T::Type{<:AbstractFloat}, x::IntegerWithNaN) =
    ifelse(isnan(x), convert(T, NaN), convert(T, x.encoded))


Base.rem(x::IntegerWithNaN{T}, ::Type{<:Integer}) where {T} =
    rem(x.encoded, T)

Base.rem(x::Integer, TN::Type{IntegerWithNaN{T}}) where {T} =
    TN(_Unsafe, rem(x, T))

Base.rem(x::IntegerWithNaN, TN::Type{IntegerWithNaN{T}}) where {T} =
    ifelse(isnan(x), intnan(TN), TN(_Unsafe, rem(x.encoded, T)))




function withnan end
export withnan

Base.@pure withnan(::Type{Int8}) = IntegerWithNaN{Int16}
Base.@pure withnan(::Type{UInt8}) = IntegerWithNaN{UInt16}
Base.@pure withnan(::Type{Int16}) = IntegerWithNaN{Int32}
Base.@pure withnan(::Type{UInt16}) = IntegerWithNaN{UInt32}
Base.@pure withnan(::Type{Int32}) = IntegerWithNaN{Int32}
Base.@pure withnan(::Type{UInt32}) = IntegerWithNaN{UInt32}
Base.@pure withnan(::Type{Int64}) = IntegerWithNaN{Int64}
Base.@pure withnan(::Type{UInt64}) = IntegerWithNaN{UInt64}

withnan(x::Integer) = convert(withnan(typeof(x)), x)


function intnan end
export intnan

Base.@pure intnan(TN::Type{IntegerWithNaN{T}}) where {T} = TN()

intnan(T::Type{<:Integer}) = intnan(withnan(T))


Base.@pure encode_nan(T::Type{<:Integer}) = typemax(T)
