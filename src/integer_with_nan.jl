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
        TN()
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
    ifelse(isnan(x), TN(), TN(_Unsafe, rem(x.encoded, T)))


import Base: ==, !=

for op in (:(==), :(!=))
    @eval begin
        $op(a::IntegerWithNaN, b::Number) =
            ifelse(isnan(a), false, $op(a.encoded, b))

        $op(a::Number, b::IntegerWithNaN) =
            ifelse(isnan(b), false, $op(a, b.encoded))

        $op(a::IntegerWithNaN, b::IntegerWithNaN) =
            ifelse(isnan(a) || isnan(b), false, $op(a.encoded, b.encoded))
    end
end


import Base: <, <=

for op in (:(<), :(<=))
    @eval begin
        $op(a::IntegerWithNaN, b::Real) =
            ifelse(isnan(a), false, $op(a.encoded, b))

        $op(a::Real, b::IntegerWithNaN) =
            ifelse(isnan(b), false, $op(a, b.encoded))

        $op(a::IntegerWithNaN, b::IntegerWithNaN) =
            ifelse(isnan(a) || isnan(b), false, $op(a.encoded, b.encoded))
    end
end


import Base: +, -, *, /, ^

for op in (:(+), :(-), :(*), :(/), :(^))
    @eval begin
        function $op(a::IntegerWithNaN, b::Number)
            R = _result_type(a, b)
            ifelse(isnan(a), nanvalue(R), convert(R, $op(a.encoded, b)))
        end

        function $op(a::Number, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(b), nanvalue(R), convert(R, $op(a, b.encoded)))
        end

        function $op(a::IntegerWithNaN, b::IntegerWithNaN)
            R = _result_type(a, b)
            ifelse(isnan(a) || isnan(b), nanvalue(R), convert(R, $op(a.encoded, b.encoded)))
        end
    end
end


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


function nanvalue end
export nanvalue

Base.@pure nanvalue(TN::Type{IntegerWithNaN{T}}) where {T} = TN()
Base.@pure nanvalue(T::Type{<:AbstractFloat}) = T(NaN)

Base.@pure encode_nan(T::Type{<:Integer}) = typemax(T)


_result_type(a::T, b::U) where {T,U} = promote_type(T, U)
