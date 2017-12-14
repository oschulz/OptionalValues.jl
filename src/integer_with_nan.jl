# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


abstract type _Internal_ end


const PrimInt = Union{Signed,Unsigned}


struct IntegerWithNaN{T<:PrimInt} <: Integer
    encoded::T

    IntegerWithNaN{T}(::Type{_Internal_}, x::T) where {T<:PrimInt} = new(x)
end

IntegerWithNaN{T}() where {T<:Integer} = IntegerWithNaN{T}(encode_nan(T))

export IntegerWithNaN


function Base.show(io::IO, ::MIME"text/plain", x::IntegerWithNaN{T}) where T
    if !isnan(x)
        print(io, "withnan($(x.encoded))")
    else
        print(io, "$(T)NaN")
    end
end

Base.show(io::IO, x::IntegerWithNaN{T}) where T =
    show(io, MIME"text/plain"(), x)


Base.isnan(x::IntegerWithNaN{T}) where {T} =
    x.encoded == encode_nan(T)


Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:Integer,U} =
    withnan(promote_type(T,U))

Base.promote_rule(::Type{Bool}, TN::Type{IntegerWithNaN{T}}) where {T} = TN

Base.promote_rule(::Type{T}, ::Type{IntegerWithNaN{U}}) where {T<:AbstractFloat,U} =
    float(promote_type(T,U))


@inline Base.convert(TN::Type{IntegerWithNaN{T}}, x::IntegerWithNaN{T}) where {T} = x

@inline Base.convert(TN::Type{IntegerWithNaN{T}}, x::IntegerWithNaN{U}) where {T,U} =
    _nan_or_value(TN, isnan(x), x.encoded)


@inline Base.convert(TN::Type{IntegerWithNaN{T}}, x::Integer) where {T} = TN(_Internal_, convert(T, x))

@inline function Base.convert(::Type{<:Integer}, x::IntegerWithNaN{T}) where {T}
    if !isnan(x)
        convert(T, x.encoded)
    else
        throw(InexactError())
    end
end


@inline Base.convert(TN::Type{IntegerWithNaN{T}}, x::AbstractFloat) where {T} =
    _nan_or_value(TN, isnan(x), x)

@inline Base.convert(T::Type{<:AbstractFloat}, x::IntegerWithNaN) =
    ifelse(isnan(x), convert(T, NaN), convert(T, x.encoded))



@inline Base.rem(x::IntegerWithNaN{T}, ::Type{<:Integer}) where {T} =
    rem(x.encoded, T)

@inline Base.rem(x::Integer, TN::Type{IntegerWithNaN{T}}) where {T} =
    TN(_Internal_, rem(x, T))

@inline Base.rem(x::IntegerWithNaN, TN::Type{IntegerWithNaN{T}}) where {T} =
    _nan_or_value(TN, isnan(x), rem(x.encoded, T))


@inline _unary_fwd(f::Function, x::IntegerWithNaN) =
    _nan_or_value(isnan(x), f(x.encoded))

@inline _unary_fwd_boolret(f::Function, x::IntegerWithNaN) =
    ifelse(isnan(x), false, f(x.encoded))


@inline _binary_fwd(f::Function, a::IntegerWithNaN, b) =
    _nan_or_value(isnan(a), f(a.encoded, b))

@inline _binary_fwd(f::Function, a, b::IntegerWithNaN) =
    _nan_or_value(isnan(b), f(a, b.encoded))

@inline _binary_fwd(f::Function, a::IntegerWithNaN, b::IntegerWithNaN) =
    _nan_or_value(isnan(a) || isnan(b), f(a.encoded, b.encoded))

@inline _binary_fwd_boolret(f::Function, a::IntegerWithNaN, b) =
    ifelse(isnan(a), false, f(a.encoded, b))

@inline _binary_fwd_boolret(f::Function, a, b::IntegerWithNaN) =
    ifelse(isnan(b), false, f(a, b.encoded))

@inline _binary_fwd_boolret(f::Function, a::IntegerWithNaN, b::IntegerWithNaN) =
    ifelse(isnan(a) || isnan(b), false, f(a.encoded, b.encoded))



import Base: ==, !=

for op in (:(==), :(!=))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Number) = _binary_fwd_boolret($op, a, b)
        @inline $op(a::Number, b::IntegerWithNaN) = _binary_fwd_boolret($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd_boolret($op, a, b)
    end
end


import Base: <, <=

for op in (:(<), :(<=))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Real) = _binary_fwd_boolret($op, a, b)
        @inline $op(a::Real, b::IntegerWithNaN) = _binary_fwd_boolret($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd_boolret($op, a, b)
    end
end


import Base: +, -, *, /

for op in (:(+), :(-), :(*), :(/))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Number) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::Bool) = _binary_fwd($op, a, b)
        @inline $op(a::Number, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::Bool, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
end


import Base: ^

for op in (:(^),)
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Number) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::Integer) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::Bool) = _binary_fwd($op, a, b)
        @inline $op(a::Number, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::Integer, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::Bool, b::IntegerWithNaN) = _binary_fwd_boolret($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
end


import Base: div, fld, cld, rem, mod, mod1, fld1, max, min

for op in (:(div), :(fld), :(cld), :(rem), :(mod), :(mod1), :(fld1), :(max), :(min))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Real) = _binary_fwd($op, a, b)
        @inline $op(a::Real, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
end


function Base.minmax(a::T, b::T) where {T<:IntegerWithNaN}
    y = minmax(a.encoded, b.encoded)
    r1 = _nan_or_value(isnan(a) || isnan(b), y[1])
    r2 = _nan_or_value(isnan(a) || isnan(b), y[2])
    (r1, r2)
end


import Base: gcd, lcm

for op in (:(gcd), :(lcm))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Integer) = _binary_fwd($op, a, b)
        @inline $op(a::Integer, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
end


import Base: <<, >>, >>>

for op in (:(<<), :(>>), :(>>>))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Integer) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::Int) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::Bool) = _binary_fwd($op, a, b)
        @inline $op(a::Integer, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::Int, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::Bool, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
end


import Base: nextpow2, prevpow2, isqrt, abs


for op in (:(nextpow2), :(prevpow2), :(isqrt), :(abs))
    @eval begin
        @inline $op(x::IntegerWithNaN) = _unary_fwd($op, x)
    end
end


import Base: ispow2

for op in (:(ispow2),)
    @eval begin
        @inline $op(x::IntegerWithNaN) = _unary_fwd_boolret($op, x)
    end
end


const SignedIntWithNan = Union{IntegerWithNaN{Int8},IntegerWithNaN{Int16},IntegerWithNaN{Int32},IntegerWithNaN{Int64},IntegerWithNaN{Int128}}

@inline  Base.checked_abs(x::SignedIntWithNan) =
    _nan_or_value(isnan(x), Base.checked_abs(x.encoded))


#=

Currently not implemented for IntNaN:

* factorial(n::Integer)
* binomial(n::T, k::T) where T<:Integer
* nextpow(a::Real, x::Real)
* prevpow(a::Real, x::Real)
* ndigits(x::Integer)
* ndigits(x::Unsigned, b::Integer)
* bin(x::Unsigned, pad::Int, neg::Bool)
* oct(x::Unsigned, pad::Int, neg::Bool)
* dec(x::Unsigned, pad::Int, neg::Bool)
* hex(x::Unsigned, pad::Int, neg::Bool)
* base(b::Int, x::Unsigned, pad::Int, neg::Bool)
* digits([T<:Integer], n::Integer, base::T=10, pad::Integer=1)
* powermod(x::Integer, p::Integer, m)

=#


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

Base.@pure withnan(::Type{T}) where {T<:AbstractFloat} = T

# Base.@pure withnan(::Type{Bool}) = IntegerWithNaN{Int8}

@inline withnan(x::Number) = convert(withnan(typeof(x)), x)


function nanvalue end
export nanvalue

Base.@pure nanvalue(TN::Type{IntegerWithNaN{T}}) where {T} = TN()
Base.@pure nanvalue(TN::Type{T}) where {T<:AbstractFloat} = convert(T, NaN)

@inline nanvalue(x::Number) = nanvalue(typeof(x))


Base.@pure encode_nan(T::Type{<:Integer}) = typemax(T)


_result_type(a::T, b::U) where {T,U} = promote_type(T, U)


@inline _nan_or_value(TN::Type{IntegerWithNaN{T}}, cond::Bool, value) where T =
    TN(_Internal_, ifelse(cond, encode_nan(T), convert(T, value)))

@inline _nan_or_value(cond::Bool, value::PrimInt) =
    _nan_or_value(IntegerWithNaN{typeof(value)}, cond, value)

@inline _nan_or_value(cond::Bool, value::AbstractFloat) =
    ifelse(cond, convert(typeof(value), NaN), value)


export IntNaN, Int32NaN, Int64NaN, UInt32NaN, UInt64NaN

const IntNaN = IntegerWithNaN{Int}()
const Int32NaN = IntegerWithNaN{Int32}()
const Int64NaN = IntegerWithNaN{Int64}()
const UIntNaN = IntegerWithNaN{UInt}()
const UInt32NaN = IntegerWithNaN{Int32}()
const UInt64NaN = IntegerWithNaN{Int64}()
