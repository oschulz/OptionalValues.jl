# This file is a part of OptionalValues.jl, licensed under the MIT License (MIT).


struct IntegerWithNaN{T<:Integer} <: Integer
    encoded::T

    IntegerWithNaN{T}() where {T<:Integer} = new(encode_nan(T))

    IntegerWithNaN{T}(x) where {T<:Integer} = new(x)
end

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


Base.convert(TN::Type{IntegerWithNaN{T}}, x::Integer) where {T} = TN(x)

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
    TN(rem(x, T))

Base.rem(x::IntegerWithNaN, TN::Type{IntegerWithNaN{T}}) where {T} =
    ifelse(isnan(x), TN(), TN(rem(x.encoded, T)))



@inline function _unary_fwd(f::Function, x::IntegerWithNaN)
    y = withnan(f(x.encoded))
    ifelse(isnan(x), nanvalue(y), y)
end

@inline _unary_fwd_boolret(f::Function, x::IntegerWithNaN) =
    ifelse(isnan(x), false, f(x.encoded))


@inline function _binary_fwd(f::Function, a::IntegerWithNaN, b)
    y = withnan(f(a.encoded, b))
    ifelse(isnan(a), nanvalue(y), y)
end

@inline function _binary_fwd(f::Function, a, b::IntegerWithNaN)
    y = withnan(f(a, b.encoded))
    ifelse(isnan(b), nanvalue(y), y)
end

@inline function _binary_fwd(f::Function, a::IntegerWithNaN, b::IntegerWithNaN)
    y = withnan(f(a.encoded, b.encoded))
    ifelse(isnan(a) || isnan(b), nanvalue(y), y)
end


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


import Base: div, fld, cld, rem, mod, mod1, fld1, max, min, minmax

for op in (:(div), :(fld), :(cld), :(rem), :(mod), :(mod1), :(fld1), :(max), :(min), :(minmax))
    @eval begin
        @inline $op(a::IntegerWithNaN, b::Real) = _binary_fwd($op, a, b)
        @inline $op(a::Real, b::IntegerWithNaN) = _binary_fwd($op, a, b)
        @inline $op(a::IntegerWithNaN, b::IntegerWithNaN) = _binary_fwd($op, a, b)
    end
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

Base.checked_abs(x::SignedIntWithNan) =
    ifelse(isnan(x), nanvalue(typeof(x)), Base.checked_abs(x.encoded))


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

withnan(x::Number) = convert(withnan(typeof(x)), x)


function nanvalue end
export nanvalue

Base.@pure nanvalue(TN::Type{IntegerWithNaN{T}}) where {T} = TN()
Base.@pure nanvalue(TN::Type{T}) where {T<:AbstractFloat} = convert(T, NaN)

nanvalue(x::Number) = nanvalue(typeof(x))


Base.@pure encode_nan(T::Type{<:Integer}) = typemax(T)


_result_type(a::T, b::U) where {T,U} = promote_type(T, U)


export IntNaN, Int32NaN, Int64NaN, UInt32NaN, UInt64NaN

const IntNaN = IntegerWithNaN{Int}()
const Int32NaN = IntegerWithNaN{Int32}()
const Int64NaN = IntegerWithNaN{Int64}()
const UIntNaN = IntegerWithNaN{UInt}()
const UInt32NaN = IntegerWithNaN{Int32}()
const UInt64NaN = IntegerWithNaN{Int64}()
